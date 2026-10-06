-module(ldclient_event_dispatch_httpc_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    authorization_header_set_on_request/1,
    custom_headers_appended/1,
    tls_request/1,
    handles_correct_rfc1123_dates/1,
    handles_incorrect_rfc1123_dates/1,
    handles_incorrect_date_types/1,
    handles_no_date_present/1,
    handle_date_in_headers/1,
    request_timeout_is_a_temporary_failure/1,
    requests_from_several_workers_run_in_parallel/1,
    stop_releases_the_instance_profile/1
]).

all() ->
    [
        authorization_header_set_on_request,
        custom_headers_appended,
        tls_request,
        handles_correct_rfc1123_dates,
        handles_incorrect_rfc1123_dates,
        handles_incorrect_date_types,
        handles_no_date_present,
        handle_date_in_headers,
        request_timeout_is_a_temporary_failure,
        requests_from_several_workers_run_in_parallel,
        stop_releases_the_instance_profile
    ].

init_per_suite(Config) ->
    %% Config registration needs the application's instance registry; make the
    %% suite independent of whichever suite ran before it.
    {ok, _} = application:ensure_all_started(ldclient),
    Config.

end_per_suite(_) ->
    ok = application:stop(ldclient).

init_per_testcase(_, Config) ->
    {ok, _} = bookish_spork:start_server(),
    Settings = ldclient_config:parse_options("sdk-key", #{}),
    ok = ldclient_config:register(default, Settings),
    CustomSettings = ldclient_config:parse_options("sdk-key", #{
        http_options => #{
            custom_headers => [
                {"Basic-String-Header", "String"},
                {"Binary-String-Header", "Binary"}
            ]}
    }),
    ok = ldclient_config:register(custom, CustomSettings),

    TlsSettings = ldclient_config:parse_options("sdk-key", #{
        http_options => #{
            tls_options => ldclient_config:tls_basic_options()
        }
    }),
    ok = ldclient_config:register(tls, TlsSettings),
    SlowSettings = ldclient_config:parse_options("sdk-key", #{events_request_timeout => 200}),
    ok = ldclient_config:register(slow_endpoint, SlowSettings),
    Config.

end_per_testcase(_, _Config) ->
    bookish_spork:stop_server().

%%====================================================================
%% Helpers
%%====================================================================

-define(MOCK_URI, "http://localhost:32002").

%%====================================================================
%% Tests
%%====================================================================

%% An endpoint that accepts the request and never answers must not hold the
%% worker: the configured request timeout turns it into a temporary failure.
request_timeout_is_a_temporary_failure(_) ->
    State = ldclient_event_dispatch_httpc:init(slow_endpoint, "sdk-key"),
    bookish_spork:stub_request(fun(_Request) ->
        timer:sleep(1500),
        [200, #{}, <<>>]
    end),
    T0 = erlang:monotonic_time(millisecond),
    {error, temporary, _Reason} = ldclient_event_dispatch_httpc:send(State, <<"[]">>, uuid:get_v4(), ?MOCK_URI ++ "/bulk"),
    Elapsed = erlang:monotonic_time(millisecond) - T0,
    true = Elapsed < 1200.

authorization_header_set_on_request(_) ->
    PayloadId = uuid:get_v4(),
    State = ldclient_event_dispatch_httpc:init(default, "sdk-key"),
    bookish_spork:stub_request([200, #{}, <<>>]),
    {ok, _} = ldclient_event_dispatch_httpc:send(State, <<"">>, PayloadId, ?MOCK_URI),
    {ok, Request} = bookish_spork:capture_request(),
    "sdk-key" = bookish_spork_request:header(Request, "authorization").

custom_headers_appended(_) ->
    PayloadId = uuid:get_v4(),
    State = ldclient_event_dispatch_httpc:init(custom, "sdk-key"),
    bookish_spork:stub_request([200, #{}, <<>>]),
    {ok, _} = ldclient_event_dispatch_httpc:send(State, <<"">>, PayloadId, ?MOCK_URI),
    {ok, Request} = bookish_spork:capture_request(),
    %% Includes non-custom headers.
    "sdk-key" = bookish_spork_request:header(Request, "authorization"),
    %% The custom headers are there as well.
    "String" = bookish_spork_request:header(Request, "basic-string-header"),
    "Binary" = bookish_spork_request:header(Request, "binary-string-header").

tls_request(_) ->
    PayloadId = uuid:get_v4(),
    State = ldclient_event_dispatch_httpc:init(tls, "sdk-key"),
    bookish_spork:stub_request([200, #{}, <<>>]),
    {ok, _} = ldclient_event_dispatch_httpc:send(State, <<"">>, PayloadId, ?MOCK_URI),
    {ok, _} = bookish_spork:capture_request().

handle_date_in_headers(_) ->
    %% This doesn't use bookish_spork, because it adds a date header in the incorrect format and overriding
    %% it with a good date doesn't work. Instead we just mock httpc here.
    PayloadId = uuid:get_v4(),
    State = ldclient_event_dispatch_httpc:init(tls, "sdk-key"),
    meck:new(httpc, [unstick]),
    meck:expect(httpc, request, fun(_, _, _, _, _) -> {ok, {{0, 200, ""}, [{"date", "Mon, 07 Nov 2022 18:43:12 GMT"}], ""}} end),
    {ok, 1667846592000} = ldclient_event_dispatch_httpc:send(State, <<"">>, PayloadId, "mock-doesn't-care").

handles_correct_rfc1123_dates(_) ->
    1667846592000 = ldclient_event_dispatch_httpc:get_server_time([{"date", "Mon, 07 Nov 2022 18:43:12 GMT"}]).

handles_incorrect_rfc1123_dates(_) ->
    %% Day needs to be 2 digits. Bookish spork doesn't do this right.
    0 = ldclient_event_dispatch_httpc:get_server_time([{"date", "Mon, 7 Nov 2022 18:43:12 GMT"}]),
    0 = ldclient_event_dispatch_httpc:get_server_time([{"date", "potato"}]).

handles_incorrect_date_types(_) ->
    0 = ldclient_event_dispatch_httpc:get_server_time([{"date", <<"Mon, 7 Nov 2022 18:43:12 GMT">>}]),
    0 = ldclient_event_dispatch_httpc:get_server_time([1667846592000]),
    0 = ldclient_event_dispatch_httpc:get_server_time([{"date", [[]]}]).

handles_no_date_present(_) ->
    0 = ldclient_event_dispatch_httpc:get_server_time([{"whatever", "value"}]),
    0 = ldclient_event_dispatch_httpc:get_server_time([]).

%% On the default httpc profile the manager queues up to five requests behind
%% the one in flight on a keep-alive connection, so several reporter workers
%% posting at once were served one after another. The instance profile reuses
%% a connection only when it is idle.
requests_from_several_workers_run_in_parallel(_) ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
    {ok, Port} = inet:port(Listen),
    Concurrency = atomics:new(2, []),
    Acceptor = spawn_link(fun() -> slow_accept_loop(Listen, Concurrency) end),
    State = ldclient_event_dispatch_httpc:init(default, "sdk-key"),
    Uri = "http://localhost:" ++ integer_to_list(Port) ++ "/bulk",
    Self = self(),
    T0 = erlang:monotonic_time(millisecond),
    Senders = [spawn_link(fun() ->
        Self ! {done, self(), ldclient_event_dispatch_httpc:send(State, <<"[]">>, uuid:get_v4(), Uri)}
    end) || _ <- lists:seq(1, 4)],
    lists:foreach(fun(Pid) ->
        receive {done, Pid, {ok, _}} -> ok after 5000 -> ct:fail("send did not complete") end
    end, Senders),
    Elapsed = erlang:monotonic_time(millisecond) - T0,
    MaxConcurrent = atomics:get(Concurrency, 2),
    ct:pal("4 requests against a 400 ms endpoint took ~b ms, max concurrent ~b", [Elapsed, MaxConcurrent]),
    true = MaxConcurrent >= 2,
    %% Serial delivery would take at least 1 600 ms.
    true = Elapsed < 1200,
    unlink(Acceptor),
    exit(Acceptor, kill),
    gen_tcp:close(Listen).

stop_releases_the_instance_profile(_) ->
    #{profile := Profile} = ldclient_event_dispatch_httpc:init(default, "sdk-key"),
    ldclient_events_default = Profile,
    Manager = httpc:profile_name(Profile),
    true = is_pid(whereis(Manager)),
    ok = ldclient_event_dispatch_httpc:stop(default),
    undefined = whereis(Manager),
    %% Stopping twice is harmless, and the next init starts it again.
    ok = ldclient_event_dispatch_httpc:stop(default),
    #{profile := Profile} = ldclient_event_dispatch_httpc:init(default, "sdk-key"),
    true = is_pid(whereis(Manager)).

slow_accept_loop(Listen, Concurrency) ->
    case gen_tcp:accept(Listen) of
        {ok, Socket} ->
            spawn(fun() -> handle_slowly(Socket, Concurrency) end),
            slow_accept_loop(Listen, Concurrency);
        {error, _} ->
            ok
    end.

handle_slowly(Socket, Concurrency) ->
    bump_max(Concurrency, atomics:add_get(Concurrency, 1, 1)),
    _ = gen_tcp:recv(Socket, 0, 2000),
    timer:sleep(400),
    ok = gen_tcp:send(Socket, <<"HTTP/1.1 202 Accepted\r\nContent-Length: 0\r\nConnection: close\r\n\r\n">>),
    atomics:sub(Concurrency, 1, 1),
    gen_tcp:close(Socket).

bump_max(Ref, Current) ->
    Max = atomics:get(Ref, 2),
    case Current > Max of
        true ->
            case atomics:compare_exchange(Ref, 2, Max, Current) of
                ok -> ok;
                _ -> bump_max(Ref, Current)
            end;
        false ->
            ok
    end.
