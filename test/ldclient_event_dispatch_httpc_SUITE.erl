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
    handle_date_in_headers/1
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
        handle_date_in_headers
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_) ->
    ok.

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

authorization_header_set_on_request(_) ->
    PayloadId = uuid:get_v4(),
    State = ldclient_event_dispatch_httpc:init(default, "sdk-key"),
    bookish_spork:stub_request([200, #{}, <<>>]),
    ok = ldclient_event_dispatch_httpc:send(State, <<"">>, PayloadId, ?MOCK_URI),
    {ok, Request} = bookish_spork:capture_request(),
    "sdk-key" = bookish_spork_request:header(Request, "authorization").

custom_headers_appended(_) ->
    PayloadId = uuid:get_v4(),
    State = ldclient_event_dispatch_httpc:init(custom, "sdk-key"),
    bookish_spork:stub_request([200, #{}, <<>>]),
    ok = ldclient_event_dispatch_httpc:send(State, <<"">>, PayloadId, ?MOCK_URI),
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
    ok = ldclient_event_dispatch_httpc:send(State, <<"">>, PayloadId, ?MOCK_URI),
    {ok, _} = bookish_spork:capture_request().

handle_date_in_headers(_) ->
    %% This doesn't use bookish_spork, because it adds a date header in the incorrect format and overriding
    %% it with a good date doesn't work. Instead we just mock httpc here.
    PayloadId = uuid:get_v4(),
    State = ldclient_event_dispatch_httpc:init(tls, "sdk-key"),
    meck:new(httpc, [unstick]),
    meck:expect(httpc, request, fun(_, _, _, _) -> {ok, {{0, 200, ""}, [{"date", "Mon, 07 Nov 2022 18:43:12 GMT"}], ""}} end),
    {ok, 1667846592000} = ldclient_event_dispatch_httpc:send(State, <<"">>, PayloadId, "mock-doesn't-care").

handles_correct_rfc1123_dates(_) ->
    1667846592000 = ldclient_event_dispatch_httpc:get_server_time([{"date", "Mon, 07 Nov 2022 18:43:12 GMT"}]).

handles_incorrect_rfc1123_dates(_) ->
    %% Day needs to be 2 digits. Bookish spork doesn't do this right.
    undefined = ldclient_event_dispatch_httpc:get_server_time([{"date", "Mon, 7 Nov 2022 18:43:12 GMT"}]),
    undefined = ldclient_event_dispatch_httpc:get_server_time([{"date", "potato"}]).

handles_incorrect_date_types(_) ->
    undefined = ldclient_event_dispatch_httpc:get_server_time([{"date", <<"Mon, 7 Nov 2022 18:43:12 GMT">>}]),
    undefined = ldclient_event_dispatch_httpc:get_server_time([1667846592000]),
    undefined = ldclient_event_dispatch_httpc:get_server_time([{"date", [[]]}]).

handles_no_date_present(_) ->
    undefined = ldclient_event_dispatch_httpc:get_server_time([{"whatever", "value"}]),
    undefined = ldclient_event_dispatch_httpc:get_server_time([]).
