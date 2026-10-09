%%-------------------------------------------------------------------
%% @doc Event dispatcher
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_event_dispatch_httpc).

-behaviour(ldclient_event_dispatch).

%% Behavior callbacks
-export([init/2, send/4, stop/1]).

-type state() :: #{
    headers => list(),
    http_options => list(),
    profile => atom()
}.

%% Options of the default httpc profile that describe how to reach the network
%% and therefore also apply to the events profile.
-define(INHERITED_PROFILE_OPTIONS, [proxy, https_proxy, ipfamily, ip, port, socket_opts, unix_socket]).

%% Expose non-exported methods for tests.
-ifdef(TEST).
-compile(export_all).
-endif.

%%===================================================================
%% Behavior callbacks
%%===================================================================

-spec init(Tag :: atom(), SdkKey :: string()) -> state().
init(Tag, _SdkKey) ->
    Options = ldclient_config:get_value(Tag, http_options),
    %% A request that is never answered must not hold a reporter worker (and
    %% its batch) forever: bound it, and let the timeout be a temporary failure.
    RequestTimeout = ldclient_config:get_value(Tag, events_request_timeout),
    HttpOptions = [{timeout, RequestTimeout} | ldclient_http_options:httpc_parse_http_options(Options)],
    DefaultHeaders = ldclient_headers:get_default_headers(Tag, string_pairs),
    Headers = ldclient_http_options:httpc_append_custom_headers([
        {"X-LaunchDarkly-Event-Schema", ldclient_config:get_event_schema()}
        | DefaultHeaders
    ], Options),
    #{
        headers => Headers,
        http_options => HttpOptions,
        profile => ensure_profile(Tag)
    }.

%% @doc Stop the instance's httpc profile.
%% @end
-spec stop(Tag :: atom()) -> ok.
stop(Tag) ->
    _ = inets:stop(httpc, profile_name(Tag)),
    ok.

%% @doc Send events to LaunchDarkly event server
%%
%% @end
-spec send(State :: state(), JsonEvents :: binary(), PayloadId :: uuid:uuid(), Uri :: string()) ->
    ldclient_event_dispatch:send_result().
send(State, JsonEvents, PayloadId, Uri) ->
    #{headers := BaseHeaders, http_options := HttpOptions} = State,
    Headers = [
        {"X-LaunchDarkly-Payload-ID", uuid:uuid_to_string(PayloadId)} |
        BaseHeaders
    ],
    Profile = maps:get(profile, State, default),
    Request = httpc:request(post, {Uri, Headers, "application/json", JsonEvents}, HttpOptions, [], Profile),
    process_request(Request).

%%===================================================================
%% Internal functions
%%===================================================================

-spec profile_name(Tag :: atom()) -> atom().
profile_name(Tag) ->
    list_to_atom("ldclient_events_" ++ atom_to_list(Tag)).

%% The reporter pool needs one connection per in-flight request. On the default
%% httpc profile the manager queues up to `max_keep_alive_length' (5) requests
%% behind the one in flight on a keep-alive connection, and opens at most
%% `max_sessions' (2) of them, so several workers posting at once share one
%% connection and a batch handed to a fresh worker waits behind a hung request
%% for its whole timeout. Each instance therefore gets its own profile in which
%% a connection is reused only when it is idle and up to `events_flush_workers'
%% persistent connections may be open (one per reporter worker). Starting the profile is idempotent: every
%% worker calls `init/2'.
-spec ensure_profile(Tag :: atom()) -> atom().
ensure_profile(Tag) ->
    Profile = profile_name(Tag),
    {ok, _} = application:ensure_all_started(inets),
    case inets:start(httpc, [{profile, Profile}]) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end,
    MaxWorkers = ldclient_config:get_value(Tag, events_flush_workers),
    Inherited = inherited_options(),
    Options = lists:keydelete(socket_opts, 1, Inherited) ++ [
        {socket_opts, socket_options(proplists:get_value(socket_opts, Inherited, []))},
        {max_sessions, MaxWorkers},
        {max_keep_alive_length, 0}
    ],
    ok = httpc:set_options(Options, Profile),
    Profile.

%% Network-related options an application configured on the default profile
%% (a proxy, for example) must keep applying to event delivery.
-spec inherited_options() -> [{atom(), term()}].
inherited_options() ->
    {ok, Options} = httpc:get_options(all),
    [Opt || {Key, Value} = Opt <- Options, lists:member(Key, ?INHERITED_PROFILE_OPTIONS), is_set(Key, Value)].

%% Unset values as reported by `httpc:get_options/1' are not valid inputs to
%% `httpc:set_options/2'.
-spec is_set(atom(), term()) -> boolean().
is_set(proxy, {undefined, _}) -> false;
is_set(https_proxy, {undefined, _}) -> false;
is_set(ip, default) -> false;
is_set(port, default) -> false;
is_set(unix_socket, undefined) -> false;
is_set(socket_opts, []) -> false;
is_set(_, _) -> true.

%% A peer that accepts the connection (and the TLS handshake) but then stops
%% reading leaves the request body queued in the socket. The request timeout
%% frees the worker, but a socket closed with output still queued is kept by
%% the inet driver, holding the payload, until the peer closes its side;
%% against a stalled endpoint that is two sockets per flush for the length of
%% the outage. `send_timeout' does not help: a single large send is accepted
%% into the port queue at once, so the port never becomes busy and the timer
%% never arms. Zero linger makes a close discard whatever is still queued and
%% release the port immediately (an abortive close, i.e. a reset instead of a
%% FIN, also when an idle keep-alive connection is closed; by then its
%% responses have been read, so nothing is lost). Options an application set
%% on the default profile are kept, except an explicit linger of its own.
-spec socket_options(Inherited :: [{atom(), term()}]) -> [{atom(), term()}].
socket_options(Inherited) ->
    [Opt || {Key, _} = Opt <- Inherited, Key =/= linger] ++ [{linger, {true, 0}}].

-type http_request() :: {ok, {{string(), integer(), string()}, [{string(), string()}], string() | binary()}}.

-spec process_request({error, term()} | http_request()) -> ldclient_event_dispatch:send_result().
process_request({error, Reason}) ->
    {error, temporary, ldclient_key_redaction:format_httpc_error(Reason)};
process_request({ok, {{_Version, StatusCode, _ReasonPhrase}, Headers, _Body}}) when StatusCode < 400 ->
    {ok, get_server_time(Headers)};
process_request({ok, {{Version, StatusCode, ReasonPhrase}, _Headers, _Body}}) ->
    Reason = format_response(Version, StatusCode, ReasonPhrase),
    HttpErrorType = ldclient_http:is_http_error_code_recoverable(StatusCode),
    {error, HttpErrorType, Reason, StatusCode}.

-spec format_response(Version :: string(), StatusCode :: integer(), ReasonPhrase :: string()) ->
    string().
format_response(Version, StatusCode, ReasonPhrase) ->
    io_lib:format("~s ~b ~s", [Version, StatusCode, ReasonPhrase]).

%% Get the server time, and if there is not time, then return 0.
-spec get_server_time(Headers :: [{Field :: [byte()], Value :: binary() | iolist()}]) -> integer().
get_server_time([{"date", Date}|_T]) when is_list(Date) ->
    %% convert_request_date expects a string that is a list of characters.
    %% Not a binary string. The guard can make sure it is a list, but not
    %% that it is a char list. So that gets checked here.
    %% A malformed header must not crash the worker after a successful send
    %% (httpd_util/calendar raise on some inputs), so any failure yields 0.
    try
        case io_lib:char_list(Date) of
            true -> case httpd_util:convert_request_date(Date) of
                        bad_date ->
                            %% This would be a date in a bad format.
                            0;
                        ParsedDate ->
                            ldclient_time:datetime_to_timestamp(ParsedDate)
                    end;
            false ->
                %% The date was a list, but was not a list of characters.
                0
        end
    catch
        _:_ -> 0
    end;
get_server_time([_H|T]) ->
    get_server_time(T);
get_server_time(_) -> 0.
