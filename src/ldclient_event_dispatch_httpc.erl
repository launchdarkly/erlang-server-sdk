%%-------------------------------------------------------------------
%% @doc Event dispatcher
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_event_dispatch_httpc).

-behaviour(ldclient_event_dispatch).

%% Behavior callbacks
-export([init/2, send/4]).

%% Internal type for ETag cache state
-type state() :: #{
headers => list(),
http_options => list()
}.

%%===================================================================
%% Behavior callbacks
%%===================================================================

-spec init(Tag :: atom(), SdkKey :: string()) -> state().
init(Tag, SdkKey) ->
    Options = ldclient_config:get_value(Tag, http_options),
    HttpOptions = ldclient_http_options:httpc_parse_http_options(Options),
    Headers = ldclient_http_options:httpc_append_custom_headers([
        {"Authorization", SdkKey},
        {"X-LaunchDarkly-Event-Schema", ldclient_config:get_event_schema()},
        {"User-Agent", ldclient_config:get_user_agent()}
    ], Options),
    #{
        headers => Headers,
        http_options => HttpOptions
    }.

%% @doc Send events to LaunchDarkly event server
%%
%% @end
-spec send(State :: state(), JsonEvents :: binary(), PayloadId :: uuid:uuid(), Uri :: string()) ->
    ok | {error, temporary, string()} | {error, permanent, string()}.
send(State, JsonEvents, PayloadId, Uri) ->
    #{headers := BaseHeaders, http_options := HttpOptions} = State,
    Headers = [
        {"X-LaunchDarkly-Payload-ID", uuid:uuid_to_string(PayloadId)} |
        BaseHeaders
    ],
    Request = httpc:request(post, {Uri, Headers, "application/json", JsonEvents}, HttpOptions, []),
    process_request(Request).

%%===================================================================
%% Internal functions
%%===================================================================

-type http_request() :: {ok, {{string(), integer(), string()}, [{string(), string()}], string() | binary()}}.

-spec process_request({error, term()} | http_request()) 
    -> ok | {error, temporary, string()} | {error, permanent, string()}.
process_request({error, Reason}) ->
    {error, temporary, Reason};
process_request({ok, {{_Version, StatusCode, _ReasonPhrase}, _Headers, _Body}}) when StatusCode < 400 ->
    ok;
process_request({ok, {{Version, StatusCode, ReasonPhrase}, _Headers, _Body}}) ->
    Reason = format_response(Version, StatusCode, ReasonPhrase),
    HttpErrorType = is_http_error_code_recoverable(StatusCode),
    {error, HttpErrorType, Reason}.

-spec format_response(Version :: string(), StatusCode :: integer(), ReasonPhrase :: string()) ->
    string().
format_response(Version, StatusCode, ReasonPhrase) ->
    io_lib:format("~s ~b ~s", [Version, StatusCode, ReasonPhrase]).

-spec is_http_error_code_recoverable(StatusCode :: integer()) -> temporary | permanent.
is_http_error_code_recoverable(400) -> temporary;
is_http_error_code_recoverable(408) -> temporary;
is_http_error_code_recoverable(429) -> temporary;
is_http_error_code_recoverable(StatusCode) when StatusCode >= 500 -> temporary;
is_http_error_code_recoverable(_) -> permanent.
