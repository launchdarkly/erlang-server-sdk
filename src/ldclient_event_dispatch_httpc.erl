%%-------------------------------------------------------------------
%% @doc Event dispatcher
%%
%% @end
%%-------------------------------------------------------------------

-module(ldclient_event_dispatch_httpc).

-behaviour(ldclient_event_dispatch).

%% Behavior callbacks
-export([send/4]).

%%===================================================================
%% Behavior callbacks
%%===================================================================

%% @doc Send events to LaunchDarkly event server
%%
%% @end
-spec send(JsonEvents :: binary(), PayloadId :: uuid:uuid(), Uri :: string(), SdkKey :: string()) -> 
    ok | {error, temporary, string()} | {error, permanent, string()}.
send(JsonEvents, PayloadId, Uri, SdkKey) ->
    Headers = [
        {"Authorization", SdkKey},
        {"X-LaunchDarkly-Event-Schema", ldclient_settings:get_event_schema()},
        {"User-Agent", ldclient_settings:get_user_agent()},
        {"X-LaunchDarkly-Payload-ID", uuid:uuid_to_string(PayloadId)}
    ],
    Request = httpc:request(post, {Uri, Headers, "application/json", JsonEvents}, [], []),
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
