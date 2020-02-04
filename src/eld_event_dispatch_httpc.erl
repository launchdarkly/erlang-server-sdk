%%-------------------------------------------------------------------
%% @doc Event dispatcher
%%
%% @end
%%-------------------------------------------------------------------

-module(eld_event_dispatch_httpc).

-behaviour(eld_event_dispatch).

%% Behavior callbacks
-export([send/4]).

%%===================================================================
%% Behavior callbacks
%%===================================================================

%% @doc Send events to LaunchDarkly event server
%%
%% @end
-spec send(JsonEvents :: binary(), PayloadId :: uuid:uuid(), Uri :: string(), SdkKey :: string()) -> ok.
send(JsonEvents, PayloadId, Uri, SdkKey) ->
    Headers = [
        {"Authorization", SdkKey},
        {"X-LaunchDarkly-Event-Schema", eld_settings:get_event_schema()},
        {"User-Agent", eld_settings:get_user_agent()},
        {"X-LaunchDarkly-Payload-ID", PayloadId}
    ],
    {ok, {{Version, StatusCode, ReasonPhrase}, _Headers, _Body}} =
        httpc:request(post, {Uri, Headers, "application/json", JsonEvents}, [], []),
    Result = if
        StatusCode < 400 -> ok;
        true ->
            case is_http_error_code_recoverable(StatusCode) of
                true ->
                    {error, temporary, format_response(Version, StatusCode, ReasonPhrase)};
                _ ->
                    {error, permanent, format_response(Version, StatusCode, ReasonPhrase)}
            end
    end,
    Result.

%%===================================================================
%% Internal functions
%%===================================================================

-spec format_response(Version :: string(), StatusCode :: integer(), ReasonPhrase :: string()) ->
    string().
format_response(Version, StatusCode, ReasonPhrase) ->
    io_lib:format("~s ~b ~s", [Version, StatusCode, ReasonPhrase]).

-spec is_http_error_code_recoverable(StatusCode :: integer()) -> boolean().
is_http_error_code_recoverable(400) -> true;
is_http_error_code_recoverable(408) -> true;
is_http_error_code_recoverable(429) -> true;
is_http_error_code_recoverable(StatusCode) when StatusCode >= 500 -> true;
is_http_error_code_recoverable(_) -> false.
