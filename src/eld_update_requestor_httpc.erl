%%-------------------------------------------------------------------
%% @doc Polling update requestor
%%
%% @end
%%-------------------------------------------------------------------

-module(eld_update_requestor_httpc).

-behaviour(eld_update_requestor).

%% Behavior callbacks
-export([all/2]).

%%===================================================================
%% Behavior callbacks
%%===================================================================

%% @doc Send events to LaunchDarkly event server
%%
%% @end
-spec all(Uri :: string(), SdkKey :: string()) -> eld_update_requestor:response().
all(Uri, SdkKey) ->
    Headers = [
        {"Authorization", SdkKey},
        {"X-LaunchDarkly-Event-Schema", eld_settings:get_event_schema()},
        {"User-Agent", eld_settings:get_user_agent()}
    ],
    {ok, {{Version, StatusCode, ReasonPhrase}, _Headers, Body}} =
        httpc:request(get, {Uri, Headers}, [], [{body_format, binary}]),
    Result = if
        StatusCode < 400 -> {ok, Body};
        true -> {error, StatusCode, format_response(Version, StatusCode, ReasonPhrase)}
    end,
    Result.

%%===================================================================
%% Internal functions
%%===================================================================

-spec format_response(Version :: string(), StatusCode :: integer(), ReasonPhrase :: string()) ->
    string().
format_response(Version, StatusCode, ReasonPhrase) ->
    io_lib:format("~s ~b ~s", [Version, StatusCode, ReasonPhrase]).
