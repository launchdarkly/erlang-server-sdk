%%-------------------------------------------------------------------
%% @doc Polling update requestor
%%
%% @end
%%-------------------------------------------------------------------

-module(eld_update_requestor_httpc).

-behaviour(eld_update_requestor).

%% Behavior callbacks
-export([init/0, all/3]).

%% Internal type for ETag cache state
-type state() :: #{string() => binary()}.

%%===================================================================
%% Behavior callbacks
%%===================================================================

%% Initializes ETag cache to empty map
-spec init() -> state().
init() -> #{}.

%% @doc Send events to LaunchDarkly event server
%%
%% @end
-spec all(Uri :: string(), SdkKey :: string(), State :: state()) ->
                 {eld_update_requestor:response(), state()}.
all(Uri, SdkKey, State) ->
    Headers = [
        {"Authorization", SdkKey},
        {"X-LaunchDarkly-Event-Schema", eld_settings:get_event_schema()},
        {"User-Agent", eld_settings:get_user_agent()}
    ],
    ETagHeaders = case maps:find(Uri, State) of
        error -> Headers;
        {ok, Etag} -> [{"If-None-Match", Etag}|Headers]
    end,
    {ok, {{Version, StatusCode, ReasonPhrase}, ResponseHeaders, Body}} =
        httpc:request(get, {Uri, ETagHeaders}, [], [{body_format, binary}]),
    if
        StatusCode =:= 304 -> {{ok, not_modified}, State};
        StatusCode < 300 ->
            case proplists:lookup("etag", [{string:casefold(K), V} || {K, V} <- ResponseHeaders]) of
                none -> {{ok, Body}, State};
                {_, ETag} ->  {{ok, Body}, State#{Uri => ETag}}
            end;
        StatusCode < 400 ->
            {{ok, Body}, State};
        true -> {{error, StatusCode, format_response(Version, StatusCode, ReasonPhrase)}, State}
    end.

%%===================================================================
%% Internal functions
%%===================================================================

-spec format_response(Version :: string(), StatusCode :: integer(), ReasonPhrase :: string()) ->
    string().
format_response(Version, StatusCode, ReasonPhrase) ->
    io_lib:format("~s ~b ~s", [Version, StatusCode, ReasonPhrase]).
