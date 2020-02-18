%%-------------------------------------------------------------------
%% @doc Polling update requestor
%%
%% @end
%%-------------------------------------------------------------------

-module(ldclient_update_requestor_httpc).

-behaviour(ldclient_update_requestor).

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
                 {ldclient_update_requestor:response(), state()}.
all(Uri, SdkKey, State) ->
    Headers = [
        {"Authorization", SdkKey},
        {"X-LaunchDarkly-Event-Schema", ldclient_settings:get_event_schema()},
        {"User-Agent", ldclient_settings:get_user_agent()}
    ],
    ETagHeaders = case maps:find(Uri, State) of
        error -> Headers;
        {ok, Etag} -> [{"If-None-Match", Etag}|Headers]
    end,
    HTTPOptions = [{connect_timeout, 2000}],
    Result = httpc:request(get, {Uri, ETagHeaders}, HTTPOptions, [{body_format, binary}]),
    case Result of
        {ok, {StatusLine = {_, StatusCode, _}, ResponseHeaders, Body}} ->
            if
                StatusCode =:= 304 -> {{ok, not_modified}, State};
                StatusCode < 300 ->
                    case proplists:lookup("etag", [{string:casefold(K), V} || {K, V} <- ResponseHeaders]) of
                        none -> {{ok, Body}, State};
                        {_, ETag} -> {{ok, Body}, State#{Uri => ETag}}
                    end;
                StatusCode < 400 -> {{ok, Body}, State};
                true -> {{error, bad_status_error(StatusLine)}, State}
            end;
        _ -> {{error, network_error}, State}
    end.

%%===================================================================
%% Internal functions
%%===================================================================

-spec bad_status_error(StatusLine :: httpc:status_line()) -> ldclient_update_requestor:errors().
bad_status_error({Version, StatusCode, ReasonPhrase}) ->
    {bad_status, StatusCode, io_lib:format("~s ~b ~s", [Version, StatusCode, ReasonPhrase])}.
