%%-------------------------------------------------------------------
%% @doc Polling update requestor
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_update_requestor_httpc).

-behaviour(ldclient_update_requestor).

%% Behavior callbacks
-export([init/2, all/2]).

%% Internal type for ETag cache state
-type state() :: #{
    etag_state => #{string() => binary()},
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
        %% Initializes ETag cache to empty map
        etag_state => #{},
        headers => Headers,
        http_options => HttpOptions
    }.

%% @doc Send events to LaunchDarkly event server
%%
%% @end
-spec all(Uri :: string(), State :: state()) ->
                 {ldclient_update_requestor:response(), state()}.
all(Uri, State) ->
    #{etag_state := EtagState, headers := Headers, http_options := HttpOptions} = State,
    ETagHeaders = case maps:find(Uri, EtagState) of
        error -> Headers;
        {ok, Etag} -> [{"If-None-Match", Etag}|Headers]
    end,
    Result = httpc:request(get, {Uri, ETagHeaders}, HttpOptions, [{body_format, binary}]),
    case Result of
        {ok, {StatusLine = {_, StatusCode, _}, ResponseHeaders, Body}} ->
            if
                StatusCode =:= 304 -> {{ok, not_modified}, State};
                StatusCode < 300 ->
                    case proplists:lookup("etag", [{string:casefold(K), V} || {K, V} <- ResponseHeaders]) of
                        none -> {{ok, Body}, State};
                        {_, ETag} -> {{ok, Body}, State#{etag_state => EtagState#{Uri => ETag}}}
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
