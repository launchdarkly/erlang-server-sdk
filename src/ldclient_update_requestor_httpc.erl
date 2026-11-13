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
init(Tag, _SdkKey) ->
    Options = ldclient_config:get_value(Tag, http_options),
    HttpOptions = ldclient_http_options:httpc_parse_http_options(Options),
    DefaultHeaders = ldclient_headers:get_default_headers(Tag, string_pairs),
    Headers = ldclient_http_options:httpc_append_custom_headers([
        {"X-LaunchDarkly-Event-Schema", ldclient_config:get_event_schema()},
        {"Accept-Encoding", "gzip"}
        | DefaultHeaders
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
                    FoldedHeaders = [{string:casefold(K), V} || {K, V} <- ResponseHeaders],
                    case decode_body(FoldedHeaders, Body) of
                        {error, _} = Error -> {Error, State};
                        {ok, DecodedBody} ->
                            case proplists:lookup("etag", FoldedHeaders) of
                                none -> {{ok, DecodedBody}, State};
                                {_, ETag} -> {{ok, DecodedBody}, State#{etag_state => EtagState#{Uri => ETag}}}
                            end
                    end;
                StatusCode < 400 -> {{ok, Body}, State};
                true -> {{error, bad_status_error(StatusLine)}, State}
            end;
        _ -> {{error, network_error}, State}
    end.

%%===================================================================
%% Internal functions
%%===================================================================

%% @doc Decompress the body when the server sends it with gzip encoding.
%% The headers must have casefolded names.
%% A body that is not valid gzip gives a decompress error.
%% @end
-spec decode_body(FoldedHeaders :: [{string(), string()}], Body :: binary()) ->
    {ok, binary()} | {error, ldclient_update_requestor:errors()}.
decode_body(FoldedHeaders, Body) ->
    case proplists:lookup("content-encoding", FoldedHeaders) of
        {_, Encoding} ->
            case string:casefold(Encoding) of
                "gzip" -> gunzip(Body);
                _ -> {ok, Body}
            end;
        none -> {ok, Body}
    end.

-spec gunzip(Body :: binary()) -> {ok, binary()} | {error, ldclient_update_requestor:errors()}.
gunzip(Body) ->
    try zlib:gunzip(Body) of
        Decoded -> {ok, Decoded}
    catch
        error:Reason -> {error, {decompress_error, Reason}}
    end.

-spec bad_status_error(StatusLine :: {
    uri_string:uri_string(),
    non_neg_integer(),
    string()
}) -> ldclient_update_requestor:errors().
bad_status_error({Version, StatusCode, ReasonPhrase}) ->
    {bad_status, StatusCode, io_lib:format("~s ~b ~s", [Version, StatusCode, ReasonPhrase])}.
