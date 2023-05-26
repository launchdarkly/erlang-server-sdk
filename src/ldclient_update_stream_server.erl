%%-------------------------------------------------------------------
%% @doc Stream server
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_update_stream_server).

-behaviour(gen_server).

%% Supervision
-export([start_link/1, init/1]).

%% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-type state() :: #{
    conn := pid() | undefined,
    backoff := ldclient_backoff:backoff(),
    feature_store := atom(),
    storage_tag := atom(),
    stream_uri := string(),
    gun_options := gun:opts(),
    headers := map()
}.

-ifdef(TEST).
-compile(export_all).
-endif.

%% Maximum backoff delay of 30 seconds.
-define(MAX_BACKOFF_DELAY, 30000).

%%===================================================================
%% Supervision
%%===================================================================

%% @doc Starts the server
%%
%% @end
-spec start_link(Tag :: atom()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Tag) ->
    error_logger:info_msg("Starting streaming update server for ~p", [Tag]),
    gen_server:start_link(?MODULE, [Tag], []).

-spec init(Args :: term()) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init([Tag]) ->
    StreamUri = ldclient_config:get_value(Tag, stream_uri) ++ "/all",
    FeatureStore = ldclient_config:get_value(Tag, feature_store),
    HttpOptions = ldclient_config:get_value(Tag, http_options),
    InitialRetryDelay = ldclient_config:get_value(Tag, stream_initial_retry_delay_ms),
    Backoff = ldclient_backoff:init(InitialRetryDelay, ?MAX_BACKOFF_DELAY, self(), listen),
    GunOptions = ldclient_http_options:gun_parse_http_options(HttpOptions),
    Headers = ldclient_http_options:gun_append_custom_headers(
        ldclient_headers:get_default_headers(Tag, binary_map), HttpOptions),
    % Need to trap exit so supervisor:terminate_child calls terminate callback
    process_flag(trap_exit, true),
    State = #{
        conn => undefined,
        backoff => Backoff,
        feature_store => FeatureStore,
        storage_tag => Tag,
        stream_uri => StreamUri,
        gun_options => GunOptions,
        headers => Headers
    },
    self() ! {listen},
    {ok, State}.

%%===================================================================
%% Behavior callbacks
%%===================================================================

-type from() :: {pid(), term()}.
-spec handle_call(Request :: term(), From :: from(), State :: state()) ->
    {reply, Reply :: term(), NewState :: state()} |
    {stop, normal, {error, atom(), term()}, state()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({listen}, #{stream_uri := Uri} = State) ->
    error_logger:info_msg("Starting streaming connection to URL: ~p", [Uri]),
    NewState = do_listen(State),
    {noreply, NewState};
handle_info({'DOWN', _Mref, process, ShotgunPid, Reason}, #{conn := ShotgunPid, backoff := Backoff} = State) ->
    NewBackoff = ldclient_backoff:fail(Backoff),
    _ = ldclient_backoff:fire(NewBackoff),
    error_logger:warning_msg("Got DOWN message from shotgun pid with reason: ~p, will retry in ~p ms~n", [Reason, maps:get(current, NewBackoff)]),
    {noreply, State#{conn := undefined, backoff := NewBackoff}};
handle_info({timeout, _TimerRef, listen}, State) ->
    error_logger:info_msg("Reconnecting streaming connection...~n"),
    NewState = do_listen(State),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: state()) -> term().
terminate(Reason, #{conn := undefined} = _State) ->
    error_logger:info_msg("Terminating, reason: ~p; Pid none~n", [Reason]),
    ok;
terminate(Reason, #{conn := ShotgunPid} = _State) ->
    error_logger:info_msg("Terminating streaming connection, reason: ~p; Pid ~p~n", [Reason, ShotgunPid]),
    ok = shotgun:close(ShotgunPid).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%===================================================================
%% Internal functions
%%===================================================================

-spec do_listen(state()) -> state().
do_listen(#{
    feature_store := FeatureStore,
    storage_tag := Tag,
    stream_uri := Uri,
    backoff := Backoff,
    gun_options := GunOptions,
    headers := Headers
    } = State
) ->
    try do_listen(Uri, FeatureStore, Tag, GunOptions, Headers) of
        {error, temporary, Reason} ->
            NewBackoff = do_listen_fail_backoff(Backoff, temporary, Reason),
            State#{backoff := NewBackoff};
        {error, permanent, Reason} ->
            error_logger:error_msg("Stream encountered permanent error ~p, giving up~n", [Reason]),
            State;
        {ok, Pid} ->
            NewBackoff = ldclient_backoff:succeed(Backoff),
            State#{conn := Pid, backoff := NewBackoff}
        catch Code:Reason ->
            NewBackoff = do_listen_fail_backoff(Backoff, Code, Reason),
            State#{backoff := NewBackoff}
    end.

%% @doc Used for firing backoff before shotgun pid is monitored
%% @private
%%
%% @end
-spec do_listen_fail_backoff(ldclient_backoff:backoff(), atom(), term()) -> ldclient_backoff:backoff().
do_listen_fail_backoff(Backoff, Code, Reason) ->
    NewBackoff = ldclient_backoff:fail(Backoff),
    error_logger:warning_msg("Error establishing streaming connection (~p): ~p, will retry in ~p ms", [Code, Reason, maps:get(current, NewBackoff)]),
    _ = ldclient_backoff:fire(NewBackoff),
    NewBackoff.

%% @doc Connect to LaunchDarkly streaming endpoint
%% @private
%%
%% @end
-spec do_listen(string(), atom(), atom(), GunOpts :: gun:opts(), Headers :: [{string(), string()}]) -> {ok, pid()} | {error, atom(), term()}.
do_listen(Uri, FeatureStore, Tag, GunOpts, Headers) ->
    {ok, {Scheme, Host, Port, Path, Query}} = ldclient_http:uri_parse(Uri),
    Opts = #{gun_opts => GunOpts},
    case shotgun:open(Host, Port, Scheme, Opts) of
        {error, gun_open_failed} ->
            {error, gun_open_failed, "Could not open connection to host"};
        {error, gun_open_timeout} ->
            {error, gun_open_timeout, "Connection timeout"};
        {ok, Pid} ->
            _ = monitor(process, Pid),
            F = fun(nofin, _Ref, Bin) ->
                    try
                        process_event(parse_shotgun_event(Bin), FeatureStore, Tag)
                    catch Code:Reason ->
                        % Exception when processing event, log error, close connection
                        error_logger:warning_msg("Invalid SSE event error (~p): ~p", [Code, Reason]),
                        shotgun:close(Pid)
                    end;
                (fin, _Ref, _Bin) ->
                    % Connection ended, close monitored shotgun client pid, so we can reconnect
                    error_logger:warning_msg("Streaming connection ended"),
                    shotgun:close(Pid)
                end,
            Options = #{async => true, async_mode => sse, handle_event => F},
            case shotgun:get(Pid, Path ++ Query, Headers, Options) of
                {error, Reason} ->
                    shotgun:close(Pid),
                    {error, temporary, Reason};
                {ok, #{status_code := StatusCode}} when StatusCode >= 400 ->
                    {error, ldclient_http:is_http_error_code_recoverable(StatusCode), StatusCode};
                {ok, _Ref} ->
                    {ok, Pid}
            end
    end.

%% @doc Processes server-sent event received from shotgun
%% @private
%%
%% @end
-spec process_event(shotgun:event(), FeatureStore :: atom(), Tag :: atom()) -> ok.
process_event(#{event := Event, data := Data}, FeatureStore, Tag) ->
    StorageDown = ldclient_update_processor_state:get_storage_initialized_state(Tag),
    case StorageDown of
        false -> ldclient_update_processor_state:set_storage_initialized_state(Tag, reload);
        _ -> ok
    end,
    EventOperation = get_event_operation(Event),
    DecodedData = decode_data(EventOperation, Data),
    ProcessResult = process_items(EventOperation, DecodedData, FeatureStore, Tag),
    true = ldclient_update_processor_state:set_initialized_state(Tag, true),
    ProcessResult.

-spec get_event_operation(Event :: binary()) -> ldclient_storage_engine:event_operation() | other.
get_event_operation(<<"put">>) -> put;
get_event_operation(<<"delete">>) -> delete;
get_event_operation(<<"patch">>) -> patch;
get_event_operation(_) -> other.

-spec decode_data(ldclient_storage_engine:event_operation() | other, binary()) -> map() | binary().
decode_data(other, Data) -> Data;
decode_data(_, Data) -> jsx:decode(Data, [return_maps]).

%% @doc Process a list of put, patch or delete items
%% @private
%%
%% @end
-spec process_items(EventOperation :: ldclient_storage_engine:event_operation(), Data :: map(), FeatureStore :: atom(), Tag :: atom()) -> ok.
process_items(put, Data, ldclient_storage_redis, Tag) ->
    [Flags, Segments] = get_put_items(Data),
    error_logger:info_msg("Received stream event with ~p flags and ~p segments", [maps:size(Flags), maps:size(Segments)]),
    ok = ldclient_storage_redis:upsert_clean(Tag, features, Flags),
    ok = ldclient_storage_redis:upsert_clean(Tag, segments, Segments),
    ok = ldclient_storage_redis:set_init(Tag);
process_items(put, Data, FeatureStore, Tag) ->
    [Flags, Segments] = get_put_items(Data),
    error_logger:info_msg("Received event with ~p flags and ~p segments", [maps:size(Flags), maps:size(Segments)]),
    ParsedFlags = maps:map(
        fun(_K, V) -> ldclient_flag:new(V) end
        , Flags),
    ParsedSegments = maps:map(
        fun(_K, V) -> ldclient_segment:new(V) end
        , Segments),
    ok = FeatureStore:upsert_clean(Tag, features, ParsedFlags),
    ok = FeatureStore:upsert_clean(Tag, segments, ParsedSegments);
process_items(patch, Data, FeatureStore, Tag) ->
    case get_patch_item(Data) of
        {Bucket, Key, Item, ParseFunction} ->
            ok = maybe_patch_item(FeatureStore, Tag, Bucket, Key, Item, ParseFunction);
        error ->
            #{<<"path">> := Path} = Data,
            error_logger:warning_msg("Unrecognized patch path ~p", [Path]),
            ok
    end;
process_items(delete, Data, FeatureStore, Tag) ->
    delete_items(Data, FeatureStore, Tag);
process_items(other, _, _, _) ->
    ok.

-spec get_put_items(Data :: map()) -> [map()].
get_put_items(#{<<"data">> := #{<<"flags">> := Flags, <<"segments">> := Segments}}) ->
    [Flags, Segments].

-spec get_patch_item(Data :: map()) -> {Bucket :: flags|segments, Key :: binary(), #{Key :: binary() => map()}, ParseFunction :: fun()} | error.
get_patch_item(#{<<"path">> := <<"/flags/",FlagKey/binary>>, <<"data">> := FlagMap}) ->
    {features, FlagKey, #{FlagKey => FlagMap}, fun ldclient_flag:new/1};
get_patch_item(#{<<"path">> := <<"/segments/",SegmentKey/binary>>, <<"data">> := SegmentMap}) ->
    {segments, SegmentKey, #{SegmentKey => SegmentMap}, fun ldclient_segment:new/1};
get_patch_item(_Data) ->
    error.

-spec delete_items(map(), atom(), atom()) -> ok.
delete_items(#{<<"path">> := <<"/flags/",Key/binary>>, <<"version">> := Version}, FeatureStore, Tag) ->
    ok = maybe_delete_item(FeatureStore, Tag, features, Key, Version);
delete_items(#{<<"path">> := <<"/segments/",Key/binary>>, <<"version">> := Version}, FeatureStore, Tag) ->
    ok = maybe_delete_item(FeatureStore, Tag, segments, Key, Version);
delete_items(_Path, _FeatureStore, _Tag) ->
    error_logger:error_msg("Invalid delete path").

-spec maybe_patch_item(atom(), atom(), atom(), binary(), map(), fun()) -> ok.
maybe_patch_item(ldclient_storage_redis, Tag, Bucket, Key, Item, _ParseFunction) ->
    FlagMap = maps:get(Key, Item, #{}),
    NewVersion = maps:get(<<"version">>, FlagMap, 0),
    ok = case ldclient_storage_redis:get(Tag, Bucket, Key) of
        [] ->
            ldclient_storage_redis:upsert(Tag, Bucket, #{Key => FlagMap});
        [{Key, ExistingItem}] ->
            ExistingVersion = maps:get(version, ExistingItem, 0),
            Overwrite = (ExistingVersion == 0) or (NewVersion > ExistingVersion),
            if
                Overwrite -> ldclient_storage_redis:upsert(Tag, Bucket, #{Key => FlagMap});
                true -> ok
            end
    end;
maybe_patch_item(FeatureStore, Tag, Bucket, Key, Item, ParseFunction) ->
    FlagMap = maps:get(Key, Item, #{}),
    NewVersion = maps:get(<<"version">>, FlagMap, 0),
    ok = case FeatureStore:get(Tag, Bucket, Key) of
        [] ->
            FeatureStore:upsert(Tag, Bucket, #{Key => ParseFunction(FlagMap)});
        [{Key, ExistingItem}] ->
            ExistingVersion = maps:get(version, ExistingItem, 0),
            Overwrite = (ExistingVersion == 0) or (NewVersion > ExistingVersion),
            if
                Overwrite -> FeatureStore:upsert(Tag, Bucket, #{Key => ParseFunction(FlagMap)});
                true -> ok
            end
    end.

-spec maybe_delete_item(atom(), atom(), atom(), binary(), pos_integer()|undefined) -> ok.
maybe_delete_item(FeatureStore, Tag, Bucket, Key, NewVersion) ->
    case FeatureStore:get(Tag, Bucket, Key) of
        [] -> 
            NewDeletedFlag = ldclient_flag:new(#{<<"key">> => Key, <<"deleted">> => true, <<"version">> => NewVersion}),
            NewItem = #{Key => NewDeletedFlag},
            FeatureStore:upsert(Tag, Bucket, NewItem);
        [{Key, ExistingItem}] ->
            ExistingVersion = maps:get(version, ExistingItem, 0),
            Overwrite = (ExistingVersion == 0) or (NewVersion > ExistingVersion),
            if
                Overwrite ->
                    NewItem = #{Key => ExistingItem#{deleted => true, version => NewVersion}},
                    FeatureStore:upsert(Tag, Bucket, NewItem);
                true ->
                    ok
            end
    end.

%% @doc Fixed version of shotgun:parse_event/1
%% @private
%%
%% @end
-spec parse_shotgun_event(binary()) -> shotgun:event().
parse_shotgun_event(EventBin) ->
    shotgun:parse_event(EventBin).
