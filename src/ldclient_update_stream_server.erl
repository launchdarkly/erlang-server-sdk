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
    backoff := backoff:backoff(),
    last_attempted := non_neg_integer(),
    sdk_key := string(),
    feature_store := atom(),
    storage_tag := atom(),
    stream_uri := string(),
    ssl_options := [ssl:tls_option()]
}.

-ifdef(TEST).
-compile(export_all).
-endif.

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
    SdkKey = ldclient_config:get_value(Tag, sdk_key),
    StreamUri = ldclient_config:get_value(Tag, stream_uri) ++ "/all",
    FeatureStore = ldclient_config:get_value(Tag, feature_store),
    SslOptions = ldclient_config:get_value(Tag, ssl_options),
    Backoff = backoff:type(backoff:init(1000, 30000, self(), listen), jitter),
    % Need to trap exit so supervisor:terminate_child calls terminate callback
    process_flag(trap_exit, true),
    State = #{
        conn => undefined,
        backoff => Backoff,
        last_attempted => 0,
        sdk_key => SdkKey,
        feature_store => FeatureStore,
        storage_tag => Tag,
        stream_uri => StreamUri,
        ssl_options => SslOptions
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
    _ = backoff:fire(Backoff),
    {_, NewBackoff} = backoff:fail(Backoff),
    error_logger:warning_msg("Got DOWN message from shotgun pid with reason: ~p~n", [Reason]),
    {noreply, State#{conn := undefined, backoff := NewBackoff}};
handle_info({timeout, _TimerRef, listen}, State) ->
    error_logger:info_msg("Reconnecting streaming connection..."),
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
    sdk_key := SdkKey,
    feature_store := FeatureStore,
    storage_tag := Tag,
    stream_uri := Uri,
    backoff := Backoff,
    last_attempted := LastAttempted,
    ssl_options := SslOptions} = State
) ->
    Now = erlang:system_time(milli_seconds),
    NewState = State#{last_attempted := Now},
    case do_listen(Uri, FeatureStore, Tag, SdkKey, SslOptions) of
        {error, Code, Reason} ->
            % Error occurred during initial connection, shotgun pid is not in state yet, so we
            % handle reconnection manually.
            error_logger:warning_msg("Error establishing streaming connection (~p): ~p, will retry in ~p ms", [Code, Reason, backoff:get(Backoff)]),
            _ = backoff:fire(Backoff),
            {_, NewBackoff} = backoff:fail(Backoff),
            NewState#{backoff := NewBackoff};
        {ok, Pid} ->
            % If the last connection attempt was more than a minute ago (i.e. we stayed connected)
            % then reset backoff state.
            {_, NewBackoff} = if
                Now - LastAttempted > 60000 -> backoff:succeed(Backoff);
                true -> {backoff:get(Backoff), Backoff}
            end,
            NewState#{conn := Pid, backoff := NewBackoff}
    end.

%% @doc Connect to LaunchDarkly streaming endpoint
%% @private
%%
%% @end
-spec do_listen(string(), atom(), atom(), string(), [ssl:tls_option()]) -> {ok, pid()} | {error, atom(), term()}.
do_listen(Uri, FeatureStore, Tag, SdkKey, SslOptions) ->
    {ok, {Scheme, _UserInfo, Host, Port, Path, Query}} = http_uri:parse(Uri),
    GunOpts = #{retry => 0},
    Opts = maybe_with_ssl_options(#{gun_opts => GunOpts}, SslOptions),
    case shotgun:open(Host, Port, Scheme, Opts) of
        {error, gun_open_failed} ->
            {error, gun_open_failed, "Could not open connection to host"};
        {error, gun_open_timeout} ->
            {error, gun_open_timeout, "Connection timeout"};
        {ok, Pid} ->
            _ = monitor(process, Pid),
            F = fun(nofin, _Ref, Bin) ->
                    process_event(parse_shotgun_event(Bin), FeatureStore, Tag);
                (fin, _Ref, _Bin) ->
                    % Connection ended, close monitored shotgun client pid, so we can reconnect
                    error_logger:warning_msg("Streaming connection ended"),
                    shotgun:close(Pid)
                end,
            Options = #{async => true, async_mode => sse, handle_event => F},
            Headers = #{
                "Authorization" => SdkKey,
                "User-Agent" => ldclient_config:get_user_agent()
            },
            case shotgun:get(Pid, Path ++ Query, Headers, Options) of
                {error, Reason} ->
                    shotgun:close(Pid),
                    {error, get_request_failed, Reason};
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
    ok = ldclient_storage_redis:upsert_clean(Tag, segments, Segments);
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
    {Bucket, Key, Item, ParseFunction} = get_patch_item(Data),
    ok = maybe_patch_item(FeatureStore, Tag, Bucket, Key, Item, ParseFunction);
process_items(delete, Data, FeatureStore, Tag) ->
    delete_items(Data, FeatureStore, Tag);
process_items(other, _, _, _) ->
    ok.

-spec get_put_items(Data :: map()) -> [map()].
get_put_items(#{<<"data">> := #{<<"flags">> := Flags, <<"segments">> := Segments}}) ->
    [Flags, Segments].

-spec get_patch_item(Data :: map()) -> {Bucket :: flags|segments, Key :: binary(), #{Key :: binary() => map()}, ParseFunction :: fun()}.
get_patch_item(#{<<"path">> := <<"/flags/",FlagKey/binary>>, <<"data">> := FlagMap}) ->
    {features, FlagKey, #{FlagKey => FlagMap}, fun ldclient_flag:new/1};
get_patch_item(#{<<"path">> := <<"/segments/",SegmentKey/binary>>, <<"data">> := SegmentMap}) ->
    {segments, SegmentKey, #{SegmentKey => SegmentMap}, fun ldclient_segment:new/1}.

-spec delete_items(map(), atom(), atom()) -> ok.
delete_items(#{<<"path">> := <<"/">>, <<"data">> := #{<<"flags">> := Flags, <<"segments">> := Segments}}, ldclient_storage_redis, Tag) ->
    MapFun = fun(_K, V) -> maps:update(<<"deleted">>, true, V) end,
    UpdatedFlags = maps:map(MapFun, Flags),
    UpdatedSegments = maps:map(MapFun, Segments),
    ok = ldclient_storage_redis:upsert(Tag, features, UpdatedFlags),
    ok = ldclient_storage_redis:upsert(Tag, segments, UpdatedSegments);
delete_items(#{<<"path">> := <<"/">>, <<"data">> := #{<<"flags">> := Flags, <<"segments">> := Segments}}, FeatureStore, Tag) ->
    MapFun = fun(_K, V) -> maps:update(<<"deleted">>, true, V) end,
    UpdatedFlags = maps:map(MapFun, Flags),
    UpdatedSegments = maps:map(MapFun, Segments),
    ParsedFlags = maps:map(
        fun(_K, V) -> ldclient_flag:new(V) end
        , UpdatedFlags),
    ParsedSegments = maps:map(
        fun(_K, V) -> ldclient_segment:new(V) end
        , UpdatedSegments),
    ok = FeatureStore:upsert(Tag, features, ParsedFlags),
    ok = FeatureStore:upsert(Tag, segments, ParsedSegments);
delete_items(#{<<"path">> := <<"/flags/",Key/binary>>, <<"version">> := Version}, FeatureStore, Tag) ->
    ok = maybe_delete_item(FeatureStore, Tag, features, Key, Version);
delete_items(#{<<"path">> := <<"/flags/",Key/binary>>}, FeatureStore, Tag) ->
    ok = maybe_delete_item(FeatureStore, Tag, features, Key, undefined);
delete_items(#{<<"path">> := <<"/segments/",Key/binary>>, <<"version">> := Version}, FeatureStore, Tag) ->
    ok = maybe_delete_item(FeatureStore, Tag, segments, Key, Version);
delete_items(#{<<"path">> := <<"/segments/",Key/binary>>}, FeatureStore, Tag) ->
    ok = maybe_delete_item(FeatureStore, Tag, segments, Key, undefined).


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
                    NewItem = #{Key => maps:put(deleted, true, ExistingItem)},
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


%% @doc If ssl options have been provided, then add them to the list of transport options
%% to be sent to shotgun
%% @private
%%
%% @end
maybe_with_ssl_options(Opts, [_|_]=SslOptions) -> Opts#{transport_opts => SslOptions};
maybe_with_ssl_options(Opts, _) -> Opts.