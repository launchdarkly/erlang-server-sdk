%%-------------------------------------------------------------------
%% @doc Stream server
%%
%% @end
%%-------------------------------------------------------------------

-module(eld_stream_server).

-behaviour(gen_server).

%% Supervision
-export([start_link/1, init/1]).

%% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% API
-export([listen/1]).

-type state() :: #{
    key => atom(),
    conn => pid() | undefined,
    backoff => backoff:backoff(),
    last_attempted => non_neg_integer(),
    sdk_key => string(),
    storage_backend => atom(),
    storage_tag => atom(),
    stream_uri => string()
}.

-ifdef(TEST).
-compile(export_all).
-endif.

%%===================================================================
%% API
%%===================================================================

%% @doc Start listening to streaming events
%%
%% @end
-spec listen(Pid :: pid()) ->
    ok
    | {error, gun_open_failed, term()}
    | {error, gun_open_timeout, term()}
    | {error, get_request_failed, term()}.
listen(Pid) ->
    gen_server:call(Pid, {listen}).

%%===================================================================
%% Supervision
%%===================================================================

%% @doc Starts the server
%%
%% @end
-spec start_link(Tag :: atom()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Tag) ->
    gen_server:start_link(?MODULE, [Tag], []).

-spec init(Args :: term()) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init([Tag]) ->
    SdkKey = eld_settings:get_value(Tag, sdk_key),
    StreamUri = eld_settings:get_value(Tag, stream_uri),
    StorageBackend = eld_settings:get_value(Tag, storage_backend),
    Backoff = backoff:type(backoff:init(100, 30000, self(), listen), jitter),
    % Need to trap exit so supervisor:terminate_child calls terminate callback
    process_flag(trap_exit, true),
    State = #{
        conn => undefined,
        backoff => Backoff,
        last_attempted => 0,
        sdk_key => SdkKey,
        storage_backend => StorageBackend,
        storage_tag => Tag,
        stream_uri => StreamUri
    },
    {ok, State}.

%%===================================================================
%% Behavior callbacks
%%===================================================================

-type from() :: {pid(), term()}.
-spec handle_call(Request :: term(), From :: from(), State :: state()) ->
    {reply, Reply :: term(), NewState :: state()} |
    {stop, normal, {error, atom(), term()}, state()}.
handle_call({listen}, _From, #{stream_uri := Uri} = State) ->
    error_logger:info_msg("Starting streaming connection to URL: ~p", [Uri]),
    NewState = do_listen(State),
    {reply, ok, NewState}.

handle_cast(_Request, State) ->
    {noreply, State}.

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
    error_logger:info_msg("Terminating, reason: ~p; Pid ~p~n", [Reason, ShotgunPid]),
    ok = shotgun:close(ShotgunPid).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%===================================================================
%% Internal functions
%%===================================================================

-spec do_listen(state()) -> state().
do_listen(#{
    sdk_key := SdkKey,
    storage_backend := StorageBackend,
    storage_tag := Tag,
    stream_uri := Uri,
    backoff := Backoff,
    last_attempted := LastAttempted} = State
) ->
    Now = erlang:system_time(milli_seconds),
    NewState = State#{last_attempted := Now},
    case do_listen(Uri, StorageBackend, Tag, SdkKey) of
        {error, get_request_failed, Reason} ->
            % Monitored shotgun process terminates, which we will detect, nothing else to do
            error_logger:warning_msg("Streaming connection request error: ~p", [Reason]),
            NewState;
        {error, Code, _Reason} ->
            error_logger:warning_msg("Could not establish streaming connection (~p), will retry in ~p ms", [Code, backoff:get(Backoff)]),
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
-spec do_listen(string(), atom(), atom(), string()) -> {ok, pid()} | {error, atom(), term()}.
do_listen(Uri, StorageBackend, Tag, SdkKey) ->
    {ok, {Scheme, _UserInfo, Host, Port, Path, Query}} = http_uri:parse(Uri),
    GunOpts = #{retry => 0},
    Opts = #{gun_opts => GunOpts},
    case shotgun:open(Host, Port, Scheme, Opts) of
        {error, gun_open_failed} ->
            {error, gun_open_failed, "Could not open connection to host"};
        {error, gun_open_timeout} ->
            {error, gun_open_timeout, "Connection timeout"};
        {ok, Pid} ->
            _ = monitor(process, Pid),
            F = fun(nofin, _Ref, Bin) ->
                    process_event(shotgun:parse_event(Bin), StorageBackend, Tag);
                (fin, _Ref, _Bin) ->
                    % Connection ended, close shotgun client, so we can reconnect
                    error_logger:warning_msg("Streaming connection ended"),
                    shotgun:close(Pid)
                end,
            Options = #{async => true, async_mode => sse, handle_event => F},
            Headers = #{
                "Authorization" => SdkKey,
                "User-Agent" => eld_settings:get_user_agent()
            },
            case shotgun:get(Pid, Path ++ Query, Headers, Options) of
                {error, Reason} ->
                    {error, get_request_failed, Reason};
                {ok, _Ref} ->
                    {ok, Pid}
            end
    end.

%% @doc Processes server-sent event received from shotgun
%% @private
%%
%% @end
-spec process_event(shotgun:event(), StorageBackend :: atom(), Tag :: atom()) -> ok.
process_event(#{event := Event, data := Data}, StorageBackend, Tag) ->
    % Return data as maps because it's a preferred storage method for flags and segments
    DecodedData = jsx:decode(Data, [return_maps]),
    EventOperation = get_event_operation(Event),
    ok = process_items(EventOperation, DecodedData, StorageBackend, Tag).

-spec get_event_operation(Event :: binary()) -> eld_storage_engine:event_operation().
get_event_operation(<<"put">>) -> put;
get_event_operation(<<"delete">>) -> delete;
get_event_operation(<<"patch">>) -> patch.

%% @doc Process a list of put, patch or delete items
%% @private
%%
%% @end
-spec process_items(EventOperation :: eld_storage_engine:event_operation(), Data :: map(), StorageBackend :: atom(), Tag :: atom()) -> ok.
process_items(put, Data, StorageBackend, Tag) ->
    [Flags, Segments] = get_put_items(Data),
    error_logger:info_msg("Received event with ~p flags and ~p segments", [maps:size(Flags), maps:size(Segments)]),
    ok = StorageBackend:put(Tag, flags, Flags),
    ok = StorageBackend:put(Tag, segments, Segments);
process_items(patch, Data, StorageBackend, Tag) ->
    {Bucket, Key, Item} = get_patch_item(Data),
    ok = maybe_patch_item(StorageBackend, Tag, Bucket, Key, Item);
process_items(delete, Data, StorageBackend, Tag) ->
    delete_items(Data, StorageBackend, Tag).

-spec get_put_items(Data :: map()) -> [map()].
get_put_items(#{<<"path">> := <<"/">>, <<"data">> := #{<<"flags">> := Flags, <<"segments">> := Segments}}) ->
    [Flags, Segments].

-spec get_patch_item(Data :: map()) -> {Bucket :: flags|segments, Key :: binary(), #{Key :: binary() => map()}}.
get_patch_item(#{<<"path">> := <<"/flags/",FlagKey/binary>>, <<"data">> := FlagMap}) ->
    {flags, FlagKey, #{FlagKey => FlagMap}};
get_patch_item(#{<<"path">> := <<"/segments/",SegmentKey/binary>>, <<"data">> := SegmentMap}) ->
    {segments, SegmentKey, #{SegmentKey => SegmentMap}}.

-spec delete_items(map(), atom(), atom()) -> ok.
delete_items(#{<<"path">> := <<"/">>, <<"data">> := #{<<"flags">> := Flags, <<"segments">> := Segments}}, StorageBackend, Tag) ->
    MapFun = fun(_K, V) -> maps:update(<<"deleted">>, true, V) end,
    UpdatedFlags = maps:map(MapFun, Flags),
    UpdatedSegments = maps:map(MapFun, Segments),
    ok = StorageBackend:put(Tag, flags, UpdatedFlags),
    ok = StorageBackend:put(Tag, segments, UpdatedSegments);
delete_items(#{<<"path">> := <<"/flags/",Key/binary>>, <<"version">> := Version}, StorageBackend, Tag) ->
    ok = maybe_delete_item(StorageBackend, Tag, flags, Key, Version);
delete_items(#{<<"path">> := <<"/flags/",Key/binary>>}, StorageBackend, Tag) ->
    ok = maybe_delete_item(StorageBackend, Tag, flags, Key, undefined);
delete_items(#{<<"path">> := <<"/segments/",Key/binary>>, <<"version">> := Version}, StorageBackend, Tag) ->
    ok = maybe_delete_item(StorageBackend, Tag, segments, Key, Version);
delete_items(#{<<"path">> := <<"/segments/",Key/binary>>}, StorageBackend, Tag) ->
    ok = maybe_delete_item(StorageBackend, Tag, segments, Key, undefined).


-spec maybe_patch_item(atom(), atom(), atom(), binary(), map()) -> ok.
maybe_patch_item(StorageBackend, Tag, Bucket, Key, Item) ->
    FlagMap = maps:get(Key, Item, #{}),
    NewVersion = maps:get(<<"version">>, FlagMap, 0),
    ok = case StorageBackend:get(Tag, Bucket, Key) of
        [] ->
            StorageBackend:put(Tag, Bucket, Item);
        [{Key, ExistingFlagMap}] ->
            ExistingVersion = maps:get(<<"version">>, ExistingFlagMap, 0),
            Overwrite = (ExistingVersion == 0) or (NewVersion > ExistingVersion),
            if
                Overwrite -> StorageBackend:put(Tag, Bucket, Item);
                true -> ok
            end
    end.

-spec maybe_delete_item(atom(), atom(), atom(), binary(), pos_integer()|undefined) -> ok.
maybe_delete_item(StorageBackend, Tag, Bucket, Key, NewVersion) ->
    case StorageBackend:get(Tag, Bucket, Key) of
        [] -> ok;
        [{Key, ExistingItem}] ->
            ExistingVersion = maps:get(<<"version">>, ExistingItem, 0),
            Overwrite = (ExistingVersion == 0) or (NewVersion > ExistingVersion),
            if
                Overwrite ->
                    NewItem = #{Key => maps:put(<<"deleted">>, true, ExistingItem)},
                    StorageBackend:put(Tag, Bucket, NewItem);
                true ->
                    ok
            end
    end.
