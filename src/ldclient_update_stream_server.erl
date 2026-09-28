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
    %% Try to use a proper type with Gun 2.0
    gun_options := any(),
    headers := map(),
    read_timeout_ms := non_neg_integer(),
    read_timer := reference() | undefined,
    sse_buffer := binary()
}.

-ifdef(TEST).
-compile(export_all).
-endif.

%% Maximum backoff delay of 30 seconds.
-define(MAX_BACKOFF_DELAY, 30000).

%% How long gun may wait for in-flight streams to finish when the streaming
%% connection is closed (gun's closing_timeout, which defaults to 15 seconds).
%% The SSE stream never finishes on its own, so without a small bound here the
%% TCP socket would stay open for the full default timeout after the client is
%% closed.
-define(STREAM_CLOSING_TIMEOUT_MS, 100).

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
    ReadTimeoutMs = ldclient_config:get_value(Tag, stream_read_timeout_ms),
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
        headers => Headers,
        read_timeout_ms => ReadTimeoutMs,
        read_timer => undefined,
        sse_buffer => <<>>
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
handle_info({'DOWN', _Mref, process, ShotgunPid, Reason}, #{conn := ShotgunPid, backoff := Backoff, read_timer := ReadTimer} = State) ->
    NewBackoff = ldclient_backoff:fail(Backoff),
    _ = ldclient_backoff:fire(NewBackoff),
    % Reason from DOWN message could contain connection details with headers/SDK keys
    SafeReason = ldclient_key_redaction:format_shotgun_error(Reason),
    error_logger:warning_msg("Got DOWN message from shotgun pid with reason: ~s, will retry in ~p ms~n", [SafeReason, maps:get(current, NewBackoff)]),
    {noreply, State#{conn := undefined, backoff := NewBackoff, read_timer := cancel_read_timer(ReadTimer), sse_buffer := <<>>}};
handle_info({stream_chunk, ShotgunPid, IsFin, Bin}, #{conn := ShotgunPid} = State) ->
    NewState = handle_stream_chunk(Bin, State),
    case IsFin of
        fin ->
            % Connection ended, close monitored shotgun client pid, so we can reconnect
            error_logger:warning_msg("Streaming connection ended"),
            close_conn(ShotgunPid);
        nofin ->
            ok
    end,
    {noreply, NewState};
handle_info({stream_chunk, _StalePid, _IsFin, _Bin}, State) ->
    {noreply, State};
handle_info({timeout, TimerRef, read_timeout}, #{read_timer := TimerRef, conn := ShotgunPid, read_timeout_ms := ReadTimeoutMs} = State) ->
    % Half-open connection: closing it produces the DOWN message that reconnects with backoff
    error_logger:warning_msg("No data received on streaming connection for ~p ms, reconnecting~n", [ReadTimeoutMs]),
    close_conn(ShotgunPid),
    {noreply, State#{read_timer := undefined}};
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
    stream_uri := Uri,
    backoff := Backoff,
    gun_options := GunOptions,
    headers := Headers,
    read_timer := ReadTimer,
    read_timeout_ms := ReadTimeoutMs
    } = State
) ->
    try do_listen(Uri, GunOptions, Headers) of
        {error, temporary, Reason} ->
            NewBackoff = do_listen_fail_backoff(Backoff, temporary, Reason),
            State#{backoff := NewBackoff};
        {error, permanent, Reason} ->
            % Reason here is already safe: either a sanitized string from format_shotgun_error
            % or an integer status code from the do_listen/5 method.
            error_logger:error_msg("Stream encountered permanent error ~p, giving up~n", [Reason]),
            State;
        {ok, Pid} ->
            NewBackoff = ldclient_backoff:succeed(Backoff),
            State#{
                conn := Pid,
                backoff := NewBackoff,
                read_timer := restart_read_timer(ReadTimer, ReadTimeoutMs),
                sse_buffer := <<>>
            }
        catch Code:_Reason ->
            % Don't pass raw exception reason as it could contain unsafe data
            NewBackoff = do_listen_fail_backoff(Backoff, Code, "unexpected exception"),
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
-spec do_listen(string(), GunOpts :: any(), Headers :: [{string(), string()}]) -> {ok, pid()} | {error, atom(), term()}.
do_listen(Uri, GunOpts, Headers) ->
    {ok, {Scheme, Host, Port, Path, Query}} = ldclient_http:uri_parse(Uri),
    HttpOpts = maps:get(http_opts, GunOpts, #{}),
    StreamGunOpts = GunOpts#{http_opts => HttpOpts#{closing_timeout => ?STREAM_CLOSING_TIMEOUT_MS}},
    Opts = #{gun_opts => StreamGunOpts},
    case shotgun:open(Host, Port, Scheme, Opts) of
        {error, gun_open_failed} ->
            {error, temporary, "Could not open connection to host"};
        {error, gun_open_timeout} ->
            {error, temporary, "Connection timeout"};
        {ok, Pid} ->
            _ = monitor(process, Pid),
            % Binary mode: heartbeats are bare comment lines that never complete an SSE event,
            % so sse mode would hide them from the read timeout. Framing is in handle_stream_chunk/2.
            Server = self(),
            F = fun(IsFin, _Ref, Bin) -> Server ! {stream_chunk, Pid, IsFin, Bin} end,
            Options = #{async => true, async_mode => binary, handle_event => F, allow_reconnect => false},
            case shotgun:get(Pid, Path ++ Query, Headers, Options) of
                {error, Reason} ->
                    shotgun:close(Pid),
                    SafeReason = ldclient_key_redaction:format_shotgun_error(Reason),
                    {error, temporary, SafeReason};
                {ok, #{status_code := StatusCode}} when StatusCode >= 400 ->
                    % Close the connection: nothing else does on this path, so the
                    % shotgun client and its connection would otherwise be left
                    % running during retry backoff.
                    shotgun:close(Pid),
                    {error, ldclient_http:is_http_error_code_recoverable(StatusCode), StatusCode};
                {ok, _Ref} ->
                    {ok, Pid}
            end
    end.

%% @doc Frame and process bytes received from the streaming connection
%% @private
%%
%% @end
-spec handle_stream_chunk(binary(), state()) -> state().
handle_stream_chunk(Bin, #{
    sse_buffer := Buffer,
    feature_store := FeatureStore,
    storage_tag := Tag,
    conn := ShotgunPid,
    read_timer := ReadTimer,
    read_timeout_ms := ReadTimeoutMs
} = State) ->
    NewTimer = restart_read_timer(ReadTimer, ReadTimeoutMs),
    {Events, Rest} = split_sse_events(<<Buffer/binary, Bin/binary>>),
    try
        lists:foreach(fun(EventBin) -> process_event_bin(EventBin, FeatureStore, Tag) end, Events)
    catch Code:_Reason ->
        % Exception when processing event - don't log exception details
        % as they could theoretically contain sensitive data
        error_logger:warning_msg("Invalid SSE event error (~p)", [Code]),
        close_conn(ShotgunPid)
    end,
    State#{sse_buffer := Rest, read_timer := NewTimer}.

%% @doc Split a buffer into complete SSE events and the unterminated tail
%% @private
%%
%% @end
-spec split_sse_events(binary()) -> {[binary()], binary()}.
split_sse_events(Buffer) ->
    [Rest | ReversedEvents] = lists:reverse(binary:split(Buffer, <<"\n\n">>, [global])),
    {lists:reverse(ReversedEvents), drop_complete_comment_lines(Rest)}.

%% Heartbeat comments never complete an event; drop finished comment lines so a quiet
%% connection does not buffer them forever.
-spec drop_complete_comment_lines(binary()) -> binary().
drop_complete_comment_lines(Rest) ->
    [Last | ReversedLines] = lists:reverse(binary:split(Rest, <<"\n">>, [global])),
    Kept = [Line || Line <- lists:reverse(ReversedLines), not is_comment_line(Line)],
    iolist_to_binary(lists:join(<<"\n">>, Kept ++ [Last])).

-spec is_comment_line(binary()) -> boolean().
is_comment_line(<<":", _/binary>>) -> true;
is_comment_line(_Line) -> false.

-spec process_event_bin(binary(), FeatureStore :: atom(), Tag :: atom()) -> ok.
process_event_bin(EventBin, FeatureStore, Tag) ->
    case parse_shotgun_event(EventBin) of
        #{event := _Event} = Event -> process_event(Event, FeatureStore, Tag);
        _NoEvent -> ok
    end.

-spec start_read_timer(non_neg_integer()) -> reference() | undefined.
start_read_timer(0) -> undefined;
start_read_timer(ReadTimeoutMs) -> erlang:start_timer(ReadTimeoutMs, self(), read_timeout).

-spec cancel_read_timer(reference() | undefined) -> undefined.
cancel_read_timer(undefined) -> undefined;
cancel_read_timer(TimerRef) ->
    _ = erlang:cancel_timer(TimerRef),
    undefined.

-spec restart_read_timer(reference() | undefined, non_neg_integer()) -> reference() | undefined.
restart_read_timer(TimerRef, ReadTimeoutMs) ->
    _ = cancel_read_timer(TimerRef),
    start_read_timer(ReadTimeoutMs).

%% The connection may already be gone; its DOWN message handles the reconnect.
-spec close_conn(pid() | undefined) -> ok.
close_conn(undefined) -> ok;
close_conn(ShotgunPid) ->
    _ = (catch shotgun:close(ShotgunPid)),
    ok.

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
