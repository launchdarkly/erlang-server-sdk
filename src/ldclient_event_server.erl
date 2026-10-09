%%-------------------------------------------------------------------
%% @doc Event server
%% @private
%% @end
%%-------------------------------------------------------------------
-module(ldclient_event_server).

-behaviour(gen_server).

%% Supervision
-export([start_link/1, init/1]).

%% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2, format_status/1]).

%% API
-export([add_event/3, flush/1]).

-type state() :: #{
    tag := atom(),
    buffer := ldclient_event_buffer:buffer(),
    event_count := non_neg_integer(),
    counters_ref := counters:counters_ref(),
    summary_event := summary_event(),
    pending_summaries := [summary_event()],
    capacity := pos_integer(),
    shed_threshold := pos_integer(),
    inbox_capacity := pos_integer(),
    batch_size := pos_integer(),
    flush_interval := pos_integer(),
    timer_ref := reference(),
    flushing := boolean(),
    flush_remaining := non_neg_integer(),
    deferred_flush := boolean(),
    dropped := non_neg_integer(),
    idle_workers := [pid()],
    busy_workers := #{pid() => in_flight()},
    retry_batches := [{[ldclient_event:event()], summary_event() | undefined, uuid:uuid()}],
    pool_size := pos_integer(),
    housekeeping_interval_ms := pos_integer(),
    housekeeping_timer_ref := reference(),
    worker_monitors := #{reference() => pid()},
    offline := boolean(),
    send_events := boolean(),
    context_cache := ldclient_context_cache:cache(),
    context_keys_capacity := pos_integer()
}.

%% A batch handed to a worker, kept until the worker reports back so that it can
%% be re-dispatched once if the worker exits mid-send. The payload id travels with
%% it so a re-dispatch reuses the same `X-LaunchDarkly-Payload-ID' and the
%% service can deduplicate a request that was in fact delivered.
-type in_flight() :: {[ldclient_event:event()], summary_event() | undefined, Attempt :: 0 | 1, uuid:uuid()}.

-type summary_event() :: #{} | #{
    counters := counters(),
    start_date := non_neg_integer(),
    end_date := non_neg_integer(),
    context_kinds := #{
        flag_key := [ldclient_context:kind_value()]
    }
}.

-type counters() :: #{
    counter_key() => counter_value()
}.

-type counter_key() :: #{
    key := ldclient_flag:key(),
    variation := non_neg_integer(),
    version := non_neg_integer()
}.

-type counter_value() :: #{
    count := non_neg_integer(),
    flag_value := ldclient_eval:result_value(),
    flag_default := term()
}.

-type options() :: #{
    include_reasons => boolean()
}.

-export_type([summary_event/0]).
-export_type([counters/0]).
-export_type([counter_key/0]).
-export_type([counter_value/0]).

%% Key used to publish the caller-side depth counter for a tag. Callers read
%% this (instead of calling into the event server) so that load can be shed
%% before the server's mailbox grows.
-define(COUNTERS_KEY, ldclient_event_server_counters).
%% Counter slots: buffered full-fidelity events, casts queued in the mailbox,
%% and events the callers discarded (shed) since the last flush, by reason.
-define(DEPTH, 1).
-define(INFLIGHT, 2).
-define(INBOX_DROPS, 3).
-define(THRESHOLD_DROPS, 4).
-define(COUNTER_SLOTS, 4).

%%===================================================================
%% API
%%===================================================================

%% @doc Add an event to the buffer
%%
%% Events are not sent immediately. They are kept in buffer up to configured
%% size and flushed at configured interval.
%%
%% This call never blocks the caller: the event is cast to the event server.
%% Admission is decided from two shared counters published by the event server:
%% the number of buffered full-fidelity events and the number of casts still
%% queued in its mailbox.
%%
%% <ul>
%%   <li>Every event is shed once the queued casts reach
%%       `events_inbox_capacity', i.e. the event server is already a full
%%       buffer behind. Below that bound every evaluation reaches the summary,
%%       even while full-fidelity payloads are dropped at `events_capacity'.</li>
%%   <li>Best-effort events (identify/custom) are additionally shed once the
%%       buffered event count reaches `events_shed_threshold', since they would
%%       be dropped at capacity anyway.</li>
%% </ul>
%%
%% A shed event is counted in the shared counters (`reason' `inbox' or
%% `shed_threshold') and reported by the event server with the next flush, as
%% `[ldclient, events, dropped]', so that nothing but a counter increment runs
%% on the caller's side while the pipeline is overloaded.
%% @end
-spec add_event(Tag :: atom(), Event :: ldclient_event:event(), Options :: options()) ->
    ok.
add_event(Tag, #{type := Type} = Event, Options) when is_atom(Tag) ->
    case admit(Tag, Type) of
        {ok, undefined} ->
            cast_event(Tag, Event, Options);
        {ok, Ref} ->
            counters:add(Ref, ?INFLIGHT, 1),
            cast_event(Tag, Event, Options);
        {shed, Ref, Reason} ->
            counters:add(Ref, drop_slot(Reason), 1)
    end.

-spec drop_slot(inbox | shed_threshold) -> ?INBOX_DROPS | ?THRESHOLD_DROPS.
drop_slot(inbox) -> ?INBOX_DROPS;
drop_slot(shed_threshold) -> ?THRESHOLD_DROPS.

-spec cast_event(Tag :: atom(), Event :: ldclient_event:event(), Options :: options()) -> ok.
cast_event(Tag, Event, Options) ->
    ServerName = get_local_reg_name(Tag),
    gen_server:cast(ServerName, {add_event, Event, Tag, Options}).

%% @doc Flush buffered events
%%
%% @end
-spec flush(Tag :: atom()) -> ok.
flush(Tag) when is_atom(Tag) ->
    ServerName = get_local_reg_name(Tag),
    %% Flushing is asynchronous: the handler replies as soon as the flush has
    %% been started (its first batches handed to idle workers), or deferred when
    %% every reporter worker is still busy, in which case it starts as soon as a
    %% worker is free. It never waits for the HTTP requests to complete. The wait
    %% is bounded by the inbox capacity divided by the server's ingest rate, so
    %% no timeout is imposed on the caller.
    gen_server:call(ServerName, {flush, Tag}, infinity).

%%===================================================================
%% Supervision
%%===================================================================

%% @doc Starts the server
%%
%% @end
-spec start_link(Tag :: atom()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Tag) ->
    ServerName = get_local_reg_name(Tag),
    error_logger:info_msg("Starting event storage server for ~p with name ~p", [Tag, ServerName]),
    gen_server:start_link({local, ServerName}, ?MODULE, [Tag], []).

-spec init(Args :: term()) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init([Tag]) ->
    FlushInterval = ldclient_config:get_value(Tag, events_flush_interval),
    Capacity = ldclient_config:get_value(Tag, events_capacity),
    ShedThreshold = ldclient_config:get_value(Tag, events_shed_threshold),
    InboxCapacity = ldclient_config:get_value(Tag, events_inbox_capacity),
    BatchSize = ldclient_config:get_value(Tag, events_batch_size),
    PoolSize = ldclient_config:get_value(Tag, events_flush_workers),
    HousekeepingInterval = ldclient_config:get_value(Tag, events_housekeeping_interval_ms),
    TimerRef = erlang:send_after(FlushInterval, self(), {flush, Tag}),
    OfflineMode = ldclient:is_offline(Tag),
    SendEvents = ldclient_config:get_value(Tag, send_events),
    Buffer = ldclient_event_buffer:new(),
    _ = ets:new(
        ldclient_event_process_server:ets_table_name(Tag),
        [set, named_table, public, {read_concurrency, true}]
    ),
    CountersRef = counters:new(?COUNTER_SLOTS, [write_concurrency]),
    % Need to trap exit so supervisor:terminate_child calls terminate callback
    process_flag(trap_exit, true),
    State = #{
        tag => Tag,
        buffer => Buffer,
        event_count => 0,
        counters_ref => CountersRef,
        summary_event => #{},
        pending_summaries => [],
        capacity => Capacity,
        shed_threshold => ShedThreshold,
        inbox_capacity => InboxCapacity,
        batch_size => BatchSize,
        flush_interval => FlushInterval,
        timer_ref => TimerRef,
        flushing => false,
        flush_remaining => 0,
        deferred_flush => false,
        dropped => 0,
        idle_workers => [],
        busy_workers => #{},
        retry_batches => [],
        pool_size => PoolSize,
        housekeeping_interval_ms => HousekeepingInterval,
        housekeeping_timer_ref => erlang:send_after(HousekeepingInterval, self(), housekeeping),
        worker_monitors => #{},
        offline => OfflineMode,
        send_events => SendEvents,
        %% Seen-context set for index-event dedup: ETS owned by this process, no
        %% message round trip per event.
        context_cache => ldclient_context_cache:new(),
        context_keys_capacity => ldclient_config:get_value(Tag, context_keys_capacity)
    },
    %% The server is registered before `init/1' runs and callers admit without
    %% counting while no counters are published, so publish them first, with
    %% the gate closed: callers shed for the few milliseconds the pool takes to
    %% start instead of filling the mailbox uncounted. The forced resync below
    %% opens the gate once there is a worker to deliver to.
    ok = close_gate(State),
    ok = carry_over_drops(Tag, CountersRef),
    persistent_term:put({?COUNTERS_KEY, Tag}, {CountersRef, ShedThreshold, InboxCapacity}),
    %% Any workers left over from a previous incarnation are stale.
    ok = ldclient_event_worker_sup:stop_all(Tag),
    InitialState = start_workers(State, PoolSize),
    case maps:get(idle_workers, InitialState) of
        [] ->
            %% Without a single reporter nothing would ever be sent; fail loudly
            %% (as the previous single-process pipeline did) instead of
            %% accepting events into a pool that cannot deliver them.
            _ = ldclient_event_buffer:delete(Buffer),
            _ = erase_counters(State),
            _ = stop_dispatcher(Tag),
            {stop, {event_workers_unavailable, Tag}};
        _ ->
            %% Casts that arrived before the counters were published were not
            %% counted; start from the real mailbox length.
            _ = resync_inflight(InitialState, force),
            ok = emit_pool_size(InitialState, initial),
            {ok, InitialState}
    end.

%%===================================================================
%% Behavior callbacks
%%===================================================================

-type from() :: {pid(), term()}.
-spec handle_call(Request :: term(), From :: from(), State :: state()) ->
    {reply, Reply :: term(), NewState :: state()} |
    {stop, normal, {error, atom(), term()}, state()}.
handle_call(_Request, _From, #{offline := true} = State) ->
    {reply, ok, State};
handle_call(_Request, _From, #{send_events := false} = State) ->
    {reply, ok, State};
handle_call({flush, _Tag}, _From, State) ->
    {reply, ok, start_flush(State, explicit)};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({add_event, _Event, _Tag, _Options} = Msg, State) ->
    %% One queued cast has been taken off the mailbox.
    ok = dequeued(State),
    handle_add_event(Msg, State);
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_add_event(term(), state()) -> {noreply, state()}.
handle_add_event(_Msg, #{offline := true} = State) ->
    {noreply, State};
handle_add_event(_Msg, #{send_events := false} = State) ->
    {noreply, State};
handle_add_event({add_event, Event, Tag, Options}, #{buffer := Buffer, event_count := Count, summary_event := SummaryEvent, capacity := Capacity, dropped := Dropped, context_cache := Cache, context_keys_capacity := KeysCapacity} = State) ->
    {Added, NewSummaryEvent, NewCount, NewDropped, NewCache} = add_event(Tag, Event, Options, SummaryEvent, Count, Capacity, Dropped, Cache, KeysCapacity),
    lists:foreach(fun(E) -> ok = ldclient_event_buffer:insert(Buffer, E) end, lists:reverse(Added)),
    {noreply, set_count(State#{summary_event := NewSummaryEvent, dropped := NewDropped, context_cache := NewCache}, NewCount)}.

handle_info({flush, _Tag}, State) ->
    {noreply, start_flush(State, timer)};
handle_info(housekeeping, #{housekeeping_interval_ms := Interval} = State) ->
    %% Clamp the queued cast counter to the real mailbox length, replace any
    %% worker that could not be (re)started earlier, and resume dispatching to a
    %% worker that appeared.
    Before = live_workers(State),
    State1 = ensure_workers(resync_inflight(State)),
    case live_workers(State1) > Before of
        true -> ok = emit_pool_size(State1, up);
        false -> ok
    end,
    State2 = maybe_start_deferred(maybe_drain(State1)),
    Ref = erlang:send_after(Interval, self(), housekeeping),
    {noreply, State2#{housekeeping_timer_ref := Ref}};
handle_info({worker_done, Pid}, #{busy_workers := Busy, idle_workers := Idle} = State) ->
    State1 = State#{busy_workers := maps:remove(Pid, Busy), idle_workers := [Pid|Idle]},
    {noreply, maybe_start_deferred(maybe_drain(State1))};
handle_info({'DOWN', Ref, process, Pid, Reason}, #{busy_workers := Busy} = State) ->
    State1 = requeue_lost_batch(maps:find(Pid, Busy), Reason, State),
    State2 = remove_worker(State1, Ref, Pid),
    State3 = ensure_workers(State2),
    case live_workers(State3) < live_workers(State) of
        true -> ok = emit_pool_size(State3, down);
        false -> ok
    end,
    {noreply, maybe_start_deferred(maybe_drain(State3))};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: state()) -> term().
terminate(Reason, #{timer_ref := TimerRef, housekeeping_timer_ref := HousekeepingTimerRef, buffer := Buffer, tag := Tag} = State) ->
    error_logger:info_msg("Terminating event service, reason: ~p", [Reason]),
    _ = erlang:cancel_timer(TimerRef),
    _ = erlang:cancel_timer(HousekeepingTimerRef),
    _ = ldclient_event_buffer:delete(Buffer),
    _ = ldclient_context_cache:delete(maps:get(context_cache, State)),
    %% Drops counted since the last flush would otherwise never be reported.
    _ = report_dropped(State),
    _ = stop_dispatcher(Tag),
    case Reason of
        normal -> _ = erase_counters(State);
        shutdown -> _ = erase_counters(State);
        {shutdown, _} -> _ = erase_counters(State);
        _ ->
            %% Abnormal exit: keep the published counters but close the gate, so
            %% callers shed while the crash report is written and the supervisor
            %% restarts the server. The new incarnation publishes fresh counters.
            close_gate(State)
    end,
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc Keep crash reports small: in-flight batches can hold a full buffer each.
%% @private
format_status(#{state := State} = Status) when is_map(State) ->
    Busy = maps:map(fun(_Pid, {Batch, _Summary, Attempt, _PayloadId}) ->
                        #{batch_size => length(Batch), attempt => Attempt}
                    end, maps:get(busy_workers, State, #{})),
    Status#{state => State#{
        busy_workers => Busy,
        retry_batches => #{count => length(maps:get(retry_batches, State, []))}
    }};
format_status(Status) ->
    Status.

%%===================================================================
%% Internal functions
%%===================================================================

%% Accumulator threaded through the per-event helpers: events to insert (newest
%% first), the buffered count, the number of events dropped at capacity, and the
%% seen-context cache (which may rotate on insert).
-type acc() :: {[ldclient_event:event()], non_neg_integer(), non_neg_integer(), ldclient_context_cache:cache()}.

-spec add_event(
    Tag :: atom(),
    Event :: ldclient_event:event(),
    Options :: options(),
    SummaryEvent :: summary_event(),
    Count :: non_neg_integer(),
    Capacity :: pos_integer(),
    Dropped :: non_neg_integer(),
    Cache :: ldclient_context_cache:cache(),
    KeysCapacity :: pos_integer()
) ->
    {[ldclient_event:event()], summary_event(), non_neg_integer(), non_neg_integer(), ldclient_context_cache:cache()}.
add_event(Tag, #{type := feature_request, context := Context, timestamp := Timestamp} = Event, Options, SummaryEvent, Count, Capacity, Dropped, Cache, KeysCapacity) ->
    AddFull = should_add_full_event(Event),
    AddDebug = should_add_debug_event(Event, Tag),
    NewSummaryEvent = add_feature_request_event(Event, SummaryEvent),
    Acc1 = maybe_add_index_event(Context, Timestamp, Capacity, KeysCapacity, {[], Count, Dropped, Cache}),
    Acc2 = maybe_add_feature_request_full_fidelity(AddFull, Event, Options, Capacity, Acc1),
    {Added, NewCount, NewDropped, NewCache} = maybe_add_debug_event(AddDebug, Event, Options, Capacity, Acc2),
    {Added, NewSummaryEvent, NewCount, NewDropped, NewCache};
add_event(_Tag, #{type := identify, context := Context} = Event, _Options, SummaryEvent, Count, Capacity, Dropped, Cache, KeysCapacity) ->
    % Notice the context, but do not conditionally add the index event.
    {_Seen, Cache1} = ldclient_context_cache:notice_context(Cache, Context, KeysCapacity),
    {Added, NewCount, NewDropped, NewCache} = add_raw_event(Event, Capacity, {[], Count, Dropped, Cache1}),
    {Added, SummaryEvent, NewCount, NewDropped, NewCache};
add_event(_Tag, #{type := custom, context := Context, timestamp := Timestamp} = Event, _Options, SummaryEvent, Count, Capacity, Dropped, Cache, KeysCapacity) ->
    Acc1 = maybe_add_index_event(Context, Timestamp, Capacity, KeysCapacity, {[], Count, Dropped, Cache}),
    {Added, NewCount, NewDropped, NewCache} = add_raw_event(Event, Capacity, Acc1),
    {Added, SummaryEvent, NewCount, NewDropped, NewCache}.

-spec add_raw_event(ldclient_event:event(), pos_integer(), acc()) -> acc().
add_raw_event(Event, Capacity, {Added, Count, Dropped, Cache}) when Count < Capacity ->
    {[Event|Added], Count + 1, Dropped, Cache};
add_raw_event(_, _Capacity, {Added, Count, Dropped, Cache}) ->
    %% Counted here and reported once per flush window (log line + telemetry)
    %% instead of logging every dropped event, which flooded the log at high
    %% evaluation rates.
    {Added, Count, Dropped + 1, Cache}.

-spec add_feature_request_event(ldclient_event:event(), summary_event()) ->
    summary_event().
add_feature_request_event(
    #{
        timestamp := Timestamp,
        context := Context,
        data := #{
            key := Key,
            value := Value,
            default := Default,
            variation := Variation,
            version := Version
        }
    },
    SummaryEvent
) when map_size(SummaryEvent) == 0 ->
    SummaryEventKey = create_summary_event_key(Key, Variation, Version),
    SummaryEventValue = create_summary_event_value(Value, Default),
    #{
        start_date => Timestamp,
        end_date => Timestamp,
        counters => #{SummaryEventKey => SummaryEventValue},
        context_kinds => #{
            Key => ldclient_context:get_kinds(Context)
        }
    };
add_feature_request_event(
    #{
        timestamp := Timestamp,
        context := Context,
        data := #{
            key := Key,
            value := Value,
            default := Default,
            variation := Variation,
            version := Version
        }
    },
    #{
        start_date := CurrStartDate,
        end_date := CurrEndDate,
        counters := SummaryEventCounters,
        context_kinds := SummaryContextKinds
    } = SummaryEvent
) ->
    ContextKindsForKey = maps:get(Key, SummaryContextKinds, []),
    SummaryEventKey = create_summary_event_key(Key, Variation, Version),
    NewSummaryEvenValue = case maps:get(SummaryEventKey, SummaryEventCounters, undefined) of
        undefined ->
            create_summary_event_value(Value, Default);
        SummaryEventValue ->
            SummaryEventValue#{count := maps:get(count, SummaryEventValue) + 1}
    end,
    NewSummaryEventCounters = SummaryEventCounters#{SummaryEventKey => NewSummaryEvenValue},
    NewStartDate = if Timestamp < CurrStartDate -> Timestamp; true -> CurrStartDate end,
    NewEndDate = if Timestamp > CurrEndDate -> Timestamp; true -> CurrEndDate end,
    SummaryEvent#{
        counters => NewSummaryEventCounters,
        start_date => NewStartDate,
        end_date => NewEndDate,
        context_kinds => SummaryContextKinds#{
            Key => sets:to_list(sets:from_list(ldclient_context:get_kinds(Context) ++ ContextKindsForKey))
        }
    }.

-spec should_add_full_event(ldclient_event:event()) -> boolean().
should_add_full_event(#{data := #{trackEvents := true}}) -> true;
should_add_full_event(_) -> false.

-spec maybe_add_feature_request_full_fidelity(boolean(), ldclient_event:event(), options(), pos_integer(), acc()) -> acc().
maybe_add_feature_request_full_fidelity(true, Event, #{include_reasons := true}, Capacity, Acc) ->
    add_raw_event(Event, Capacity, Acc);
maybe_add_feature_request_full_fidelity(true, #{data := #{include_reason := true}} = Event, _Options, Capacity, Acc) ->
    add_raw_event(Event, Capacity, Acc);
maybe_add_feature_request_full_fidelity(true, Event, _Options, Capacity, Acc) ->
    add_raw_event(ldclient_event:strip_eval_reason(Event), Capacity, Acc);
maybe_add_feature_request_full_fidelity(false, _Event, _Options, _Capacity, Acc) ->
    Acc.

-spec maybe_add_index_event(ldclient_context:context(), non_neg_integer(), pos_integer(), pos_integer(), acc()) -> acc().
maybe_add_index_event(Context, Timestamp, Capacity, KeysCapacity, {Added, Count, Dropped, Cache}) ->
    case ldclient_context_cache:notice_context(Cache, Context, KeysCapacity) of
        {true, Cache1} -> {Added, Count, Dropped, Cache1};
        {false, Cache1} -> add_raw_event(ldclient_event:new_index(Context, Timestamp), Capacity, {Added, Count, Dropped, Cache1})
    end.

-spec should_add_debug_event(ldclient_event:event(), Tag :: atom()) -> boolean().
should_add_debug_event(#{data := #{debugEventsUntilDate := null}}, _Tag) -> false;
should_add_debug_event(#{data := #{debugEventsUntilDate := DebugDate}}, Tag) ->
    LastServerTime = ldclient_event_process_server:get_last_server_time(Tag),
    Now = erlang:system_time(milli_seconds),
    (DebugDate > Now) and (DebugDate >  LastServerTime).

-spec maybe_add_debug_event(boolean(), ldclient_event:event(), options(), pos_integer(), acc()) -> acc().
maybe_add_debug_event(false, _, _Options, _Capacity, Acc) -> Acc;
maybe_add_debug_event(true, #{data := EventData} = FeatureEvent, #{include_reasons := true}, Capacity, Acc) ->
    add_raw_event(FeatureEvent#{data := EventData#{debug => true}}, Capacity, Acc);
maybe_add_debug_event(true, #{data := EventData} = FeatureEvent, _Options, Capacity, Acc) ->
    add_raw_event(ldclient_event:strip_eval_reason(FeatureEvent#{data := EventData#{debug => true}}), Capacity, Acc).

-spec create_summary_event_key(ldclient_flag:key(), ldclient_flag:variation(), ldclient_flag:version()) ->
    counter_key().
create_summary_event_key(Key, Variation, Version) ->
    #{
        key => Key,
        variation => Variation,
        version => Version
    }.

-spec create_summary_event_value(ldclient_eval:result_value(), term()) ->
    counter_value().
create_summary_event_value(Value, Default) ->
    #{
        count => 1,
        flag_value => Value,
        flag_default => Default
    }.

-spec get_local_reg_name(Tag :: atom()) -> atom().
get_local_reg_name(Tag) ->
    list_to_atom("ldclient_event_server_" ++ atom_to_list(Tag)).

%% @doc Decide whether an incoming event may be cast to the event server, using
%% the counters the server publishes: buffered full-fidelity events and casts
%% still queued in its mailbox. Every kind is shed once the queued casts reach
%% `events_inbox_capacity' (a bounded input queue, as the reference SDKs have);
%% best-effort events are also shed once the buffer itself is at
%% `events_shed_threshold', because they would be dropped at capacity anyway.
%%
%% The persistent_term entry outlives an abnormal exit of the server with its
%% gate closed (see `terminate/2'), and is erased on a normal stop; the next
%% incarnation publishes fresh counters.
%% @end
-spec admit(Tag :: atom(), Type :: atom()) ->
    {ok, counters:counters_ref() | undefined} | {shed, counters:counters_ref(), inbox | shed_threshold}.
admit(Tag, Type) ->
    case persistent_term:get({?COUNTERS_KEY, Tag}, undefined) of
        undefined ->
            {ok, undefined};
        {Ref, ShedThreshold, InboxCapacity} ->
            case counters:get(Ref, ?INFLIGHT) >= InboxCapacity of
                true ->
                    {shed, Ref, inbox};
                false when Type =/= feature_request ->
                    case counters:get(Ref, ?DEPTH) >= ShedThreshold of
                        true -> {shed, Ref, shed_threshold};
                        false -> {ok, Ref}
                    end;
                false ->
                    {ok, Ref}
            end
    end.

%% @doc Take the drops the callers counted since the last report, leaving any
%% that are counted concurrently in place.
%% @end
-spec take_drops(counters:counters_ref(), ?INBOX_DROPS | ?THRESHOLD_DROPS) -> non_neg_integer().
take_drops(Ref, Slot) ->
    case counters:get(Ref, Slot) of
        Count when Count > 0 ->
            counters:sub(Ref, Slot, Count),
            Count;
        _ ->
            0
    end.

%% @doc Drops counted against a previous incarnation's counters (callers shed
%% while the server restarted after a crash) would otherwise never be
%% reported; the new incarnation reports them with its first flush.
%% @end
-spec carry_over_drops(Tag :: atom(), counters:counters_ref()) -> ok.
carry_over_drops(Tag, NewRef) ->
    case persistent_term:get({?COUNTERS_KEY, Tag}, undefined) of
        {OldRef, _, _} ->
            lists:foreach(
                fun(Slot) -> counters:add(NewRef, Slot, take_drops(OldRef, Slot)) end,
                [?INBOX_DROPS, ?THRESHOLD_DROPS]);
        undefined ->
            ok
    end.

-spec update_counters(state()) -> state().
update_counters(#{counters_ref := Ref, event_count := Count} = State) ->
    counters:put(Ref, ?DEPTH, Count),
    State.

%% @doc Account for one cast leaving the mailbox. Callers increment the slot
%% before casting; drift (casts to a dead server, counts from a previous
%% incarnation) is corrected by `resync_inflight/1' on every housekeeping tick.
%% @end
-spec dequeued(state()) -> ok.
dequeued(#{counters_ref := Ref}) ->
    counters:sub(Ref, ?INFLIGHT, 1).

%% @doc Keep the queued-cast counter honest. The mailbox length also counts the
%% few non-cast messages (`worker_done', timers, system messages), so periodic
%% resyncs only clamp the counter into `[0, Len]' instead of overwriting it; a
%% `force' resync (at startup) takes the mailbox length as is.
%% @end
-spec resync_inflight(state()) -> state().
resync_inflight(State) ->
    resync_inflight(State, clamp).

-spec resync_inflight(state(), clamp | force) -> state().
resync_inflight(#{counters_ref := Ref} = State, Mode) ->
    {message_queue_len, Len} = process_info(self(), message_queue_len),
    Current = counters:get(Ref, ?INFLIGHT),
    case Mode =:= force orelse Current < 0 orelse Current > Len of
        true -> counters:put(Ref, ?INFLIGHT, Len);
        false -> ok
    end,
    State.

%% @doc Let the dispatcher release what it set up for this instance (an httpc
%% profile, for example). The callback is optional.
%% @end
-spec stop_dispatcher(Tag :: atom()) -> ok.
stop_dispatcher(Tag) ->
    try
        Dispatcher = ldclient_config:get_value(Tag, events_dispatcher),
        _ = code:ensure_loaded(Dispatcher),
        case erlang:function_exported(Dispatcher, stop, 1) of
            true -> Dispatcher:stop(Tag);
            false -> ok
        end
    catch _:_ ->
        ok
    end.

%% @doc Make callers shed everything until a new incarnation publishes counters.
%% @end
-spec close_gate(state()) -> ok.
close_gate(#{counters_ref := Ref, inbox_capacity := InboxCapacity}) ->
    counters:put(Ref, ?INFLIGHT, InboxCapacity),
    ok.

-spec set_count(state(), non_neg_integer()) -> state().
set_count(State, Count) ->
    update_counters(State#{event_count := Count}).

-spec erase_counters(state()) -> boolean().
erase_counters(#{tag := Tag}) ->
    persistent_term:erase({?COUNTERS_KEY, Tag}).

%%===================================================================
%% Pool scheduling
%%===================================================================

%% @doc Start a flush, or defer it.
%%
%% A flush with nothing to send is a no-op. Otherwise, if a reporter worker is
%% idle a flush window is opened over everything buffered now; if every worker
%% is still busy with a previous request the flush is deferred and runs as soon
%% as a worker is free (one deferred flush at a time, like the single waiting
%% payload in the Go and Java SDKs). Meanwhile the events stay in the buffer and
%% the summary keeps accumulating; nothing is lost here, and the buffer's
%% capacity bound applies as always. Exited workers are replaced before the pool
%% is judged busy.
%%
%% The periodic timer is re-armed when it fired (`timer') and when an explicit
%% flush actually starts a window; a deferred or skipped explicit flush leaves
%% the schedule alone.
%% @end
-spec start_flush(state(), timer | explicit | deferred) -> state().
start_flush(State0, Trigger) ->
    State = report_dropped(resync_inflight(State0#{deferred_flush := false})),
    case has_work(State) of
        false ->
            rearm_if_timer(State, Trigger);
        true ->
            State1 = ensure_workers(State),
            case maps:get(idle_workers, State1) of
                [] -> defer_flush(rearm_if_timer(State1, Trigger));
                _ -> open_window(State1, Trigger)
            end
    end.

-spec has_work(state()) -> boolean().
has_work(#{event_count := Count, summary_event := SummaryEvent}) ->
    Count > 0 orelse map_size(SummaryEvent) > 0.

-spec rearm_if_timer(state(), timer | explicit | deferred) -> state().
rearm_if_timer(State, timer) -> rearm_flush_timer(State);
rearm_if_timer(State, _Trigger) -> State.

-spec rearm_flush_timer(state()) -> state().
rearm_flush_timer(#{tag := Tag, flush_interval := FlushInterval, timer_ref := TimerRef} = State) ->
    _ = erlang:cancel_timer(TimerRef),
    State#{timer_ref := erlang:send_after(FlushInterval, self(), {flush, Tag})}.

-spec defer_flush(state()) -> state().
defer_flush(#{tag := Tag} = State) ->
    telemetry:execute([ldclient, events, flush_skipped], #{count => 1}, #{tag => Tag}),
    State#{deferred_flush := true}.

%% @doc Run a deferred flush once a worker is idle and no window is open.
%% @end
-spec maybe_start_deferred(state()) -> state().
maybe_start_deferred(#{deferred_flush := true, flushing := false, retry_batches := [], idle_workers := [_|_]} = State) ->
    start_flush(State, deferred);
maybe_start_deferred(State) ->
    State.

-spec open_window(state(), timer | explicit | deferred) -> state().
open_window(#{summary_event := SummaryEvent, pending_summaries := Pending, event_count := Count} = State0, Trigger) ->
    %% Re-arm the flush timer as soon as the window starts so a slow dispatcher
    %% cannot delay subsequent flushes.
    State = case Trigger of
        deferred -> State0;
        _ -> rearm_flush_timer(State0)
    end,
    NewPending = case map_size(SummaryEvent) of
        0 -> Pending;
        _ -> Pending ++ [SummaryEvent]
    end,
    %% Everything buffered now belongs to this window. A window is only opened
    %% while a worker is idle, which implies no other window is in progress, so
    %% at most one summary is ever pending.
    drain(State#{
        summary_event := #{},
        pending_summaries := NewPending,
        flushing := true,
        flush_remaining := Count
    }).

%% @doc Report the events discarded since the previous flush: those dropped
%% here at `events_capacity' and those the callers shed. One log line and one
%% `[ldclient, events, dropped]' telemetry event per reason and window.
%% @end
-spec report_dropped(state()) -> state().
report_dropped(#{dropped := Dropped, counters_ref := Ref} = State) ->
    ok = report_dropped(capacity, Dropped, State),
    ok = report_dropped(inbox, take_drops(Ref, ?INBOX_DROPS), State),
    ok = report_dropped(shed_threshold, take_drops(Ref, ?THRESHOLD_DROPS), State),
    State#{dropped := 0}.

-spec report_dropped(capacity | inbox | shed_threshold, non_neg_integer(), state()) -> ok.
report_dropped(_Reason, 0, _State) ->
    ok;
report_dropped(Reason, Count, #{tag := Tag} = State) ->
    telemetry:execute([ldclient, events, dropped], #{count => Count}, #{tag => Tag, reason => Reason}),
    {Format, Args} = dropped_message(Reason, Count, State),
    error_logger:warning_msg(Format, Args).

-spec dropped_message(capacity | inbox | shed_threshold, pos_integer(), state()) -> {string(), [term()]}.
dropped_message(capacity, Count, #{tag := Tag, capacity := Capacity}) ->
    {"Exceeded event queue capacity (~b) for ~p: dropped ~b events since the last flush. Increase events_capacity to avoid dropping events.",
     [Capacity, Tag, Count]};
dropped_message(inbox, Count, #{tag := Tag, inbox_capacity := InboxCapacity}) ->
    {"Exceeded event inbox capacity (~b) for ~p: ~b events were discarded before reaching the buffer since the last flush, and the evaluations among them are missing from the summary. Events are produced faster than the event server processes them.",
     [InboxCapacity, Tag, Count]};
dropped_message(shed_threshold, Count, #{tag := Tag, shed_threshold := ShedThreshold}) ->
    {"Exceeded event shed threshold (~b) for ~p: ~b identify and custom events were discarded since the last flush. Increase events_capacity (and events_shed_threshold, if set) to keep them.",
     [ShedThreshold, Tag, Count]}.

%% @doc Continue dispatching if a flush window is open or a batch is waiting to
%% be re-dispatched after its worker exited.
%% @end
-spec maybe_drain(state()) -> state().
maybe_drain(#{flushing := true} = State) -> drain(State);
maybe_drain(#{retry_batches := [_|_]} = State) -> drain(State);
maybe_drain(State) -> State.

%% @doc Hand out buffered batches to idle workers until there is no more work or
%% no worker is available. When the events captured at the start of the window
%% and their summary have been dispatched, the flush window is complete.
%% @end
-spec drain(state()) -> state().
drain(#{flush_remaining := 0, pending_summaries := [], retry_batches := []} = State) ->
    complete_flush(State);
drain(#{idle_workers := [Worker|Idle], retry_batches := [{Batch, Summary, PayloadId}|Rest]} = State) ->
    %% A batch whose worker exited mid-send gets exactly one more attempt, with
    %% the same payload id.
    State1 = dispatch(Worker, Batch, Summary, 1, PayloadId, State#{idle_workers := Idle, retry_batches := Rest}),
    drain(State1);
drain(#{idle_workers := [Worker|Idle], flush_remaining := Remaining} = State) when Remaining > 0 ->
    case pop_batch(State) of
        {[], State1} ->
            %% Nothing left to pop even though the window is not closed. Close
            %% it rather than spin dispatching empty batches.
            drain(State1#{flush_remaining := 0});
        {Batch, State1} ->
            {Summary, NewPending} = take_summary(maps:get(pending_summaries, State1)),
            State2 = dispatch(Worker, Batch, Summary, 0, uuid:get_v4(), State1#{idle_workers := Idle, pending_summaries := NewPending}),
            drain(State2)
    end;
drain(#{idle_workers := [Worker|Idle], flush_remaining := 0, pending_summaries := [Summary|Rest]} = State) ->
    State1 = dispatch(Worker, [], Summary, 0, uuid:get_v4(), State#{idle_workers := Idle, pending_summaries := Rest}),
    drain(State1);
drain(State) ->
    %% No idle worker: the rest of the window waits for `worker_done' or a DOWN
    %% message. The pool has a fixed size, so nothing is started here.
    State.

-spec pop_batch(state()) -> {[ldclient_event:event()], state()}.
pop_batch(#{buffer := Buffer, batch_size := BatchSize, event_count := Count, flush_remaining := Remaining} = State) ->
    %% Never pop more than the events that belong to the current flush window.
    %% Events inserted after the window started sit at the tail of the buffer and
    %% must be left for the next window.
    Take = min(BatchSize, Remaining),
    Batch = ldclient_event_buffer:pop_batch(Buffer, Take),
    {Batch, set_count(State#{flush_remaining := max(0, Remaining - length(Batch))}, Count - length(Batch))}.

-spec dispatch(pid(), [ldclient_event:event()], summary_event() | undefined, 0 | 1, uuid:uuid(), state()) -> state().
dispatch(Worker, Batch, Summary, Attempt, PayloadId, #{busy_workers := Busy} = State) ->
    ok = ldclient_event_process_server:send_batch(Worker, self(), Batch, Summary, PayloadId),
    %% Keep the batch until the worker reports back so it is not lost if the
    %% worker exits (crash, or a cast that raced with the worker's exit).
    State#{busy_workers := Busy#{Worker => {Batch, Summary, Attempt, PayloadId}}}.

-spec requeue_lost_batch({ok, in_flight()} | error, term(), state()) -> state().
requeue_lost_batch({ok, {Batch, Summary, 0, PayloadId}}, Reason, #{retry_batches := Retry, tag := Tag} = State) ->
    error_logger:warning_msg("Event worker for ~p exited (~p) while sending a batch of ~b events; re-dispatching it once",
        [Tag, Reason, length(Batch)]),
    State#{retry_batches := Retry ++ [{Batch, Summary, PayloadId}]};
requeue_lost_batch({ok, {Batch, _Summary, _Attempt, _PayloadId}}, Reason, #{tag := Tag} = State) ->
    telemetry:execute([ldclient, events, send_error], #{count => 1}, #{tag => Tag, type => worker_exit}),
    error_logger:error_msg("Event worker for ~p exited (~p) again while sending a batch of ~b events; dropping it",
        [Tag, Reason, length(Batch)]),
    State;
requeue_lost_batch(error, _Reason, State) ->
    State.

-spec take_summary([summary_event()]) -> {summary_event() | undefined, [summary_event()]}.
take_summary([Summary|Rest]) -> {Summary, Rest};
take_summary([]) -> {undefined, []}.

%% @doc Close a flush window. Events buffered since the window started are left
%% in place (and their summary counts are kept in `summary_event') for the next
%% window, so completing a flush never discards ongoing evaluations.
%% @end
-spec complete_flush(state()) -> state().
complete_flush(State) ->
    State#{
        pending_summaries := [],
        flushing := false,
        flush_remaining := 0
    }.

%% @doc Every live worker, including one that is still attempting a retry.
%% @end
-spec live_workers(state()) -> non_neg_integer().
live_workers(#{worker_monitors := Monitors}) ->
    map_size(Monitors).

%% @doc Emit the current pool size. `direction' describes the transition that
%% produced this sample (`initial', `up' when a crashed worker was replaced,
%% `down' when a worker exited), while `workers' is the absolute value suitable
%% for a gauge metric.
%% @end
-spec emit_pool_size(state(), initial | up | down) -> ok.
emit_pool_size(#{tag := Tag, idle_workers := Idle, busy_workers := Busy}, Direction) ->
    telemetry:execute(
        [ldclient, events, pool_size],
        #{workers => length(Idle) + map_size(Busy)},
        #{tag => Tag, direction => Direction}
    ).

-spec start_workers(state(), integer()) -> state().
start_workers(State, Remaining) when Remaining =< 0 ->
    State;
start_workers(State, Remaining) ->
    start_workers(start_worker(State), Remaining - 1).

-spec start_worker(state()) -> state().
start_worker(#{tag := Tag, idle_workers := Idle, worker_monitors := Monitors} = State) ->
    %% The worker supervisor may itself be restarting; a failed start is retried
    %% on the next housekeeping tick by `ensure_workers/1' instead of crashing here.
    try ldclient_event_worker_sup:start_worker(Tag) of
        {ok, Pid} ->
            Ref = erlang:monitor(process, Pid),
            State#{idle_workers := [Pid|Idle], worker_monitors := Monitors#{Ref => Pid}};
        {error, Reason} ->
            error_logger:error_msg("Could not start event worker for ~p: ~p", [Tag, Reason]),
            State
    catch
        exit:Reason ->
            error_logger:error_msg("Could not start event worker for ~p: ~p", [Tag, Reason]),
            State
    end.

%% @doc Replace workers that exited (or could not be started earlier) so the
%% pool is back at its configured size.
%% @end
-spec ensure_workers(state()) -> state().
ensure_workers(#{pool_size := PoolSize} = State) ->
    Live = live_workers(State),
    case PoolSize > Live of
        true -> start_workers(State, PoolSize - Live);
        false -> State
    end.

-spec remove_worker(state(), reference(), pid()) -> state().
remove_worker(#{idle_workers := Idle, busy_workers := Busy, worker_monitors := Monitors} = State, Ref, Pid) ->
    State#{
        idle_workers := lists:delete(Pid, Idle),
        busy_workers := maps:remove(Pid, Busy),
        worker_monitors := maps:remove(Ref, Monitors)
    }.
