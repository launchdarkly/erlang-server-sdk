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
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

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
    batch_size := pos_integer(),
    flush_interval := pos_integer(),
    timer_ref := reference(),
    flushing := boolean(),
    idle_workers := [pid()],
    busy_workers := #{pid() => true},
    min_workers := pos_integer(),
    max_workers := pos_integer(),
    scale_up_threshold := non_neg_integer(),
    scale_down_threshold := non_neg_integer(),
    scale_interval_ms := pos_integer(),
    scale_cooldown_ms := non_neg_integer(),
    scale_timer_ref := reference(),
    last_scale_ms := integer(),
    worker_monitors := #{reference() => pid()},
    flush_waiters := [from()],
    offline := boolean(),
    send_events := boolean()
}.

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

%%===================================================================
%% API
%%===================================================================

%% @doc Add an event to the buffer
%%
%% Events are not sent immediately. They are kept in buffer up to configured
%% size and flushed at configured interval.
%%
%% This call never blocks the caller: the event is cast to the event server
%% unless the buffer is already at the configured shed threshold, in which case
%% the event is dropped (load shedding) and a telemetry event is emitted.
%% @end
-spec add_event(Tag :: atom(), Event :: ldclient_event:event(), Options :: options()) ->
    ok.
add_event(Tag, Event, Options) when is_atom(Tag) ->
    case should_shed(Tag) of
        true ->
            telemetry:execute([ldclient, events, shed], #{count => 1}, #{tag => Tag}),
            ok;
        false ->
            ServerName = get_local_reg_name(Tag),
            gen_server:cast(ServerName, {add_event, Event, Tag, Options})
    end.

%% @doc Flush buffered events
%%
%% @end
-spec flush(Tag :: atom) -> ok.
flush(Tag) when is_atom(Tag) ->
    ServerName = get_local_reg_name(Tag),
    gen_server:call(ServerName, {flush, Tag}).

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
    BatchSize = ldclient_config:get_value(Tag, events_batch_size),
    MinWorkers = ldclient_config:get_value(Tag, events_min_workers),
    MaxWorkers = ldclient_config:get_value(Tag, events_max_workers),
    ScaleUpThreshold = ldclient_config:get_value(Tag, events_scale_up_threshold),
    ScaleDownThreshold = ldclient_config:get_value(Tag, events_scale_down_threshold),
    ScaleInterval = ldclient_config:get_value(Tag, events_scale_interval_ms),
    ScaleCooldown = ldclient_config:get_value(Tag, events_scale_cooldown_ms),
    TimerRef = erlang:send_after(FlushInterval, self(), {flush, Tag}),
    OfflineMode = ldclient:is_offline(Tag),
    SendEvents = ldclient_config:get_value(Tag, send_events),
    Buffer = ldclient_event_buffer:new(),
    _ = ets:new(
        ldclient_event_process_server:ets_table_name(Tag),
        [set, named_table, public, {read_concurrency, true}]
    ),
    CountersRef = counters:new(1, [write_concurrency]),
    persistent_term:put({?COUNTERS_KEY, Tag}, {CountersRef, ShedThreshold}),
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
        batch_size => BatchSize,
        flush_interval => FlushInterval,
        timer_ref => TimerRef,
        flushing => false,
        idle_workers => [],
        busy_workers => #{},
        min_workers => MinWorkers,
        max_workers => MaxWorkers,
        scale_up_threshold => ScaleUpThreshold,
        scale_down_threshold => ScaleDownThreshold,
        scale_interval_ms => ScaleInterval,
        scale_cooldown_ms => ScaleCooldown,
        scale_timer_ref => erlang:send_after(ScaleInterval, self(), scale),
        last_scale_ms => erlang:monotonic_time(millisecond),
        worker_monitors => #{},
        flush_waiters => [],
        offline => OfflineMode,
        send_events => SendEvents
    },
    %% Any workers left over from a previous incarnation are stale.
    ok = ldclient_event_worker_sup:stop_all(Tag),
    {ok, start_workers(State, MinWorkers)}.

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
handle_call({flush, _Tag}, From, #{flush_waiters := Waiters} = State) ->
    {noreply, start_flush(State#{flush_waiters := [From|Waiters]})};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({add_event, Event, Tag, Options}, #{buffer := Buffer, event_count := Count, summary_event := SummaryEvent, capacity := Capacity} = State) ->
    {Added, NewSummaryEvent, NewCount} = add_event(Tag, Event, Options, SummaryEvent, Count, Capacity),
    lists:foreach(fun(E) -> ok = ldclient_event_buffer:insert(Buffer, E) end, lists:reverse(Added)),
    {noreply, set_count(State#{summary_event := NewSummaryEvent}, NewCount)};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({flush, _Tag}, State) ->
    {noreply, start_flush(State)};
handle_info(scale, #{scale_interval_ms := Interval} = State) ->
    State1 = maybe_scale(State),
    Ref = erlang:send_after(Interval, self(), scale),
    {noreply, State1#{scale_timer_ref := Ref}};
handle_info({worker_done, Pid}, #{busy_workers := Busy, idle_workers := Idle, flushing := Flushing} = State) ->
    State1 = State#{busy_workers := maps:remove(Pid, Busy), idle_workers := [Pid|Idle]},
    case Flushing of
        true -> {noreply, drain(State1)};
        false -> {noreply, State1}
    end;
handle_info({'DOWN', Ref, process, Pid, _Reason}, State) ->
    State1 = ensure_workers(remove_worker(State, Ref, Pid)),
    case maps:get(flushing, State1) of
        true -> {noreply, drain(State1)};
        false -> {noreply, State1}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: state()) -> term().
terminate(Reason, #{timer_ref := TimerRef, scale_timer_ref := ScaleTimerRef, buffer := Buffer} = State) ->
    error_logger:info_msg("Terminating event service, reason: ~p", [Reason]),
    _ = erlang:cancel_timer(TimerRef),
    _ = erlang:cancel_timer(ScaleTimerRef),
    _ = ldclient_event_buffer:delete(Buffer),
    _ = erase_counters(State),
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%===================================================================
%% Internal functions
%%===================================================================

-spec add_event(
    Tag :: atom(),
    Event :: ldclient_event:event(),
    Options :: options(),
    SummaryEvent :: summary_event(),
    Count :: non_neg_integer(),
    Capacity :: pos_integer()
) ->
    {[ldclient_event:event()], summary_event(), non_neg_integer()}.
add_event(Tag, #{type := feature_request, context := Context, timestamp := Timestamp} = Event, Options, SummaryEvent, Count, Capacity) ->
    AddFull = should_add_full_event(Event),
    AddDebug = should_add_debug_event(Event, Tag),
    NewSummaryEvent = add_feature_request_event(Event, SummaryEvent),
    {Added1, Count1} = maybe_add_index_event(Tag, Context, Timestamp, Capacity, Count),
    {Added2, Count2} = maybe_add_feature_request_full_fidelity(AddFull, Event, Options, Added1, Capacity, Count1),
    {Added3, Count3} = maybe_add_debug_event(AddDebug, Event, Options, Added2, Capacity, Count2),
    {Added3, NewSummaryEvent, Count3};
add_event(Tag, #{type := identify, context := Context} = Event, _Options, SummaryEvent, Count, Capacity) ->
    % Notice the context, but do not conditionally add the index event.
    ldclient_context_cache:notice_context(Tag, Context),
    {Added, NewCount} = add_raw_event(Event, [], Capacity, Count),
    {Added, SummaryEvent, NewCount};
add_event(Tag, #{type := custom, context := Context, timestamp := Timestamp} = Event, _Options, SummaryEvent, Count, Capacity) ->
    {Added1, Count1} = maybe_add_index_event(Tag, Context, Timestamp, Capacity, Count),
    {Added2, Count2} = add_raw_event(Event, Added1, Capacity, Count1),
    {Added2, SummaryEvent, Count2}.

-spec add_raw_event(ldclient_event:event(), [ldclient_event:event()], pos_integer(), non_neg_integer()) ->
    {[ldclient_event:event()], non_neg_integer()}.
add_raw_event(Event, Added, Capacity, Count) when Count < Capacity ->
    {[Event|Added], Count + 1};
add_raw_event(_, Added, _Capacity, Count) ->
    error_logger:warning_msg("Exceeded event queue capacity. Increase capacity to avoid dropping events."),
    {Added, Count}.

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

-spec maybe_add_feature_request_full_fidelity(boolean(), ldclient_event:event(), options(), [ldclient_event:event()], pos_integer(), non_neg_integer()) ->
    {[ldclient_event:event()], non_neg_integer()}.
maybe_add_feature_request_full_fidelity(true, Event, #{include_reasons := true}, Added, Capacity, Count) ->
    add_raw_event(Event, Added, Capacity, Count);
maybe_add_feature_request_full_fidelity(true, #{data := #{include_reason := true}} = Event, _Options, Added, Capacity, Count) ->
    add_raw_event(Event, Added, Capacity, Count);
maybe_add_feature_request_full_fidelity(true, Event, _Options, Added, Capacity, Count) ->
    add_raw_event(ldclient_event:strip_eval_reason(Event), Added, Capacity, Count);
maybe_add_feature_request_full_fidelity(false, _Event, _Options, Added, _Capacity, Count) ->
    {Added, Count}.

-spec maybe_add_index_event(atom(), ldclient_context:context(), non_neg_integer(), pos_integer(), non_neg_integer()) ->
    {[ldclient_event:event()], non_neg_integer()}.
maybe_add_index_event(Tag, Context, Timestamp, Capacity, Count) ->
    case ldclient_context_cache:notice_context(Tag, Context) of
        true -> {[], Count};
        false -> add_index_event(Context, Timestamp, Capacity, Count)
    end.

-spec add_index_event(Context :: ldclient_context:context(), Timestamp :: non_neg_integer(), pos_integer(), non_neg_integer()) ->
    {[ldclient_event:event()], non_neg_integer()}.
add_index_event(Context, Timestamp, Capacity, Count) ->
    IndexEvent = ldclient_event:new_index(Context, Timestamp),
    add_raw_event(IndexEvent, [], Capacity, Count).

-spec should_add_debug_event(ldclient_event:event(), Tag :: atom()) -> boolean().
should_add_debug_event(#{data := #{debugEventsUntilDate := null}}, _Tag) -> false;
should_add_debug_event(#{data := #{debugEventsUntilDate := DebugDate}}, Tag) ->
    LastServerTime = ldclient_event_process_server:get_last_server_time(Tag),
    Now = erlang:system_time(milli_seconds),
    (DebugDate > Now) and (DebugDate >  LastServerTime).

-spec maybe_add_debug_event(boolean(), ldclient_event:event(), options(), [ldclient_event:event()], pos_integer(), non_neg_integer()) ->
    {[ldclient_event:event()], non_neg_integer()}.
maybe_add_debug_event(false, _, _Options, Events, _, Count) -> {Events, Count};
maybe_add_debug_event(true, #{data := EventData} = FeatureEvent,#{include_reasons := true}, Events, Capacity, Count) ->
    add_raw_event(FeatureEvent#{data := EventData#{debug => true}}, Events, Capacity, Count);
maybe_add_debug_event(true, #{data := EventData} = FeatureEvent, _Options, Events, Capacity, Count) ->
    add_raw_event(ldclient_event:strip_eval_reason(FeatureEvent#{data := EventData#{debug => true}}), Events, Capacity, Count).

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

%% @doc Decide whether an incoming event should be shed before it is cast to the
%% event server. Reads the shared depth counter published by the event server.
%% @end
-spec should_shed(Tag :: atom()) -> boolean().
should_shed(Tag) ->
    case persistent_term:get({?COUNTERS_KEY, Tag}, undefined) of
        undefined ->
            false;
        {Ref, Threshold} ->
            try
                counters:get(Ref, 1) >= Threshold
            catch
                _:_ ->
                    %% The counter belongs to a previous incarnation of the
                    %% server (e.g. after a crash). Do not shed; the event
                    %% server will publish a fresh counter.
                    false
            end
    end.

-spec update_counters(state()) -> state().
update_counters(#{counters_ref := Ref, event_count := Count} = State) ->
    counters:put(Ref, 1, Count),
    State.

-spec set_count(state(), non_neg_integer()) -> state().
set_count(State, Count) ->
    update_counters(State#{event_count := Count}).

-spec erase_counters(state()) -> boolean().
erase_counters(#{tag := Tag}) ->
    persistent_term:erase({?COUNTERS_KEY, Tag}).

%%===================================================================
%% Pool scheduling
%%===================================================================

-spec start_flush(state()) -> state().
start_flush(#{summary_event := SummaryEvent, pending_summaries := Pending} = State) ->
    NewPending = case map_size(SummaryEvent) of
        0 -> Pending;
        _ -> Pending ++ [SummaryEvent]
    end,
    drain(State#{summary_event := #{}, pending_summaries := NewPending, flushing := true}).

%% @doc Hand out buffered batches to idle workers until there is no more work or
%% no worker is available. When everything has been dispatched and all workers
%% are idle, the flush window is complete.
%% @end
-spec drain(state()) -> state().
drain(#{event_count := 0, pending_summaries := [], busy_workers := Busy} = State) when map_size(Busy) =:= 0 ->
    complete_flush(State);
drain(#{idle_workers := [Worker|Idle], event_count := Count} = State) when Count > 0 ->
    {Batch, State1} = pop_batch(State),
    {Summary, NewPending} = take_summary(maps:get(pending_summaries, State1)),
    State2 = dispatch(Worker, Batch, Summary, State1#{idle_workers := Idle, pending_summaries := NewPending}),
    drain(State2);
drain(#{idle_workers := [Worker|Idle], event_count := 0, pending_summaries := [Summary|Rest]} = State) ->
    State1 = dispatch(Worker, [], Summary, State#{idle_workers := Idle, pending_summaries := Rest}),
    drain(State1);
drain(State) ->
    %% No idle worker available; wait for `worker_done' or a DOWN message.
    State.

-spec pop_batch(state()) -> {[ldclient_event:event()], state()}.
pop_batch(#{buffer := Buffer, batch_size := BatchSize, event_count := Count} = State) ->
    Batch = ldclient_event_buffer:pop_batch(Buffer, BatchSize),
    {Batch, set_count(State, Count - length(Batch))}.

-spec dispatch(pid(), [ldclient_event:event()], summary_event() | undefined, state()) -> state().
dispatch(Worker, Batch, Summary, #{busy_workers := Busy} = State) ->
    ok = ldclient_event_process_server:send_batch(Worker, self(), Batch, Summary),
    State#{busy_workers := Busy#{Worker => true}}.

-spec take_summary([summary_event()]) -> {summary_event() | undefined, [summary_event()]}.
take_summary([Summary|Rest]) -> {Summary, Rest};
take_summary([]) -> {undefined, []}.

-spec complete_flush(state()) -> state().
complete_flush(#{tag := Tag, flush_interval := FlushInterval, timer_ref := TimerRef, flush_waiters := Waiters} = State) ->
    _ = erlang:cancel_timer(TimerRef),
    NewTimerRef = erlang:send_after(FlushInterval, self(), {flush, Tag}),
    lists:foreach(fun(From) -> gen_server:reply(From, ok) end, Waiters),
    set_count(State#{
        timer_ref := NewTimerRef,
        flush_waiters := [],
        pending_summaries := [],
        summary_event := #{},
        flushing := false
    }, 0).

-spec maybe_scale(state()) -> state().
maybe_scale(#{scale_cooldown_ms := Cooldown, last_scale_ms := Last} = State) ->
    Now = erlang:monotonic_time(millisecond),
    case Cooldown =< (Now - Last) of
        true -> scale(State, Now);
        false -> State
    end.

-spec scale(state(), integer()) -> state().
scale(
    #{
        event_count := Depth,
        idle_workers := Idle,
        busy_workers := Busy,
        min_workers := Min,
        max_workers := Max,
        scale_up_threshold := UpThreshold,
        scale_down_threshold := DownThreshold
    } = State,
    Now
) ->
    Active = length(Idle) + map_size(Busy),
    ShouldScaleUp = (Depth >= UpThreshold) andalso (Active < Max),
    ShouldScaleDown = (Depth =< DownThreshold) andalso (Active > Min) andalso (Idle =/= []),
    case {ShouldScaleUp, ShouldScaleDown} of
        {true, _} -> scale_up(State, Active, Now);
        {false, true} -> scale_down(State, Active, Now);
        _ -> State
    end.

-spec scale_up(state(), non_neg_integer(), integer()) -> state().
scale_up(#{tag := Tag} = State, Active, Now) ->
    telemetry:execute([ldclient, events, pool_scale], #{workers => Active + 1}, #{tag => Tag, direction => up}),
    State1 = start_worker(State#{last_scale_ms := Now}),
    case maps:get(flushing, State1) of
        true -> drain(State1);
        false -> State1
    end.

-spec scale_down(state(), non_neg_integer(), integer()) -> state().
scale_down(#{tag := Tag, idle_workers := [Worker|_], worker_monitors := Monitors} = State, Active, Now) ->
    case find_monitor(Worker, Monitors) of
        undefined -> ok;
        Ref -> _ = erlang:demonitor(Ref, [flush]), ok
    end,
    _ = ldclient_event_worker_sup:stop_worker(Tag, Worker),
    telemetry:execute([ldclient, events, pool_scale], #{workers => Active - 1}, #{tag => Tag, direction => down}),
    remove_worker(State#{last_scale_ms := Now}, find_monitor(Worker, Monitors), Worker).

-spec find_monitor(pid(), #{reference() => pid()}) -> reference() | undefined.
find_monitor(Pid, Monitors) ->
    case [Ref || {Ref, Monitored} <- maps:to_list(Monitors), Monitored =:= Pid] of
        [Ref|_] -> Ref;
        [] -> undefined
    end.

-spec start_workers(state(), non_neg_integer()) -> state().
start_workers(State, 0) ->
    State;
start_workers(State, Remaining) ->
    start_workers(start_worker(State), Remaining - 1).

-spec start_worker(state()) -> state().
start_worker(#{tag := Tag, idle_workers := Idle, worker_monitors := Monitors} = State) ->
    case ldclient_event_worker_sup:start_worker(Tag) of
        {ok, Pid} ->
            Ref = erlang:monitor(process, Pid),
            State#{idle_workers := [Pid|Idle], worker_monitors := Monitors#{Ref => Pid}};
        {error, Reason} ->
            error_logger:error_msg("Could not start event worker for ~p: ~p", [Tag, Reason]),
            State
    end.

-spec ensure_workers(state()) -> state().
ensure_workers(#{min_workers := Min, idle_workers := Idle, busy_workers := Busy} = State) ->
    Active = length(Idle) + map_size(Busy),
    case Min > Active of
        true -> start_workers(State, Min - Active);
        false -> State
    end.

-spec remove_worker(state(), reference(), pid()) -> state().
remove_worker(#{idle_workers := Idle, busy_workers := Busy, worker_monitors := Monitors} = State, Ref, Pid) ->
    State#{
        idle_workers := lists:delete(Pid, Idle),
        busy_workers := maps:remove(Pid, Busy),
        worker_monitors := maps:remove(Ref, Monitors)
    }.
