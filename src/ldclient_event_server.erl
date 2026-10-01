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
    events := [ldclient_event:event()],
    event_count := non_neg_integer(),
    counters_ref := counters:counters_ref(),
    summary_event := summary_event(),
    capacity := pos_integer(),
    shed_threshold := pos_integer(),
    flush_interval := pos_integer(),
    timer_ref := reference(),
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
    TimerRef = erlang:send_after(FlushInterval, self(), {flush, Tag}),
    OfflineMode = ldclient:is_offline(Tag),
    SendEvents = ldclient_config:get_value(Tag, send_events),
    CountersRef = counters:new(1, [write_concurrency]),
    persistent_term:put({?COUNTERS_KEY, Tag}, {CountersRef, ShedThreshold}),
    % Need to trap exit so supervisor:terminate_child calls terminate callback
    process_flag(trap_exit, true),
    State = #{
        tag => Tag,
        events => [],
        event_count => 0,
        counters_ref => CountersRef,
        summary_event => #{},
        capacity => Capacity,
        shed_threshold => ShedThreshold,
        flush_interval => FlushInterval,
        timer_ref => TimerRef,
        offline => OfflineMode,
        send_events => SendEvents
    },
    {ok, State}.

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
handle_call({flush, Tag}, _From, #{events := Events, summary_event := SummaryEvent, flush_interval := FlushInterval, timer_ref := TimerRef} = State) ->
    _ = erlang:cancel_timer(TimerRef),
    ok = ldclient_event_process_server:send_events(Tag, Events, SummaryEvent),
    NewTimerRef = erlang:send_after(FlushInterval, self(), {flush, Tag}),
    {reply, ok, reset_counters(State#{events := [], summary_event := #{}, timer_ref := NewTimerRef})}.
handle_cast({add_event, Event, Tag, Options}, #{events := Events, event_count := Count, summary_event := SummaryEvent, capacity := Capacity} = State) ->
    {NewEvents, NewSummaryEvent, NewCount} = add_event(Tag, Event, Options, Events, SummaryEvent, Count, Capacity),
    {noreply, update_counters(State#{events := NewEvents, event_count := NewCount, summary_event := NewSummaryEvent})};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({flush, Tag}, #{events := Events, summary_event := SummaryEvent, flush_interval := FlushInterval} = State) ->
    ok = ldclient_event_process_server:send_events(Tag, Events, SummaryEvent),
    TimerRef = erlang:send_after(FlushInterval, self(), {flush, Tag}),
    {noreply, reset_counters(State#{events := [], summary_event := #{}, timer_ref := TimerRef})};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: state()) -> term().
terminate(Reason, #{timer_ref := TimerRef} = State) ->
    error_logger:info_msg("Terminating event service, reason: ~p", [Reason]),
    _ = erlang:cancel_timer(TimerRef),
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
    Events :: [ldclient_event:event()],
    SummaryEvent :: summary_event(),
    Count :: non_neg_integer(),
    Capacity :: pos_integer()
) ->
    {[ldclient_event:event()], summary_event(), non_neg_integer()}.
add_event(Tag, #{type := feature_request, context := Context, timestamp := Timestamp} = Event, Options, Events, SummaryEvent, Count, Capacity) ->
    AddFull = should_add_full_event(Event),
    AddDebug = should_add_debug_event(Event, Tag),
    NewSummaryEvent = add_feature_request_event(Event, SummaryEvent),
    {EventsWithIndex, Count1} = maybe_add_index_event(Tag, Context, Timestamp, Events, Capacity, Count),
    {EventsWithFeature, Count2} = maybe_add_feature_request_full_fidelity(AddFull, Event, Options, EventsWithIndex, Capacity, Count1),
    {NewEvents, Count3} = maybe_add_debug_event(AddDebug, Event, Options, EventsWithFeature, Capacity, Count2),
    {NewEvents, NewSummaryEvent, Count3};
add_event(Tag, #{type := identify, context := Context} = Event, _Options, Events, SummaryEvent, Count, Capacity) ->
    % Notice the context, but do not conditionally add the index event.
    ldclient_context_cache:notice_context(Tag, Context),
    {NewEvents, NewCount} = add_raw_event(Event, Events, Capacity, Count),
    {NewEvents, SummaryEvent, NewCount};
add_event(Tag, #{type := custom, context := Context, timestamp := Timestamp} = Event, _Options, Events, SummaryEvent, Count, Capacity) ->
    {EventsWithIndex, Count1} = maybe_add_index_event(Tag, Context, Timestamp, Events, Capacity, Count),
    {NewEvents, Count2} = add_raw_event(Event, EventsWithIndex, Capacity, Count1),
    {NewEvents, SummaryEvent, Count2}.

-spec add_raw_event(ldclient_event:event(), [ldclient_event:event()], pos_integer(), non_neg_integer()) ->
    {[ldclient_event:event()], non_neg_integer()}.
add_raw_event(Event, Events, Capacity, Count) when Count < Capacity ->
    {[Event|Events], Count + 1};
add_raw_event(_, Events, _Capacity, Count) ->
    error_logger:warning_msg("Exceeded event queue capacity. Increase capacity to avoid dropping events."),
    {Events, Count}.

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
maybe_add_feature_request_full_fidelity(true, Event, #{include_reasons := true}, Events, Capacity, Count) ->
    add_raw_event(Event, Events, Capacity, Count);
maybe_add_feature_request_full_fidelity(true, #{data := #{include_reason := true}} = Event, _Options, Events, Capacity, Count) ->
    add_raw_event(Event, Events, Capacity, Count);
maybe_add_feature_request_full_fidelity(true, Event, _Options, Events, Capacity, Count) ->
    add_raw_event(ldclient_event:strip_eval_reason(Event), Events, Capacity, Count);
maybe_add_feature_request_full_fidelity(false, _Event, _Options, Events, _Capacity, Count) ->
    {Events, Count}.

-spec maybe_add_index_event(atom(), ldclient_context:context(), non_neg_integer(), [ldclient_event:event()], pos_integer(), non_neg_integer()) ->
    {[ldclient_event:event()], non_neg_integer()}.
maybe_add_index_event(Tag, Context, Timestamp, Events, Capacity, Count) ->
    case ldclient_context_cache:notice_context(Tag, Context) of
        true -> {Events, Count};
        false -> add_index_event(Context, Timestamp, Events, Capacity, Count)
    end.

-spec add_index_event(Context :: ldclient_context:context(), Timestamp :: non_neg_integer(), Events :: [ldclient_event:event()], pos_integer(), non_neg_integer()) ->
    {[ldclient_event:event()], non_neg_integer()}.
add_index_event(Context, Timestamp, Events, Capacity, Count) ->
    IndexEvent = ldclient_event:new_index(Context, Timestamp),
    add_raw_event(IndexEvent, Events, Capacity, Count).

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

-spec reset_counters(state()) -> state().
reset_counters(State) ->
    update_counters(State#{event_count := 0}).

-spec erase_counters(state()) -> ok.
erase_counters(#{tag := Tag}) ->
    persistent_term:erase({?COUNTERS_KEY, Tag}).
