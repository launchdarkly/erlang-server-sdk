%%-------------------------------------------------------------------
%% @doc Event server
%%
%% @end
%%-------------------------------------------------------------------
-module(eld_event_server).

-behaviour(gen_server).

%% Supervision
-export([start_link/1, init/1]).

%% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% API
-export([add_event/2, flush/1]).

-type state() :: #{
    events := [eld_event:event()],
    summary_event := summary_event(),
    capacity := pos_integer(),
    flush_interval := pos_integer(),
    timer_ref := reference()
}.

-type summary_event() :: #{} | #{
    counters := counters(),
    start_date := non_neg_integer(),
    end_date := non_neg_integer()
}.

-type counters() :: #{
    counter_key() => counter_value()
}.

-type counter_key() :: #{
    key := eld_flag:key(),
    variation := non_neg_integer(),
    version := non_neg_integer()
}.

-type counter_value() :: #{
    count := non_neg_integer(),
    flag_value := eld_flag:variation_value(),
    flag_default := term()
}.

-export_type([summary_event/0]).
-export_type([counters/0]).
-export_type([counter_key/0]).
-export_type([counter_value/0]).

%%===================================================================
%% API
%%===================================================================

%% @doc Add an event to the buffer
%%
%% Events are not sent immediately. They are kept in buffer up to configured
%% size and flushed at configured interval.
%% @end
-spec add_event(Tag :: atom(), Event :: eld_event:event()) ->
    ok.
add_event(Tag, Event) when is_atom(Tag) ->
    ServerName = get_local_reg_name(Tag),
    gen_server:call(ServerName, {add_event, Event, Tag}).

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
    io:format("Starting event storage server with name: ~p~n", [ServerName]),
    gen_server:start_link({local, ServerName}, ?MODULE, [Tag], []).

-spec init(Args :: term()) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init([Tag]) ->
    FlushInterval = eld_settings:get_value(Tag, events_flush_interval),
    Capacity = eld_settings:get_value(Tag, events_capacity),
    TimerRef = erlang:send_after(FlushInterval, self(), {flush, Tag}),
    State = #{
        events => [],
        summary_event => #{},
        capacity => Capacity,
        flush_interval => FlushInterval,
        timer_ref => TimerRef
    },
    {ok, State}.

%%===================================================================
%% Behavior callbacks
%%===================================================================

-type from() :: {pid(), term()}.
-spec handle_call(Request :: term(), From :: from(), State :: state()) ->
    {reply, Reply :: term(), NewState :: state()} |
    {stop, normal, {error, atom(), term()}, state()}.
handle_call({add_event, Event, Tag}, _From, #{events := Events, summary_event := SummaryEvent, capacity := Capacity} = State) ->
    io:format("Adding event: ~p~n", [Event]),
    {NewEvents, NewSummaryEvent} = add_event(Tag, Event, Events, SummaryEvent, Capacity),
    {reply, ok, State#{events := NewEvents, summary_event := NewSummaryEvent}};
handle_call({flush, Tag}, _From, #{events := Events, summary_event := SummaryEvent, flush_interval := FlushInterval, timer_ref := TimerRef} = State) ->
    _ = erlang:cancel_timer(TimerRef),
    io:format("Flushing events: ~p~n", [Events]),
    io:format("Flushing summary event: ~p~n", [SummaryEvent]),
    ok = eld_event_process_server:send_events(Tag, Events, SummaryEvent),
    NewTimerRef = erlang:send_after(FlushInterval, self(), {flush, Tag}),
    {reply, ok, State#{events := [], summary_event := #{}, timer_ref := NewTimerRef}}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({flush, Tag}, #{events := Events, summary_event := SummaryEvent, flush_interval := FlushInterval} = State) ->
    io:format("Flushing with interval~n"),
    ok = eld_event_process_server:send_events(Tag, Events, SummaryEvent),
    TimerRef = erlang:send_after(FlushInterval, self(), {flush, Tag}),
    {noreply, State#{events := [], summary_event := #{}, timer_ref := TimerRef}};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: state()) -> term().
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%===================================================================
%% Internal functions
%%===================================================================

-spec add_event(atom(), eld_event:event(), [eld_event:event()], summary_event(), pos_integer()) ->
    {[eld_event:event()], summary_event()}.
add_event(Tag, #{type := feature_request, user := User, timestamp := Timestamp} = Event, Events, SummaryEvent, Capacity) ->
    NewSummaryEvent = add_feature_request_event(Event, SummaryEvent),
    EventsWithIndex = maybe_add_index_event(Tag, User, Timestamp, Events, Capacity),
    EventsWithDebug = maybe_add_debug_event(Event, EventsWithIndex, Capacity),
    NewEvents = maybe_add_feature_request_full_fidelity(Event, EventsWithDebug, Capacity),
    {NewEvents, NewSummaryEvent};
add_event(_Tag, #{type := identify} = Event, Events, SummaryEvent, Capacity) ->
    {add_raw_event(Event, Events, Capacity), SummaryEvent};
add_event(Tag, #{type := custom, user := User, timestamp := Timestamp} = Event, Events, SummaryEvent, Capacity) ->
    EventsWithIndex = maybe_add_index_event(Tag, User, Timestamp, Events, Capacity),
    {add_raw_event(Event, EventsWithIndex, Capacity), SummaryEvent}.

-spec add_raw_event(eld_event:event(), [eld_event:event()], pos_integer()) ->
    [eld_event:event()].
add_raw_event(Event, Events, Capacity) when length(Events) < Capacity ->
    [Event|Events];
add_raw_event(_, Events, _) ->
    error_logger:warning_msg("Exceeded event queue capacity. Increase capacity to avoid dropping events."),
    Events.

-spec add_feature_request_event(eld_event:event(), summary_event()) ->
    summary_event().
add_feature_request_event(
    #{
        timestamp := Timestamp,
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
        counters => #{SummaryEventKey => SummaryEventValue}
    };
add_feature_request_event(
    #{
        timestamp := Timestamp,
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
        counters := SummaryEventCounters
    } = SummaryEvent
) ->
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
        end_date => NewEndDate
    }.

-spec maybe_add_feature_request_full_fidelity(eld_event:event(), [eld_event:event()], pos_integer()) ->
    [eld_event:event()].
maybe_add_feature_request_full_fidelity(#{data := #{track_events := true}} = Event, Events, Capacity) ->
    add_raw_event(Event, Events, Capacity);
maybe_add_feature_request_full_fidelity(_Event, Events, _Capacity) ->
    Events.

-spec maybe_add_index_event(atom(), eld_user:user(), non_neg_integer(), [eld_event:event()], pos_integer()) ->
    [eld_event:event()].
maybe_add_index_event(Tag, User, Timestamp, Events, Capacity) ->
    case eld_user_cache:notice_user(Tag, User) of
        true -> Events;
        false -> add_index_event(User, Timestamp, Events, Capacity)
    end.

-spec add_index_event(User :: eld_user:user(), Timestamp :: non_neg_integer(), Events :: [eld_event:event()], pos_integer()) ->
    [eld_event:event()].
add_index_event(User, Timestamp, Events, Capacity) ->
    IndexEvent = eld_event:new_index(User, Timestamp),
    add_raw_event(IndexEvent, Events, Capacity).

-spec maybe_add_debug_event(eld_event:event(), [eld_event:event()], pos_integer()) ->
    [eld_event:event()].
maybe_add_debug_event(#{data := #{debug_events_until_date := null}}, Events, _) -> Events;
maybe_add_debug_event(#{data := #{debug_events_until_date := DebugDate} = EventData} = FeatureEvent, Events, Capacity) ->
    Now = erlang:system_time(milli_seconds),
    case DebugDate > Now of
        true -> add_raw_event(FeatureEvent#{data := EventData#{debug => true}}, Events, Capacity);
        false -> Events
    end.

-spec create_summary_event_key(eld_flag:key(), eld_flag:variation(), eld_flag:version()) ->
    counter_key().
create_summary_event_key(Key, Variation, Version) ->
    #{
        key => Key,
        variation => Variation,
        version => Version
    }.

-spec create_summary_event_value(eld_flag:variation_value(), term()) ->
    counter_value().
create_summary_event_value(Value, Default) ->
    #{
        count => 1,
        flag_value => Value,
        flag_default => Default
    }.

-spec get_local_reg_name(Tag :: atom()) -> atom().
get_local_reg_name(Tag) ->
    list_to_atom("eld_event_server_" ++ atom_to_list(Tag)).
