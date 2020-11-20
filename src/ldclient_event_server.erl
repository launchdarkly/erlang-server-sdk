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
    events := [ldclient_event:event()],
    summary_event := summary_event(),
    capacity := pos_integer(),
    inline_users => boolean(),
    flush_interval := pos_integer(),
    timer_ref := reference(),
    offline := boolean()
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

%%===================================================================
%% API
%%===================================================================

%% @doc Add an event to the buffer
%%
%% Events are not sent immediately. They are kept in buffer up to configured
%% size and flushed at configured interval.
%% @end
-spec add_event(Tag :: atom(), Event :: ldclient_event:event(), Options :: options()) ->
    ok.
add_event(Tag, Event, Options) when is_atom(Tag) ->
    ServerName = get_local_reg_name(Tag),
    gen_server:call(ServerName, {add_event, Event, Tag, Options}).

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
    FlushInterval = ldclient_settings:get_value(Tag, events_flush_interval),
    Capacity = ldclient_settings:get_value(Tag, events_capacity),
    InlineUsers = ldclient_settings:get_value(Tag, inline_users_in_events),
    TimerRef = erlang:send_after(FlushInterval, self(), {flush, Tag}),
    OfflineMode = ldclient:is_offline(Tag),
    % Need to trap exit so supervisor:terminate_child calls terminate callback
    process_flag(trap_exit, true),
    State = #{
        events => [],
        summary_event => #{},
        capacity => Capacity,
        inline_users => InlineUsers,
        flush_interval => FlushInterval,
        timer_ref => TimerRef,
        offline => OfflineMode
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
handle_call({add_event, Event, Tag, Options}, _From, #{events := Events, summary_event := SummaryEvent, capacity := Capacity, inline_users := InlineUsers} = State) ->
    {NewEvents, NewSummaryEvent} = add_event(Tag, Event, Options, Events, SummaryEvent, Capacity, InlineUsers),
    {reply, ok, State#{events := NewEvents, summary_event := NewSummaryEvent}};
handle_call({flush, Tag}, _From, #{events := Events, summary_event := SummaryEvent, flush_interval := FlushInterval, timer_ref := TimerRef} = State) ->
    _ = erlang:cancel_timer(TimerRef),
    ok = ldclient_event_process_server:send_events(Tag, Events, SummaryEvent),
    NewTimerRef = erlang:send_after(FlushInterval, self(), {flush, Tag}),
    {reply, ok, State#{events := [], summary_event := #{}, timer_ref := NewTimerRef}}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({flush, Tag}, #{events := Events, summary_event := SummaryEvent, flush_interval := FlushInterval} = State) ->
    ok = ldclient_event_process_server:send_events(Tag, Events, SummaryEvent),
    TimerRef = erlang:send_after(FlushInterval, self(), {flush, Tag}),
    {noreply, State#{events := [], summary_event := #{}, timer_ref := TimerRef}};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: state()) -> term().
terminate(Reason, #{timer_ref := TimerRef} = _State) ->
    error_logger:info_msg("Terminating event service, reason: ~p", [Reason]),
    _ = erlang:cancel_timer(TimerRef),
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%===================================================================
%% Internal functions
%%===================================================================

-spec add_event(atom(), ldclient_event:event(), options(), [ldclient_event:event()], summary_event(), pos_integer(), boolean()) ->
    {[ldclient_event:event()], summary_event()}.
add_event(Tag, #{type := feature_request, user := User, timestamp := Timestamp} = Event, Options, Events, SummaryEvent, Capacity, InlineUsers) ->
    AddFull = should_add_full_event(Event),
    AddDebug = should_add_debug_event(Event),
    AddIndex = not (AddFull and InlineUsers),
    NewSummaryEvent = add_feature_request_event(Event, SummaryEvent),
    EventsWithIndex = maybe_add_index_event(Tag, User, Timestamp, Events, Capacity, AddIndex),
    EventsWithFeature = maybe_add_feature_request_full_fidelity(AddFull, Event, Options, EventsWithIndex, Capacity),
    NewEvents = maybe_add_debug_event(AddDebug, Event, EventsWithFeature, Capacity),
    {NewEvents, NewSummaryEvent};
add_event(_Tag, #{type := identify} = Event, _Options, Events, SummaryEvent, Capacity, _InlineUsers) ->
    {add_raw_event(Event, Events, Capacity), SummaryEvent};
add_event(Tag, #{type := custom, user := User, timestamp := Timestamp} = Event, _Options, Events, SummaryEvent, Capacity, InlineUsers) ->
    AddIndex = not InlineUsers,
    EventsWithIndex = maybe_add_index_event(Tag, User, Timestamp, Events, Capacity, AddIndex),
    {add_raw_event(Event, EventsWithIndex, Capacity), SummaryEvent}.

-spec add_raw_event(ldclient_event:event(), [ldclient_event:event()], pos_integer()) ->
    [ldclient_event:event()].
add_raw_event(Event, Events, Capacity) when length(Events) < Capacity ->
    [Event|Events];
add_raw_event(_, Events, _) ->
    error_logger:warning_msg("Exceeded event queue capacity. Increase capacity to avoid dropping events."),
    Events.

-spec add_feature_request_event(ldclient_event:event(), summary_event()) ->
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

-spec should_add_full_event(ldclient_event:event()) -> boolean().
should_add_full_event(#{data := #{track_events := true}}) -> true;
should_add_full_event(_) -> false.

-spec maybe_add_feature_request_full_fidelity(boolean(), ldclient_event:event(), options(), [ldclient_event:event()], pos_integer()) ->
    [ldclient_event:event()].
maybe_add_feature_request_full_fidelity(true, Event, #{include_reasons := true}, Events, Capacity) ->
    add_raw_event(Event, Events, Capacity);
maybe_add_feature_request_full_fidelity(true, #{data := #{include_reason := true}} = Event, _Options, Events, Capacity) ->
    add_raw_event(Event, Events, Capacity);
maybe_add_feature_request_full_fidelity(true, Event, _Options, Events, Capacity) ->
    add_raw_event(ldclient_event:strip_eval_reason(Event), Events, Capacity);
maybe_add_feature_request_full_fidelity(false, _Event, _Options, Events, _Capacity) ->
    Events.

-spec maybe_add_index_event(atom(), ldclient_user:user(), non_neg_integer(), [ldclient_event:event()], pos_integer(), boolean()) ->
    [ldclient_event:event()].
maybe_add_index_event(_, _, _, Events, _, false) -> Events;
maybe_add_index_event(Tag, User, Timestamp, Events, Capacity, true) ->
    case ldclient_user_cache:notice_user(Tag, User) of
        true -> Events;
        false -> add_index_event(User, Timestamp, Events, Capacity)
    end.

-spec add_index_event(User :: ldclient_user:user(), Timestamp :: non_neg_integer(), Events :: [ldclient_event:event()], pos_integer()) ->
    [ldclient_event:event()].
add_index_event(User, Timestamp, Events, Capacity) ->
    IndexEvent = ldclient_event:new_index(User, Timestamp),
    add_raw_event(IndexEvent, Events, Capacity).

-spec should_add_debug_event(ldclient_event:event()) -> boolean().
should_add_debug_event(#{data := #{debug_events_until_date := null}}) -> false;
should_add_debug_event(#{data := #{debug_events_until_date := DebugDate}}) ->
    Now = erlang:system_time(milli_seconds),
    DebugDate > Now.

-spec maybe_add_debug_event(boolean(), ldclient_event:event(), [ldclient_event:event()], pos_integer()) ->
    [ldclient_event:event()].
maybe_add_debug_event(false, _, Events, _) -> Events;
maybe_add_debug_event(true, #{data := EventData} = FeatureEvent, Events, Capacity) ->
    add_raw_event(FeatureEvent#{data := EventData#{debug => true}}, Events, Capacity).

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
