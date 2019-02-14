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
    % TODO add event buffer capacity
    events := [eld_event:event()],
    summary_event := summary_event(),
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

%% @doc Start listening to streaming events
%%
%% @end
-spec add_event(Tag :: atom(), Event :: eld_event:event()) ->
    ok.
add_event(Tag, Event) when is_atom(Tag) ->
    ServerName = get_local_reg_name(Tag),
    gen_server:call(ServerName, {add_event, Event}).

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
    TimerRef = erlang:send_after(FlushInterval, self(), {flush, Tag}),
    State = #{
        events => [],
        summary_event => #{},
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
handle_call({add_event, Event}, _From, #{events := Events, summary_event := SummaryEvent} = State) ->
    io:format("Adding event: ~p~n", [Event]),
    {NewEvents, NewSummaryEvent} = add_event(Event, Events, SummaryEvent),
    {reply, ok, State#{events := NewEvents, summary_event := NewSummaryEvent}};
handle_call({flush, Tag}, _From, #{events := Events, summary_event := SummaryEvent, flush_interval := FlushInterval, timer_ref := TimerRef} = State) ->
    _ = erlang:cancel_timer(TimerRef),
    io:format("Flushing events: ~p~n", [Events]),
    io:format("Flushing summary event: ~p~n", [SummaryEvent]),
    ok = eld_event_dispatch_server:send_events(Tag, Events, SummaryEvent),
    TimerRef = erlang:send_after(FlushInterval, self(), {flush, Tag}),
    {reply, ok, State#{events := [], summary_event := #{}, timer_ref := TimerRef}}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({flush, Tag}, #{events := Events, summary_event := SummaryEvent, flush_interval := FlushInterval} = State) ->
    io:format("Flushing with interval~n"),
    ok = eld_event_dispatch_server:send_events(Tag, Events, SummaryEvent),
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

-spec add_event(eld_event:event(), [eld_event:event()], summary_event()) ->
    {[eld_event:event()], summary_event()}.
add_event(#{type := feature_request} = Event, Events, SummaryEvent) ->
    NewSummaryEvent = add_feature_request_event(Event, SummaryEvent),
    NewEvents = add_feature_request_full_fidelity(Event, Events),
    {NewEvents, NewSummaryEvent};
add_event(#{type := identify} = Event, Events, SummaryEvent) ->
    {add_non_feature_request_event(Event, Events), SummaryEvent};
add_event(#{type := index} = Event, Events, SummaryEvent) ->
    {add_non_feature_request_event(Event, Events), SummaryEvent};
add_event(#{type := custom} = Event, Events, SummaryEvent) ->
    {add_non_feature_request_event(Event, Events), SummaryEvent}.

-spec add_non_feature_request_event(eld_event:event(), [eld_event:event()]) ->
    [eld_event:event()].
add_non_feature_request_event(Event, Events) ->
    [Event|Events].

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

-spec add_feature_request_full_fidelity(eld_event:event(), [eld_event:event()]) ->
    [eld_event:event()].
add_feature_request_full_fidelity(#{data := #{track_events := true}} = Event, Events) ->
    add_non_feature_request_event(Event, Events);
add_feature_request_full_fidelity(_Event, Events) ->
    Events.

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
