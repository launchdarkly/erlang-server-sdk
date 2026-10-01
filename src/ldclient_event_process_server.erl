%%-------------------------------------------------------------------
%% @doc Event processor server
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_event_process_server).

-behaviour(gen_server).

%% Supervision
-export([start_link/1, init/1]).

%% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2, format_status/1]).

%% API
-export([
    send_batch/4,
    decommission/1,
    get_last_server_time/1,
    ets_table_name/1
]).

%% Types
-type state() :: #{
    sdk_key := string(),
    dispatcher := atom(),
    global_private_attributes := ldclient_config:private_attributes(),
    events_uri := string(),
    tag := atom(),
    dispatcher_state := any(),
    pending := non_neg_integer(),
    decommission := boolean()
}.

-define(TABLE_PREFIX, "event_process_state").

%% Exponential backoff bounds for retrying transient dispatch failures. A
%% permanent failure is not retried.
-define(RETRY_INITIAL_MS, 1000).
-define(RETRY_MAX_MS, 60000).

%%===================================================================
%% API
%%===================================================================

%% @doc Ask a reporter worker to format and dispatch a batch of events.
%%
%% `Owner' is notified with `{worker_done, self()}' once the batch has been
%% handed to the dispatcher (or scheduled for retry). `SummaryEvent' is either
%% a summary event map or `undefined'.
%% @end
-spec send_batch(Worker :: pid(), Owner :: pid(), Events :: [ldclient_event:event()], SummaryEvent :: ldclient_event_server:summary_event() | undefined) ->
    ok.
send_batch(Worker, Owner, Events, SummaryEvent) ->
    gen_server:cast(Worker, {send_batch, Owner, Events, SummaryEvent}).

%% @doc Ask a worker to stop once it has no outstanding retries. Used when the
%% pool scales down: a worker holding a scheduled retry must not be killed, or
%% the events it is retrying would be lost.
%% @end
-spec decommission(Worker :: pid()) -> ok.
decommission(Worker) ->
    gen_server:cast(Worker, decommission).

-spec get_last_server_time(Tag :: atom()) -> integer().
get_last_server_time(Tag) ->
    TableName = ets_table_name(Tag),
    case ets:info(TableName) of
        undefined ->
            0;
        _ ->
            case ets:lookup(TableName, last_known_server_time) of
                [] -> 0;
                [{last_known_server_time, LastKnownServerTime}] -> LastKnownServerTime
            end
    end.


%%===================================================================
%% Supervision
%%===================================================================

%% @doc Starts a reporter worker.
%%
%% Workers are unregistered: multiple workers exist per tag and the event
%% server addresses them directly by pid.
%% @end
-spec start_link(Tag :: atom()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Tag) ->
    gen_server:start_link(?MODULE, [Tag], []).

-spec init(Args :: term()) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init([Tag]) ->
    SdkKey = ldclient_config:get_value(Tag, sdk_key),
    Dispatcher = ldclient_config:get_value(Tag, events_dispatcher),
    GlobalPrivateAttributes = ldclient_config:get_value(Tag, private_attributes),
    EventsUri = ldclient_config:get_value(Tag, events_uri) ++ "/bulk",
    State = #{
        sdk_key => SdkKey,
        dispatcher => Dispatcher,
        global_private_attributes => GlobalPrivateAttributes,
        events_uri => EventsUri,
        tag => Tag,
        dispatcher_state =>  Dispatcher:init(Tag, SdkKey),
        pending => 0,
        decommission => false
    },
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

-spec handle_cast(Request :: term(), State :: state()) ->
    {noreply, NewState :: state()} | {stop, normal, NewState :: state()}.
handle_cast({send_batch, Owner, Events, SummaryEvent},
    #{global_private_attributes := GlobalPrivateAttributes} = State) ->
    FormattedSummaryEvent = format_summary_event(SummaryEvent),
    FormattedEvents = format_events(Events, GlobalPrivateAttributes),
    OutputEvents = combine_events(FormattedEvents, FormattedSummaryEvent),
    PayloadId = uuid:get_v4(),
    NewState = do_send(OutputEvents, PayloadId, 0, State),
    %% Report the worker as available as soon as the batch has been attempted, so
    %% the pool can keep dispatching; a scheduled retry is tracked separately in
    %% `pending' and delays decommissioning rather than idling the worker.
    _ = Owner ! {worker_done, self()},
    maybe_stop(NewState);
handle_cast(decommission, State) ->
    maybe_stop(State#{decommission := true});
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({send, OutputEvents, PayloadId, Attempt}, #{pending := Pending} = State) ->
    %% The scheduled retry timer has fired.
    NewState = do_send(OutputEvents, PayloadId, Attempt, State#{pending := max(0, Pending - 1)}),
    maybe_stop(NewState);
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: state()) -> term().
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc Redact SDK key from state for logging
%% @private
%%
%% @end
format_status(#{state := State}) ->
    #{state => State#{sdk_key => "[REDACTED]"}};
format_status(Other) ->
    Other.

%%===================================================================
%% Internal functions
%%===================================================================

-spec format_events([ldclient_event:event()], ldclient_config:private_attributes()) -> list().
format_events(Events, GlobalPrivateAttributes) ->
    {FormattedEvents, _} = lists:foldl(fun format_event/2, {[], GlobalPrivateAttributes}, Events),
    lists:reverse(FormattedEvents).

-spec format_event(ldclient_event:event(), {list(), ldclient_config:private_attributes()}) ->
    {FormattedEvents :: list(), GlobalPrivateAttributes :: ldclient_config:private_attributes()}.
format_event(
    #{
        type := feature_request,
        timestamp := Timestamp,
        context := Context,
        data := #{
            debug := Debug,
            key := Key,
            variation := Variation,
            value := Value,
            default := Default,
            version := Version,
            prereq_of := PrereqOf
        }
    } = Event,
    {FormattedEvents, GlobalPrivateAttributes}
) ->
    Kind = if Debug -> <<"debug">>; true -> <<"feature">> end,
    OutputEvent = maybe_set_prereq_of(PrereqOf, #{
        <<"kind">> => Kind,
        <<"creationDate">> => Timestamp,
        <<"key">> => Key,
        <<"variation">> => Variation,
        <<"value">> => Value,
        <<"default">> => Default,
        <<"version">> => Version
    }),
    FormattedEvent = format_event_set_context(Kind, Context, maybe_set_reason(Event, OutputEvent), GlobalPrivateAttributes),
    {[FormattedEvent|FormattedEvents], GlobalPrivateAttributes};
format_event(#{type := identify, timestamp := Timestamp, context := Context}, {FormattedEvents, GlobalPrivateAttributes}) ->
    Kind = <<"identify">>,
    OutputEvent = #{
        <<"kind">> => Kind,
        <<"creationDate">> => Timestamp
    },
    FormattedEvent = format_event_set_context(Kind, Context, OutputEvent, GlobalPrivateAttributes),
    {[FormattedEvent|FormattedEvents], GlobalPrivateAttributes};
format_event(#{type := index, timestamp := Timestamp, context := Context}, {FormattedEvents, GlobalPrivateAttributes}) ->
    Kind = <<"index">>,
    OutputEvent = #{
        <<"kind">> => Kind,
        <<"creationDate">> => Timestamp
    },
    FormattedEvent = format_event_set_context(Kind, Context, OutputEvent, GlobalPrivateAttributes),
    {[FormattedEvent|FormattedEvents], GlobalPrivateAttributes};
format_event(#{type := custom, timestamp := Timestamp, key := Key, context := Context, data := Data} = Event, {FormattedEvents, GlobalPrivateAttributes}) ->
    Kind = <<"custom">>,
    OutputEvent = maybe_set_metric_value(Event, #{
        <<"kind">> => Kind,
        <<"creationDate">> => Timestamp,
        <<"key">> => Key,
        <<"data">> => Data
    }),
    FormattedEvent = format_event_set_context(Kind, Context, OutputEvent, GlobalPrivateAttributes),
    {[FormattedEvent|FormattedEvents], GlobalPrivateAttributes};
format_event(#{type := custom, timestamp := Timestamp, key := Key, context := Context} = Event, {FormattedEvents, GlobalPrivateAttributes}) ->
    Kind = <<"custom">>,
    OutputEvent = maybe_set_metric_value(Event, #{
        <<"kind">> => Kind,
        <<"creationDate">> => Timestamp,
        <<"key">> => Key
    }),
    FormattedEvent = format_event_set_context(Kind, Context, OutputEvent, GlobalPrivateAttributes),
    {[FormattedEvent|FormattedEvents], GlobalPrivateAttributes}.

maybe_set_prereq_of(null, OutputEvent) -> OutputEvent;
maybe_set_prereq_of(PrereqOf, OutputEvent) -> OutputEvent#{<<"prereqOf">> => PrereqOf}.

-spec maybe_set_reason(ldclient_event:event(), #{binary() => any()}) -> #{binary() => any()}.
maybe_set_reason(#{data := #{eval_reason := EvalReason}}, OutputEvent) ->
    OutputEvent#{<<"reason">> => ldclient_eval_reason:format(EvalReason)};
maybe_set_reason(_Event, OutputEvent) ->
    OutputEvent.

-spec format_event_set_context(binary(), ldclient_context:context(), map(), ldclient_config:private_attributes()) -> map().
format_event_set_context(<<"feature">>, Context, OutputEvent, GlobalPrivateAttributes) ->
    OutputEvent#{
        <<"context">> => ldclient_context_filter:format_context_for_event_with_anonyous_redaction(GlobalPrivateAttributes, Context)
    };
format_event_set_context(<<"debug">>, Context, OutputEvent, GlobalPrivateAttributes) ->
    OutputEvent#{
        <<"context">> => ldclient_context_filter:format_context_for_event(GlobalPrivateAttributes, Context)
    };
format_event_set_context(<<"identify">>, Context, OutputEvent, GlobalPrivateAttributes) ->
    OutputEvent#{
        <<"context">> => ldclient_context_filter:format_context_for_event(GlobalPrivateAttributes, Context)
    };
format_event_set_context(<<"index">>, Context, OutputEvent, GlobalPrivateAttributes) ->
    OutputEvent#{
        <<"context">> => ldclient_context_filter:format_context_for_event(GlobalPrivateAttributes, Context)
    };
format_event_set_context(<<"custom">>, Context, OutputEvent, GlobalPrivateAttributes) ->
    OutputEvent#{<<"context">> => ldclient_context_filter:format_context_for_event_with_anonyous_redaction(GlobalPrivateAttributes, Context)}.

-spec maybe_set_metric_value(ldclient_event:event(), map()) -> map().
maybe_set_metric_value(#{metric_value := MetricValue}, OutputEvent) ->
    OutputEvent#{<<"metricValue">> => MetricValue};
maybe_set_metric_value(_, OutputEvent) ->
    OutputEvent.

-spec format_summary_event(ldclient_event_server:summary_event() | undefined) -> map().
format_summary_event(undefined) -> #{};
format_summary_event(SummaryEvent) when map_size(SummaryEvent) == 0 -> #{};
format_summary_event(#{start_date := StartDate, end_date := EndDate, counters := Counters, context_kinds := ContextKinds}) ->
    #{
        <<"kind">> => <<"summary">>,
        <<"startDate">> => StartDate,
        <<"endDate">> => EndDate,
        <<"features">> => format_summary_event_counters(Counters, ContextKinds)
    }.

-spec format_summary_event_counters(ldclient_event_server:counters(), map()) -> map().
format_summary_event_counters(Counters, ContextKinds) ->
    maps:fold(fun(CounterKey, CounterValue, Acc) ->
        format_summary_event_counters(CounterKey, CounterValue, ContextKinds, Acc) end, #{}, Counters).

-spec format_summary_event_counters(ldclient_event_server:counter_key(), ldclient_event_server:counter_value(), map(), map()) ->
    map().
format_summary_event_counters(
    #{
        key := FlagKey,
        variation := Variation,
        version := Version
    },
    #{
        count := Count,
        flag_value := FlagValue,
        flag_default := Default
    },
    ContextKinds,
    Acc
) ->
    FlagMap = maps:get(FlagKey, Acc, #{default => Default, counters => []}),
    CounterWithVersion = maybe_set_unknown(Version, #{
        value => FlagValue,
        count => Count
    }),
    CounterWithVariation = maybe_set_variation(Variation, CounterWithVersion),
    Counter = maybe_add_version(Version, CounterWithVariation),
    NewFlagMap = FlagMap#{
        counters => [Counter|maps:get(counters, FlagMap)],
        contextKinds => maps:get(FlagKey, ContextKinds)},
    Acc#{FlagKey => NewFlagMap}.

maybe_set_unknown(null = _Version, Counter) -> Counter#{unknown => true};
maybe_set_unknown(_Version, Counter) -> Counter.

maybe_set_variation(null, Counter) -> Counter;
maybe_set_variation(Variation, Counter) -> Counter#{variation => Variation}.

maybe_add_version(null, Counter) -> Counter;
maybe_add_version(Version, Counter) -> Counter#{version => Version}.

-spec combine_events(OutputEvents :: list(), OutputSummaryEvent :: map()) -> list().
combine_events([], OutputSummaryEvent) when map_size(OutputSummaryEvent) == 0 -> [];
combine_events(OutputEvents, OutputSummaryEvent) when map_size(OutputSummaryEvent) == 0 -> OutputEvents;
combine_events(OutputEvents, OutputSummaryEvent) -> [OutputSummaryEvent|OutputEvents].

-spec do_send(list(), uuid:uuid(), non_neg_integer(), state()) -> state().
do_send(OutputEvents, PayloadId, Attempt, State) ->
    #{
        dispatcher := Dispatcher,
        events_uri := Uri,
        dispatcher_state := DispatcherState,
        tag := Tag
    } = State,
    case send(Dispatcher, DispatcherState, OutputEvents, PayloadId, Uri) of
        ok ->
            emit_published(Tag, OutputEvents),
            State;
        {ok, Date} ->
            ets:insert(ets_table_name(Tag), {last_known_server_time, Date}),
            emit_published(Tag, OutputEvents),
            State;
        {error, temporary, Reason} ->
            telemetry:execute([ldclient, events, send_error], #{count => 1}, #{tag => Tag, type => temporary}),
            error_logger:warning_msg("Temporary error sending events (~p); retrying with backoff", [Reason]),
            Next = Attempt + 1,
            _ = erlang:send_after(backoff_delay(Next), self(), {send, OutputEvents, PayloadId, Next}),
            maps:update_with(pending, fun(P) -> P + 1 end, State);
        {error, permanent, Reason} ->
            telemetry:execute([ldclient, events, send_error], #{count => 1}, #{tag => Tag, type => permanent}),
            error_logger:error_msg("Permanent error sending events (~p); dropping batch", [Reason]),
            State
    end.

%% @doc Stop the worker if it has been decommissioned and has no outstanding
%% retries left.
%% @end
-spec maybe_stop(state()) -> {noreply, state()} | {stop, normal, state()}.
maybe_stop(#{decommission := true, pending := 0} = State) ->
    {stop, normal, State};
maybe_stop(State) ->
    {noreply, State}.

%% @doc Report how many events were successfully delivered in a batch. Emitted
%% once per successful dispatch (including successful retries) so it can back a
%% "published events" counter metric.
%% @end
-spec emit_published(atom(), list()) -> ok.
emit_published(_Tag, []) ->
    ok;
emit_published(Tag, OutputEvents) ->
    telemetry:execute(
        [ldclient, events, published],
        #{count => length(OutputEvents)},
        #{tag => Tag}
    ).

-spec backoff_delay(pos_integer()) -> non_neg_integer().
backoff_delay(Attempt) ->
    Exponent = min(Attempt - 1, 16),
    Base = min(?RETRY_MAX_MS, ?RETRY_INITIAL_MS bsl Exponent),
    Jitter = 0.5 * Base,
    trunc(Base - (rand:uniform() * Jitter)).

-spec send(Dispatcher :: atom(), DispatcherState :: any(), OutputEvents :: list(), PayloadId :: uuid:uuid(), Uri :: string()) ->
    ok | {ok, integer()} | {error, temporary, string()} | {error, permanent, string()}.
send(_, _, [], _, _) ->
    ok;
send(Dispatcher, DispatcherState, OutputEvents, PayloadId, Uri) ->
    JsonEvents = jsx:encode(OutputEvents),
    Dispatcher:send(DispatcherState, JsonEvents, PayloadId, Uri).

-spec ets_table_name(Tag :: atom()) -> atom().
ets_table_name(Tag) -> list_to_atom(?TABLE_PREFIX ++ atom_to_list(Tag)).
