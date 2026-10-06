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
    send_batch/5,
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

-type send_result() ::
    ok | {ok, integer()} | {error, temporary, string()} | {error, permanent, string()}.

-define(TABLE_PREFIX, "event_process_state").

%% A transient dispatch failure is retried exactly once (matching the SDK's
%% existing contract); a permanent failure is not retried.
-define(RETRY_DELAY_MS, 1000).

%%===================================================================
%% API
%%===================================================================

%% @doc Ask a reporter worker to format and dispatch a batch of events.
%%
%% `Owner' is notified with `{worker_done, self()}' once the batch has been
%% handed to the dispatcher (or scheduled for retry). `SummaryEvent' is either
%% a summary event map or `undefined'. `PayloadId' is chosen by the owner so
%% that a batch re-dispatched after a worker exit keeps the same id.
%% @end
-spec send_batch(Worker :: pid(), Owner :: pid(), Events :: [ldclient_event:event()],
                 SummaryEvent :: ldclient_event_server:summary_event() | undefined, PayloadId :: uuid:uuid()) ->
    ok.
send_batch(Worker, Owner, Events, SummaryEvent, PayloadId) ->
    gen_server:cast(Worker, {send_batch, Owner, Events, SummaryEvent, PayloadId}).

%% @doc Ask a worker to stop once it has no outstanding retries. Used when the
%% pool scales down: a worker holding scheduled retries must not be killed, or
%% the events it is retrying would be lost. Each retry is attempted at most
%% once, so a decommissioned worker lives for at most
%% `pending * (retry delay + request time)'.
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
handle_cast({send_batch, Owner, Events, SummaryEvent, PayloadId},
    #{global_private_attributes := GlobalPrivateAttributes} = State) ->
    FormattedSummaryEvent = format_summary_event(SummaryEvent),
    FormattedEvents = format_events(Events, GlobalPrivateAttributes),
    OutputEvents = combine_events(FormattedEvents, FormattedSummaryEvent),
    StartTime = erlang:monotonic_time(),
    NewState = do_send(OutputEvents, PayloadId, 0, StartTime, State),
    %% Report the worker as available as soon as the batch has been attempted, so
    %% the pool can keep dispatching; a scheduled retry is tracked separately in
    %% `pending' and delays decommissioning rather than idling the worker.
    _ = Owner ! {worker_done, self()},
    stop_if_idle_decommissioned(NewState);
handle_cast(decommission, State) ->
    %% Stop immediately if there is no retry in flight; otherwise stop once every
    %% scheduled retry has been attempted. Retries are never rescheduled, so the
    %% worker's remaining lifetime is bounded even during a sustained outage.
    stop_if_idle_decommissioned(State#{decommission := true});
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({send, OutputEvents, PayloadId, Attempt, StartTime}, #{pending := Pending} = State) ->
    %% The scheduled retry timer has fired.
    NewState = do_send(OutputEvents, PayloadId, Attempt, StartTime, State#{pending := max(0, Pending - 1)}),
    stop_if_idle_decommissioned(NewState);
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

-spec do_send(list(), uuid:uuid(), non_neg_integer(), integer(), state()) -> state().
do_send(OutputEvents, PayloadId, Attempt, StartTime, State) ->
    #{
        dispatcher := Dispatcher,
        events_uri := Uri,
        dispatcher_state := DispatcherState,
        tag := Tag
    } = State,
    {Result, Size} = send(Dispatcher, DispatcherState, OutputEvents, PayloadId, Uri, Tag),
    case Result of
        ok ->
            emit_published(Tag, OutputEvents),
            emit_flush(Tag, OutputEvents, Size, StartTime, accepted),
            State;
        {ok, Date} ->
            %% The table is owned by the event server and may already be gone
            %% while the instance is shutting down.
            _ = (catch ets:insert(ets_table_name(Tag), {last_known_server_time, Date})),
            emit_published(Tag, OutputEvents),
            emit_flush(Tag, OutputEvents, Size, StartTime, accepted),
            State;
        {error, temporary, Reason} when Attempt =:= 0 ->
            telemetry:execute([ldclient, events, send_error], #{count => 1}, #{tag => Tag, type => temporary}),
            error_logger:warning_msg("Temporary error sending events (~p); retrying once", [Reason]),
            _ = erlang:send_after(?RETRY_DELAY_MS, self(), {send, OutputEvents, PayloadId, 1, StartTime}),
            maps:update_with(pending, fun(P) -> P + 1 end, State);
        {error, temporary, Reason} ->
            telemetry:execute([ldclient, events, send_error], #{count => 1}, #{tag => Tag, type => temporary}),
            error_logger:error_msg("Temporary error sending events (~p); retry failed, dropping batch", [Reason]),
            emit_flush(Tag, OutputEvents, Size, StartTime, failed),
            State;
        {error, permanent, Reason} ->
            telemetry:execute([ldclient, events, send_error], #{count => 1}, #{tag => Tag, type => permanent}),
            error_logger:error_msg("Permanent error sending events (~p); dropping batch", [Reason]),
            emit_flush(Tag, OutputEvents, Size, StartTime, failed),
            State
    end.

%% @doc Stop a decommissioned worker that has nothing in flight.
%% @end
-spec stop_if_idle_decommissioned(state()) -> {noreply, state()} | {stop, normal, state()}.
stop_if_idle_decommissioned(#{decommission := true, pending := 0} = State) ->
    {stop, normal, State};
stop_if_idle_decommissioned(State) ->
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

%% @doc Report one delivery attempt per batch, including its retry. The
%% `duration' spans the first attempt through the retry so it can back flush
%% count, batch size, flush duration, sent and failed metrics.
%% @end
-spec emit_flush(atom(), list(), non_neg_integer(), integer(), accepted | failed) -> ok.
emit_flush(_Tag, [], _Size, _StartTime, _Outcome) ->
    ok;
emit_flush(Tag, OutputEvents, Size, StartTime, Outcome) ->
    telemetry:execute(
        [ldclient, events, flush],
        #{
            count => length(OutputEvents),
            size => Size,
            duration => erlang:monotonic_time() - StartTime
        },
        #{tag => Tag, outcome => Outcome}
    ).

-spec send(Dispatcher :: atom(), DispatcherState :: any(), OutputEvents :: list(), PayloadId :: uuid:uuid(), Uri :: string(), Tag :: atom()) ->
    {send_result(), non_neg_integer()}.
send(_, _, [], _, _, _) ->
    {ok, 0};
send(Dispatcher, DispatcherState, OutputEvents, PayloadId, Uri, Tag) ->
    case encode(OutputEvents, Tag) of
        {ok, JsonEvents} ->
            {Dispatcher:send(DispatcherState, JsonEvents, PayloadId, Uri), byte_size(JsonEvents)};
        empty ->
            {ok, 0}
    end.

%% @doc Encode a batch. If a term in some event cannot be represented as JSON
%% (for example a tuple in custom event data, or a map with a key that is not
%% a binary, atom or integer), drop only those events and report them, instead
%% of crashing the worker and losing the whole batch. jsx raises `badarg' for
%% some such terms and `function_clause' for others, so every error is treated
%% as "not JSON". The summary is the one event that must survive: an
%% unencodable application-supplied default value is replaced by `null' there.
%% @end
-spec encode(OutputEvents :: [map()], Tag :: atom()) -> {ok, binary()} | empty.
encode(OutputEvents, Tag) ->
    try
        {ok, jsx:encode(OutputEvents)}
    catch
        error:_ ->
            Sanitized = [sanitize_summary(E) || E <- OutputEvents],
            Encodable = [E || E <- Sanitized, is_encodable(E)],
            Dropped = length(Sanitized) - length(Encodable),
            case Dropped of
                0 -> ok;
                _ ->
                    telemetry:execute([ldclient, events, dropped], #{count => Dropped}, #{tag => Tag, reason => unencodable}),
                    error_logger:error_msg("Dropped ~b events for ~p that could not be encoded as JSON", [Dropped, Tag])
            end,
            case Encodable of
                [] -> empty;
                _ -> {ok, jsx:encode(Encodable)}
            end
    end.

%% A summary carries, per flag, the default value the application passed to the
%% first evaluation of that flag in the window, and for an unknown flag (or a
%% flag with an invalid variation) that default is also the counter's value.
%% Those are the only parts of a summary the application controls; if one is
%% not JSON, send `null' in its place rather than lose every flag's counters.
-spec sanitize_summary(map()) -> map().
sanitize_summary(#{<<"kind">> := <<"summary">>, <<"features">> := Features} = Summary) ->
    Summary#{<<"features">> := maps:map(fun(_FlagKey, #{default := Default, counters := Counters} = Flag) ->
        Flag#{
            default := encodable_or_null(Default),
            counters := [Counter#{value := encodable_or_null(Value)} || #{value := Value} = Counter <- Counters]
        }
    end, Features)};
sanitize_summary(Event) ->
    Event.

-spec encodable_or_null(term()) -> term().
encodable_or_null(Term) ->
    case is_encodable(Term) of
        true -> Term;
        false -> null
    end.

-spec is_encodable(term()) -> boolean().
is_encodable(Term) ->
    try
        _ = jsx:encode(Term),
        true
    catch
        error:_ -> false
    end.

-spec ets_table_name(Tag :: atom()) -> atom().
ets_table_name(Tag) -> list_to_atom(?TABLE_PREFIX ++ atom_to_list(Tag)).
