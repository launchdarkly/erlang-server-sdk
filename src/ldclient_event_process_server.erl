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
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% API
-export([
    send_events/3,
    get_last_server_time/1
]).

%% Types
-type state() :: #{
    sdk_key := string(),
    dispatcher := atom(),
    global_private_attributes := ldclient_config:private_attributes(),
    events_uri := string(),
    tag := atom(),
    dispatcher_state := any(),
    last_server_time := integer()
}.

%%===================================================================
%% API
%%===================================================================

%% @doc Send events to LaunchDarkly event server
%%
%% @end
-spec send_events(Tag :: atom(), Events :: [ldclient_event:event()], SummaryEvent :: ldclient_event_server:summary_event()) ->
    ok.
send_events(Tag, Events, SummaryEvent) ->
    ServerName = get_local_reg_name(Tag),
    gen_server:cast(ServerName, {send_events, Events, SummaryEvent}).

-spec get_last_server_time(Tag :: atom()) -> integer().
get_last_server_time(Tag) ->
    ServerName = get_local_reg_name(Tag),
    gen_server:call(ServerName, {get_last_server_time}).

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
    error_logger:info_msg("Starting event processor for ~p with name ~p", [Tag, ServerName]),
    gen_server:start_link({local, ServerName}, ?MODULE, [Tag], []).

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
        last_server_time => 0
    },
    {ok, State}.

%%===================================================================
%% Behavior callbacks
%%===================================================================

-type from() :: {pid(), term()}.
-spec handle_call(Request :: term(), From :: from(), State :: state()) ->
    {reply, Reply :: term(), NewState :: state()} |
    {stop, normal, {error, atom(), term()}, state()}.
handle_call({get_last_server_time}, _From, #{last_server_time := LastServerTime} = State) ->
    {reply, LastServerTime, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(Request :: term(), State :: state()) -> {noreply, NewState :: state()}.
handle_cast({send_events, Events, SummaryEvent},
    #{
        dispatcher := Dispatcher,
        global_private_attributes := GlobalPrivateAttributes,
        events_uri := Uri,
        dispatcher_state := DispatcherState
    } = State) ->
    FormattedSummaryEvent = format_summary_event(SummaryEvent),
    FormattedEvents = format_events(Events, GlobalPrivateAttributes),
    OutputEvents = combine_events(FormattedEvents, FormattedSummaryEvent),
    PayloadId = uuid:get_v4(),
    NewState = case send(Dispatcher, DispatcherState, OutputEvents, PayloadId, Uri) of
       ok ->
           State;
       {ok, Date} ->
           State#{last_server_time => Date};
       {error, temporary, _Reason} ->
           erlang:send_after(1000, self(), {send, OutputEvents, PayloadId}),
           State;
       {error, permanent, Reason} ->
           error_logger:error_msg("Permanent error sending events ~p", [Reason]),
           State
    end,
    {noreply, NewState};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({send, OutputEvents, PayloadId}, State) ->
    #{
        dispatcher := Dispatcher,
        events_uri := Uri,
        dispatcher_state := DispatcherState
    } = State,
    _ = send(Dispatcher, DispatcherState, OutputEvents, PayloadId, Uri),
    {noreply, State};
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

-spec format_events([ldclient_event:event()], ldclient_config:private_attributes()) -> list().
format_events(Events, GlobalPrivateAttributes) ->
    {FormattedEvents, _} = lists:foldl(fun format_event/2, {[], GlobalPrivateAttributes}, Events),
    FormattedEvents.

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
format_event_set_context(<<"feature">>, Context, OutputEvent, _) ->
    OutputEvent#{<<"contextKeys">> => ldclient_context:get_keys_and_kinds(Context)};
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
format_event_set_context(<<"custom">>, Context, OutputEvent, _) ->
    OutputEvent#{<<"contextKeys">> => ldclient_context:get_keys_and_kinds(Context)}.

-spec maybe_set_metric_value(ldclient_event:event(), map()) -> map().
maybe_set_metric_value(#{metric_value := MetricValue}, OutputEvent) ->
    OutputEvent#{<<"metricValue">> => MetricValue};
maybe_set_metric_value(_, OutputEvent) ->
    OutputEvent.

-spec format_summary_event(ldclient_event_server:summary_event()) -> map().
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

-spec send(Dispatcher :: atom(), DispatcherState :: any(), OutputEvents :: list(), PayloadId :: uuid:uuid(), Uri :: string()) ->
    ok | {ok, integer()} | {error, temporary, string()} | {error, permanent, string()}.
send(_, _, [], _, _) ->
    ok;
send(Dispatcher, DispatcherState, OutputEvents, PayloadId, Uri) ->
    JsonEvents = jsx:encode(OutputEvents),
    Dispatcher:send(DispatcherState, JsonEvents, PayloadId, Uri).

-spec get_local_reg_name(Tag :: atom()) -> atom().
get_local_reg_name(Tag) ->
    list_to_atom("ldclient_event_process_server_" ++ atom_to_list(Tag)).
