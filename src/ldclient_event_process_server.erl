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
    inline_users := boolean(),
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
    InlineUsers = ldclient_config:get_value(Tag, inline_users_in_events),
    GlobalPrivateAttributes = ldclient_config:get_value(Tag, private_attributes),
    EventsUri = ldclient_config:get_value(Tag, events_uri) ++ "/bulk",
    State = #{
        sdk_key => SdkKey,
        dispatcher => Dispatcher,
        inline_users => InlineUsers,
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
        inline_users := InlineUsers,
        global_private_attributes := GlobalPrivateAttributes,
        events_uri := Uri,
        dispatcher_state := DispatcherState
    } = State) ->
    FormattedSummaryEvent = format_summary_event(SummaryEvent),
    FormattedEvents = format_events(Events, InlineUsers, GlobalPrivateAttributes),
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

-spec context_kind(ldclient_user:user()) -> binary().
context_kind(User) ->
    case ldclient_user:get(anonymous, User) of
        true -> <<"anonymousUser">>;
        _ -> <<"user">>
    end.

-spec format_events([ldclient_event:event()], boolean(), ldclient_config:private_attributes()) -> list().
format_events(Events, InlineUsers, GlobalPrivateAttributes) ->
    {FormattedEvents, _, _} = lists:foldl(fun format_event/2, {[], InlineUsers, GlobalPrivateAttributes}, Events),
    FormattedEvents.

-spec format_event(ldclient_event:event(), {list(), boolean(), ldclient_config:private_attributes()}) ->
    {list(), boolean(), ldclient_config:private_attributes()}.
format_event(
    #{
        type := feature_request,
        timestamp := Timestamp,
        user := User,
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
    {FormattedEvents, InlineUsers, GlobalPrivateAttributes}
) ->
    Kind = if Debug -> <<"debug">>; true -> <<"feature">> end,
    OutputEvent = maybe_set_prereq_of(PrereqOf, maybe_set_context_kind(Event, #{
        <<"kind">> => Kind,
        <<"creationDate">> => Timestamp,
        <<"key">> => Key,
        <<"variation">> => Variation,
        <<"value">> => Value,
        <<"default">> => Default,
        <<"version">> => Version
    })),
    FormattedEvent = format_event_set_user(Kind, User, maybe_set_reason(Event, OutputEvent), InlineUsers, GlobalPrivateAttributes),
    {[FormattedEvent|FormattedEvents], InlineUsers, GlobalPrivateAttributes};
format_event(#{type := identify, timestamp := Timestamp, user := User}, {FormattedEvents, InlineUsers, GlobalPrivateAttributes}) ->
    Kind = <<"identify">>,
    OutputEvent = #{
        <<"kind">> => Kind,
        <<"creationDate">> => Timestamp
    },
    FormattedEvent = format_event_set_user(Kind, User, OutputEvent, InlineUsers, GlobalPrivateAttributes),
    {[FormattedEvent|FormattedEvents], InlineUsers, GlobalPrivateAttributes};
format_event(#{type := index, timestamp := Timestamp, user := User}, {FormattedEvents, InlineUsers, GlobalPrivateAttributes}) ->
    Kind = <<"index">>,
    OutputEvent = #{
        <<"kind">> => Kind,
        <<"creationDate">> => Timestamp
    },
    FormattedEvent = format_event_set_user(Kind, User, OutputEvent, InlineUsers, GlobalPrivateAttributes),
    {[FormattedEvent|FormattedEvents], InlineUsers, GlobalPrivateAttributes};
format_event(#{type := custom, timestamp := Timestamp, key := Key, user := User, data := Data} = Event, {FormattedEvents, InlineUsers, GlobalPrivateAttributes}) ->
    Kind = <<"custom">>,
    OutputEvent = maybe_set_context_kind(Event, maybe_set_metric_value(Event, #{
        <<"kind">> => Kind,
        <<"creationDate">> => Timestamp,
        <<"key">> => Key,
        <<"data">> => Data
    })),
    FormattedEvent = format_event_set_user(Kind, User, OutputEvent, InlineUsers, GlobalPrivateAttributes),
    {[FormattedEvent|FormattedEvents], InlineUsers, GlobalPrivateAttributes};
format_event(#{type := custom, timestamp := Timestamp, key := Key, user := User} = Event, {FormattedEvents, InlineUsers, GlobalPrivateAttributes}) ->
    Kind = <<"custom">>,
    OutputEvent = maybe_set_context_kind(Event, maybe_set_metric_value(Event, #{
        <<"kind">> => Kind,
        <<"creationDate">> => Timestamp,
        <<"key">> => Key
    })),
    FormattedEvent = format_event_set_user(Kind, User, OutputEvent, InlineUsers, GlobalPrivateAttributes),
    {[FormattedEvent|FormattedEvents], InlineUsers, GlobalPrivateAttributes};
format_event(#{
    type := alias, 
    timestamp := Timestamp, 
    user := User, 
    previous_user := PreviousUser}, 
    {FormattedEvents, InlineUsers, GlobalPrivateAttributes}) ->
    Kind = <<"alias">>,
    OutputEvent = #{
        <<"kind">> => Kind,
        <<"creationDate">> => Timestamp,
        <<"key">> => ldclient_user:get(key, User),
        <<"contextKind">> => context_kind(User),
        <<"previousKey">> => ldclient_user:get(key, PreviousUser),
        <<"previousContextKind">> => context_kind(PreviousUser)
    },
    {[OutputEvent|FormattedEvents], InlineUsers, GlobalPrivateAttributes}.

maybe_set_prereq_of(null, OutputEvent) -> OutputEvent;
maybe_set_prereq_of(PrereqOf, OutputEvent) -> OutputEvent#{<<"prereqOf">> => PrereqOf}.

-spec maybe_set_reason(ldclient_event:event(), #{binary() => any()}) -> #{binary() => any()}.
maybe_set_reason(#{data := #{eval_reason := EvalReason}}, OutputEvent) ->
    OutputEvent#{<<"reason">> => ldclient_eval_reason:format(EvalReason)};
maybe_set_reason(_Event, OutputEvent) ->
    OutputEvent.

-spec maybe_set_private_attrs(ScrubbedUser :: map(), ScrubbedAttrNames :: ldclient:private_attributes()) -> map().
maybe_set_private_attrs(ScrubbedUser, [] = _ScrubbedAttrNames) -> ScrubbedUser;
maybe_set_private_attrs(ScrubbedUser, ScrubbedAttrNames) ->
    ScrubbedUser#{<<"privateAttrs">> => ScrubbedAttrNames}.

-spec format_event_set_user(binary(), ldclient_user:user(), map(), boolean(), ldclient_config:private_attributes()) -> map().
format_event_set_user(<<"feature">>, User, OutputEvent, true, GlobalPrivateAttributes) ->
    {ScrubbedUser, ScrubbedAttrNames} = ldclient_user:scrub(User, GlobalPrivateAttributes),
    OutputEvent#{
        <<"user">> => ldclient_user:event_format(maybe_set_private_attrs(ScrubbedUser, ScrubbedAttrNames))
    };
format_event_set_user(<<"feature">>, #{key := UserKey}, OutputEvent, false, _) ->
    OutputEvent#{<<"userKey">> => UserKey};
format_event_set_user(<<"feature">>, _User, OutputEvent, _, _) ->
    % User has no key
    OutputEvent#{<<"userKey">> => null};
format_event_set_user(<<"debug">>, User, OutputEvent, _, GlobalPrivateAttributes) ->
    {ScrubbedUser, ScrubbedAttrNames} = ldclient_user:scrub(User, GlobalPrivateAttributes),
    OutputEvent#{
        <<"user">> => ldclient_user:event_format(maybe_set_private_attrs(ScrubbedUser, ScrubbedAttrNames))
    };
format_event_set_user(<<"identify">>, #{key := UserKey} = User, OutputEvent, _, GlobalPrivateAttributes) ->
    {ScrubbedUser, ScrubbedAttrNames} = ldclient_user:scrub(User, GlobalPrivateAttributes),
    OutputEvent#{
        <<"key">> => UserKey,
        <<"user">> => ldclient_user:event_format(maybe_set_private_attrs(ScrubbedUser, ScrubbedAttrNames))
    };
format_event_set_user(<<"index">>, User, OutputEvent, _, GlobalPrivateAttributes) ->
    {ScrubbedUser, ScrubbedAttrNames} = ldclient_user:scrub(User, GlobalPrivateAttributes),
    OutputEvent#{
        <<"user">> => ldclient_user:event_format(maybe_set_private_attrs(ScrubbedUser, ScrubbedAttrNames))
    };
format_event_set_user(<<"custom">>, User, OutputEvent, true, GlobalPrivateAttributes) ->
    {ScrubbedUser, ScrubbedAttrNames} = ldclient_user:scrub(User, GlobalPrivateAttributes),
    OutputEvent#{
        <<"user">> => ldclient_user:event_format(maybe_set_private_attrs(ScrubbedUser, ScrubbedAttrNames))
    };
format_event_set_user(<<"custom">>, #{key := UserKey}, OutputEvent, false, _) ->
    OutputEvent#{<<"userKey">> => UserKey}.

-spec maybe_set_metric_value(ldclient_event:event(), map()) -> map().
maybe_set_metric_value(#{metric_value := MetricValue}, OutputEvent) ->
    OutputEvent#{<<"metricValue">> => MetricValue};
maybe_set_metric_value(_, OutputEvent) ->
    OutputEvent.

-spec maybe_set_context_kind(ldclient_event:event(), map()) -> map().
maybe_set_context_kind(#{user := User}, OutputEvent) ->
    case ldclient_user:get(anonymous, User) of
        true -> OutputEvent#{<<"contextKind">> => <<"anonymousUser">>};
        _ -> OutputEvent
    end.

-spec format_summary_event(ldclient_event_server:summary_event()) -> map().
format_summary_event(SummaryEvent) when map_size(SummaryEvent) == 0 -> #{};
format_summary_event(#{start_date := StartDate, end_date := EndDate, counters := Counters}) ->
    #{
        <<"kind">> => <<"summary">>,
        <<"startDate">> => StartDate,
        <<"endDate">> => EndDate,
        <<"features">> => format_summary_event_counters(Counters)
    }.

-spec format_summary_event_counters(ldclient_event_server:counters()) -> map().
format_summary_event_counters(Counters) ->
    maps:fold(fun format_summary_event_counters/3, #{}, Counters).

-spec format_summary_event_counters(ldclient_event_server:counter_key(), ldclient_event_server:counter_value(), map()) ->
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
    Acc
) ->
    FlagMap = maps:get(FlagKey, Acc, #{default => Default, counters => []}),
    CounterWithVersion = maybe_set_unknown(Version, #{
        value => FlagValue,
        count => Count
    }),
    CounterWithVariation = maybe_set_variation(Variation, CounterWithVersion),
    Counter = maybe_add_version(Version, CounterWithVariation),
    NewFlagMap = FlagMap#{counters := [Counter|maps:get(counters, FlagMap)]},
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
