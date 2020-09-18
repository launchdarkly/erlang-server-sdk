%%-------------------------------------------------------------------
%% @doc Event processor server
%%
%% @end
%%-------------------------------------------------------------------

-module(ldclient_event_process_server).

-behaviour(gen_server).

%% Supervision
-export([start_link/1, init/1]).

%% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% API
-export([send_events/3]).

%% Types
-type state() :: #{
    sdk_key := string(),
    dispatcher := atom(),
    inline_users := boolean(),
    global_private_attributes := ldclient_settings:private_attributes(),
    events_uri := string()
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
    SdkKey = ldclient_settings:get_value(Tag, sdk_key),
    Dispatcher = ldclient_settings:get_value(Tag, events_dispatcher),
    InlineUsers = ldclient_settings:get_value(Tag, inline_users_in_events),
    GlobalPrivateAttributes = ldclient_settings:get_value(Tag, private_attributes),
    EventsUri = ldclient_settings:get_value(Tag, events_uri) ++ "/bulk",
    State = #{
        sdk_key => SdkKey,
        dispatcher => Dispatcher,
        inline_users => InlineUsers,
        global_private_attributes => GlobalPrivateAttributes,
        events_uri => EventsUri
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

handle_cast({send_events, Events, SummaryEvent},
    #{
        sdk_key := SdkKey,
        dispatcher := Dispatcher,
        inline_users := InlineUsers,
        global_private_attributes := GlobalPrivateAttributes,
        events_uri := Uri
    } = State) ->
    FormattedSummaryEvent = format_summary_event(SummaryEvent),
    FormattedEvents = format_events(Events, InlineUsers, GlobalPrivateAttributes),
    OutputEvents = combine_events(FormattedEvents, FormattedSummaryEvent),
    PayloadId = uuid:get_v4(),
    _ = case send(OutputEvents, PayloadId, Dispatcher, Uri, SdkKey) of
        ok ->
            ok;
        {error, temporary, _Reason} ->
            erlang:send_after(1000, self(), {send, OutputEvents, PayloadId});
        {error, permanent, Reason} ->
            error_logger:error_msg("Permanent error sending events ~p", [Reason])
    end,
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({send, OutputEvents, PayloadId}, #{sdk_key := SdkKey, dispatcher := Dispatcher, events_uri := Uri} = State) ->
    _ = send(OutputEvents, PayloadId, Dispatcher, Uri, SdkKey),
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

-spec format_events([ldclient_event:event()], boolean(), ldclient_settings:private_attributes()) -> list().
format_events(Events, InlineUsers, GlobalPrivateAttributes) ->
    {FormattedEvents, _, _} = lists:foldl(fun format_event/2, {[], InlineUsers, GlobalPrivateAttributes}, Events),
    FormattedEvents.

-spec format_event(ldclient_event:event(), {list(), boolean(), ldclient_settings:private_attributes()}) ->
    {list(), boolean(), ldclient_settings:private_attributes()}.
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
    OutputEvent = #{
        <<"kind">> => Kind,
        <<"creationDate">> => Timestamp,
        <<"key">> => Key,
        <<"variation">> => Variation,
        <<"value">> => Value,
        <<"default">> => Default,
        <<"version">> => Version,
        <<"prereqOf">> => PrereqOf
    },
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
    OutputEvent = maybe_set_metric_value(Event, #{
        <<"kind">> => Kind,
        <<"creationDate">> => Timestamp,
        <<"key">> => Key,
        <<"data">> => Data
    }),
    FormattedEvent = format_event_set_user(Kind, User, OutputEvent, InlineUsers, GlobalPrivateAttributes),
    {[FormattedEvent|FormattedEvents], InlineUsers, GlobalPrivateAttributes}.

-spec maybe_set_reason(ldclient_event:event(), #{binary() => any()}) -> #{binary() => any()}.
maybe_set_reason(#{data := #{eval_reason := EvalReason}}, OutputEvent) ->
    OutputEvent#{<<"reason">> => format_eval_reason(EvalReason)};
maybe_set_reason(_Event, OutputEvent) ->
    OutputEvent.

-spec format_eval_reason(ldclient_eval:reason()) -> map().
format_eval_reason(target_match) -> #{<<"kind">> => <<"TARGET_MATCH">>};
format_eval_reason({rule_match, RuleIndex, RuleId}) -> #{kind => <<"RULE_MATCH">>, ruleIndex => RuleIndex, ruleId => RuleId};
format_eval_reason({prerequisite_failed, [PrereqKey|_]}) -> #{<<"kind">> => <<"PREREQUISITE_FAILED">>, prerequisiteKey => PrereqKey};
format_eval_reason({error, client_not_ready}) -> #{kind => <<"ERROR">>, errorKind => <<"CLIENT_NOT_READY">>};
format_eval_reason({error, flag_not_found}) -> #{kind => <<"ERROR">>, errorKind => <<"FLAG_NOT_FOUND">>};
format_eval_reason({error, malformed_flag}) -> #{kind => <<"ERROR">>, errorKind => <<"MALFORMED_FLAG">>};
format_eval_reason({error, user_not_specified}) -> #{kind => <<"ERROR">>, errorKind => <<"USER_NOT_SPECIFIED">>};
format_eval_reason({error, wrong_type}) -> #{kind => <<"ERROR">>, errorKind => <<"WRONG_TYPE">>};
format_eval_reason({error, exception}) -> #{kind => <<"ERROR">>, errorKind => <<"EXCEPTION">>};
format_eval_reason(fallthrough) -> #{<<"kind">> => <<"FALLTHROUGH">>};
format_eval_reason(off) -> #{<<"kind">> => <<"OFF">>}.

-spec format_event_set_user(binary(), ldclient_user:user(), map(), boolean(), ldclient_settings:private_attributes()) -> map().
format_event_set_user(<<"feature">>, User, OutputEvent, true, GlobalPrivateAttributes) ->
    {ScrubbedUser, ScrubbedAttrNames} = ldclient_user:scrub(User, GlobalPrivateAttributes),
    OutputEvent#{
        <<"user">> => ScrubbedUser#{<<"privateAttrs">> => ScrubbedAttrNames}
    };
format_event_set_user(<<"feature">>, #{key := UserKey}, OutputEvent, false, _) ->
    OutputEvent#{<<"userKey">> => UserKey};
format_event_set_user(<<"feature">>, _User, OutputEvent, _, _) ->
    % User has no key
    OutputEvent#{<<"userKey">> => null};
format_event_set_user(<<"debug">>, User, OutputEvent, _, GlobalPrivateAttributes) ->
    {ScrubbedUser, ScrubbedAttrNames} = ldclient_user:scrub(User, GlobalPrivateAttributes),
    OutputEvent#{
        <<"user">> => ScrubbedUser#{<<"privateAttrs">> => ScrubbedAttrNames}
    };
format_event_set_user(<<"identify">>, #{key := UserKey} = User, OutputEvent, _, GlobalPrivateAttributes) ->
    {ScrubbedUser, ScrubbedAttrNames} = ldclient_user:scrub(User, GlobalPrivateAttributes),
    OutputEvent#{
        <<"key">> => UserKey,
        <<"user">> => ScrubbedUser#{<<"privateAttrs">> => ScrubbedAttrNames}
    };
format_event_set_user(<<"index">>, User, OutputEvent, _, GlobalPrivateAttributes) ->
    {ScrubbedUser, ScrubbedAttrNames} = ldclient_user:scrub(User, GlobalPrivateAttributes),
    OutputEvent#{
        <<"user">> => ScrubbedUser#{<<"privateAttrs">> => ScrubbedAttrNames}
    };
format_event_set_user(<<"custom">>, User, OutputEvent, true, GlobalPrivateAttributes) ->
    {ScrubbedUser, ScrubbedAttrNames} = ldclient_user:scrub(User, GlobalPrivateAttributes),
    OutputEvent#{
        <<"user">> => ScrubbedUser#{<<"privateAttrs">> => ScrubbedAttrNames}
    };
format_event_set_user(<<"custom">>, #{key := UserKey}, OutputEvent, false, _) ->
    OutputEvent#{<<"userKey">> => UserKey}.

-spec maybe_set_metric_value(ldclient_event:event(), map()) -> map().
maybe_set_metric_value(#{metric_value := MetricValue}, OutputEvent) ->
    OutputEvent#{<<"metricValue">> => MetricValue};
maybe_set_metric_value(_, OutputEvent) ->
    OutputEvent.

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
    Counter = #{
        value => FlagValue,
        version => Version,
        count => Count,
        variation => Variation,
        unknown => if Version == null -> true; true -> false end
    },
    NewFlagMap = FlagMap#{counters := [Counter|maps:get(counters, FlagMap)]},
    Acc#{FlagKey => NewFlagMap}.

-spec combine_events(OutputEvents :: list(), OutputSummaryEvent :: map()) -> list().
combine_events([], OutputSummaryEvent) when map_size(OutputSummaryEvent) == 0 -> [];
combine_events(OutputEvents, OutputSummaryEvent) when map_size(OutputSummaryEvent) == 0 -> OutputEvents;
combine_events(OutputEvents, OutputSummaryEvent) -> [OutputSummaryEvent|OutputEvents].

-spec send(OutputEvents :: list(), PayloadId :: uuid:uuid(), Dispatcher :: atom(), string(), string()) ->
    ok | {error, temporary, string()} | {error, permanent, string()}.
send([], _, _, _, _) ->
    ok;
send(OutputEvents, PayloadId, Dispatcher, Uri, SdkKey) ->
    JsonEvents = jsx:encode(OutputEvents),
    Dispatcher:send(JsonEvents, PayloadId, Uri, SdkKey).

-spec get_local_reg_name(Tag :: atom()) -> atom().
get_local_reg_name(Tag) ->
    list_to_atom("ldclient_event_process_server_" ++ atom_to_list(Tag)).
