%%-------------------------------------------------------------------
%% @doc Evaluation
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_eval).

%% API
-export([flag_key_for_context/4]).
-export([all_flags_eval/2]).
-export([all_flags_state/3]).

%% Types
-type result() :: {
    Detail :: detail(),
    Events :: [ldclient_event:event()]
}.
-type detail() :: {
    VariationIndex :: variation_index(),
    Value :: result_value(),
    Reason :: reason()
}.
-type result_value() :: null | ldclient_flag:variation_value().
-type variation_index() :: null | non_neg_integer().
-type reason() ::
target_match
    | {rule_match, RuleIndex :: non_neg_integer(), RuleUUID :: binary()}
    | {rule_match, RuleIndex :: non_neg_integer(), in_experiment}
    | {rule_match, RuleIndex :: non_neg_integer(), RuleUUID :: binary(), InExperiment :: true}
    | {prerequisite_failed, FlagKeys :: [binary()]}
    | {error, error_type()}
    | fallthrough
    | {fallthrough, in_experiment}
    | off.
-type error_type() :: client_not_ready | flag_not_found | malformed_flag
    | user_not_specified | wrong_type | exception.
-type feature_flags_state() :: #{
    flag_values => #{binary() => any()}
}.

-type all_flags_state_options() :: #{
    with_reasons => boolean()
%%    client_side_only => boolean(), % TODO: Support.
%%    details_only_for_tracked_flags => boolean() % TODO: Support.
}.

-export_type([detail/0]).
-export_type([reason/0]).
-export_type([result_value/0]).
-export_type([feature_flags_state/0]).
-export_type([all_flags_state_options/0]).

%%===================================================================
%% API
%%===================================================================

%% @doc Evaluates feature flag for a given context
%%
%% @end
-spec flag_key_for_context(
    Tag :: atom(),
    FlagKey :: ldclient_flag:key(),
    Context :: ldclient_context:context(),
    DefaultValue :: result_value()
) -> result().
flag_key_for_context(Tag, FlagKey, Context, DefaultValue) ->
    try
        flag_key_for_context(Tag, FlagKey, Context, DefaultValue, get_state(Tag), get_initialization_state(Tag))
    catch _:_ ->
        Reason = {error, exception},
        Events = [ldclient_event:new_for_unknown_flag(FlagKey, Context, DefaultValue, Reason)],
        {{null, DefaultValue, Reason}, Events}
    end.

-spec flag_key_for_context(
    Tag :: atom(),
    FlagKey :: ldclient_flag:key(),
    Context :: ldclient_context:context(),
    DefaultValue :: result_value(),
    Offline :: atom(),
    Initialized :: atom()
) -> result().
flag_key_for_context(_Tag, _FlagKey, _Context, DefaultValue, offline, _) ->
    {{null, DefaultValue, {error, client_not_ready}}, []};
flag_key_for_context(_Tag, _FlagKey, _Context, DefaultValue, _, not_initialized) ->
    {{null, DefaultValue, {error, client_not_ready}}, []};
flag_key_for_context(Tag, FlagKey, Context, DefaultValue, online, store_initialized) ->
    error_logger:warning_msg("Variation called before LaunchDarkly client initialization completed - using last known values from feature store."),
    FeatureStore = ldclient_config:get_value(Tag, feature_store),
    FlagRecs = FeatureStore:get(Tag, features, FlagKey),
    flag_recs_for_context(FlagKey, FlagRecs, Context, FeatureStore, Tag, DefaultValue);
flag_key_for_context(Tag, FlagKey, Context, DefaultValue, online, initialized) ->
    FeatureStore = ldclient_config:get_value(Tag, feature_store),
    FlagRecs = FeatureStore:get(Tag, features, FlagKey),
    flag_recs_for_context(FlagKey, FlagRecs, Context, FeatureStore, Tag, DefaultValue).

%% @doc Returns an object that encapsulates the state of all feature flags for a given context.
%%
%% This includes the flag values, and also metadata that can be used on the front end.
%% The most common use case for this method is to bootstrap a set of client-side feature flags from a
%% back-end service.
%% @end
-spec all_flags_state(
    Context :: ldclient_context:context(),
    Options :: all_flags_state_options(),
    Tag :: atom()
) -> map().
all_flags_state(Context, Options, Tag) ->
    all_flags_state(Context, Options, Tag, get_state(Tag), get_initialization_state(Tag)).

-spec is_not_deleted(Item :: map()) -> boolean().
is_not_deleted(#{deleted := true}) -> false;
is_not_deleted(_) -> true.

-spec all_flags_state(
    Context :: ldclient_context:context(),
    Options :: all_flags_state_options(),
    Tag :: atom(),
    Offline :: atom(),
    InitializationState :: atom()
) -> map().
all_flags_state(_Context, _Options, _Tag, offline, _) ->
    #{<<"$valid">> => false, <<"$flagsState">> => #{}};
all_flags_state(_Context, _Options, _Tag, _, not_initialized) ->
    #{<<"$valid">> => false, <<"$flagsState">> => #{}};
all_flags_state(Context, #{with_reasons := WithReason} = _Options, Tag, Offline, store_initialized) ->
    error_logger:warning_msg("Called allFlagsState before client initialization; using last known values from data store."),
    all_flags_state(Context, #{with_reasons := WithReason} = _Options, Tag, Offline, initialized);
all_flags_state(Context, #{with_reasons := WithReason} = _Options, Tag, _, initialized) ->
    FeatureStore = ldclient_config:get_value(Tag, feature_store),
    AllFlags = [Flag || Flag = {_, FlagValue} <- FeatureStore:all(Tag, features), is_not_deleted(FlagValue)],
    EvalFun = fun({FlagKey, #{version := Version} = Flag}, #{<<"$flagsState">> := FlagsState} = Acc) ->
        % Here the state is either initialized, or store_initialized, and we are online. Call directly to that version
        % of flag_key_for_context. This will prevent additional warnings for the client initialization not being
        % complete in the store_initialized state.
        {{VariationIndex, V, Reason}, _Events} = flag_key_for_context(Tag, FlagKey, Context, null, online, initialized),
        FlagState = maybe_add_track_events(Flag,
            maybe_add_debug_events_until_date(Flag, #{
                <<"version">> => Version})),
        UpdatedFlagState = case is_integer(VariationIndex) of
            true -> FlagState#{
                <<"variation">> => VariationIndex
            };
            false -> FlagState
        end,
        FlagStateWithReason = maybe_add_reason(Flag, Reason, WithReason, UpdatedFlagState),
        UpdatedFlagsState = maps:put(FlagKey, FlagStateWithReason, FlagsState),
        Acc#{FlagKey => V, <<"$flagsState">> => UpdatedFlagsState}
    end,
    lists:foldl(EvalFun, #{
        <<"$valid">> => true,
        <<"$flagsState">> => #{}
    }, AllFlags).

-spec maybe_add_reason(Flag :: ldclient_flag:flag(), Reason :: reason(), WithReason :: boolean(), Map :: map()) -> map().
maybe_add_reason(_Flag, Reason, true = _WithReason, Map) ->
    Map#{<<"reason">> => ldclient_eval_reason:format(Reason)};
maybe_add_reason(#{trackEventsFallthrough := true} = _Flag, fallthrough = Reason, _WithReason, Map) ->
    Map#{
        <<"trackReason">> => true,
        <<"trackEvents">> => true,
        <<"reason">> => ldclient_eval_reason:format(Reason)
    };
maybe_add_reason(_Flag, {fallthrough, in_experiment} = Reason, _WithReason, Map) ->
    Map#{
        <<"trackReason">> => true,
        <<"reason">> => ldclient_eval_reason:format(Reason)
    };
maybe_add_reason(_Flag, {rule_match, _, in_experiment} = Reason, _WithReason, Map) ->
    Map#{
        <<"trackReason">> => true,
        <<"reason">> => ldclient_eval_reason:format(Reason)
    };
maybe_add_reason(#{rules := Rules} = _Flag, {rule_match, RuleIndex, _RuleId} = Reason, _WithReason, Map) ->
    MatchedRule = lists:nth(RuleIndex + 1, Rules),
    case MatchedRule of
        #{trackEvents := true} ->
            Map#{
                <<"trackEvents">> => true,
                <<"trackReason">> => true,
                <<"reason">> => ldclient_eval_reason:format(Reason)
            };
        _ ->
            Map
    end;
maybe_add_reason(_Flag, _Reason, _WithReason, Map) ->
    Map.

-spec maybe_add_track_events(Flag :: ldclient_flag:flag(), Map :: map()) -> map().
maybe_add_track_events(#{trackEvents := true} = _Flag, Map) ->
    Map#{<<"trackEvents">> => true};
maybe_add_track_events(_Flag, Map) -> Map.

maybe_add_debug_events_until_date(#{debugEventsUntilDate := DebugEventsUntilDate} = _Flag, Map) when is_integer(DebugEventsUntilDate) ->
    Map#{<<"debugEventsUntilDate">> => DebugEventsUntilDate};
maybe_add_debug_events_until_date(_Flag, Map) -> Map.

%% @doc Returns all flags for a given context
%%
%% @end
-spec all_flags_eval(
    Context :: ldclient_context:context(),
    Tag :: atom()
) -> feature_flags_state().
all_flags_eval(Context, Tag) ->
    all_flags_eval(Context, Tag, get_state(Tag), get_initialization_state(Tag)).

-spec all_flags_eval(
    Context :: ldclient_context:context(),
    Tag :: atom(),
    Offline :: atom(),
    Initialized :: atom()
) -> feature_flags_state().
all_flags_eval(_Context, _Tag, offline, _) ->
    #{flag_values => #{}};
all_flags_eval(_Context, _Tag, _, not_initialized) ->
    #{flag_values => #{}};
all_flags_eval(Context, Tag, Offline, store_initialized) ->
    error_logger:warning_msg("Called allFlagsState before client initialization; using last known values from data store."),
    all_flags_eval(Context, Tag, Offline, initialized);
all_flags_eval(Context, Tag, online, initialized) ->
    FeatureStore = ldclient_config:get_value(Tag, feature_store),
    AllFlags = [FlagKey || {FlagKey, Flag} <- FeatureStore:all(Tag, features), is_not_deleted(Flag)],
    EvalFun = fun(FlagKey, Acc) ->
        % Here the state is either initialized, or store_initialized, and we are online. Call directly to that version
        % of flag_key_for_context. This will prevent additional warnings for the client initialization not being
        % complete in the store_initialized state.
        {{_, V, _}, _Events} = flag_key_for_context(Tag, FlagKey, Context, null, online, initialized),
        Acc#{FlagKey => V}
              end,
    #{flag_values => lists:foldl(EvalFun, #{}, AllFlags)}.

%%===================================================================
%% Internal functions
%%===================================================================

-spec get_state(Tag :: atom()) -> atom().
get_state(Tag) -> get_state(Tag, ldclient:is_offline(Tag)).

-spec get_state(Tag :: atom(), Offline :: boolean()) -> atom().
get_state(_Tag, true) -> offline;
get_state(_Tag, false) -> online.

%% An SDK is initialized when it has received a flag payload from launchdarkly. An SDK using a persistent store may
%% not be initialized, but it may have an initialized persistent store. In the case the store is initialized,
%% then flags should be evaluated from the store.
-spec get_initialization_state(Tag :: atom()) -> initialized | not_initialized | store_initialized.
get_initialization_state(Tag) -> get_initialization_state(Tag, ldclient:initialized(Tag)).

get_initialization_state(_Tag, true) -> initialized;
get_initialization_state(Tag, false) ->
    % If not initialized, and redis is in use, then ask the redis store if it is initialized.
    % After the redis store has been established to be initialized it will set the storage state.

    % This logic is currently only applied to redis, because other stores do not currently
    % have a meaningful initialized state.
    case ldclient_config:get_value(Tag, feature_store) of
         ldclient_storage_redis ->
             % First check the state, checking redis is slower, so should only be done if the
             % state has not been set.
             case ldclient_instance:feature_store_initialized(Tag) of
                 true -> store_initialized;
                 false -> case ldclient_storage_redis:get_init(Tag) of
                              true -> store_initialized;
                              false -> not_initialized
                          end
             end;
        % For non-redis stores treat them as not initialized.
         _ -> not_initialized
    end.

-spec flag_recs_for_context(
    FlagKey :: ldclient_flag:key(),
    FlagRecs :: [{ldclient_flag:key(), ldclient_flag:flag()}],
    Context :: ldclient_context:context(),
    FeatureStore :: atom(),
    Tag :: atom(),
    DefaultValue :: result_value()
) -> result().
flag_recs_for_context(FlagKey, [], Context, _FeatureStore, _Tag, DefaultValue) ->
    % Flag not found
    error_logger:warning_msg("Unknown feature flag ~p; returning default value", [FlagKey]),
    Reason = {error, flag_not_found},
    Events = [ldclient_event:new_for_unknown_flag(FlagKey, Context, DefaultValue, Reason)],
    {{null, DefaultValue, Reason}, Events};
flag_recs_for_context(FlagKey, [{FlagKey, #{deleted := true}} | _], Context, _FeatureStore, _Tag, DefaultValue) ->
    % Flag found, but it's deleted
    error_logger:warning_msg("Unknown feature flag ~p; returning default value", [FlagKey]),
    Reason = {error, flag_not_found},
    Events = [ldclient_event:new_for_unknown_flag(FlagKey, Context, DefaultValue, Reason)],
    {{null, DefaultValue, Reason}, Events};
flag_recs_for_context(FlagKey, [{FlagKey, Flag} | _], Context, FeatureStore, Tag, DefaultValue) ->
    % Flag found
    flag_for_context_check_valid(Flag, Context, FeatureStore, Tag, DefaultValue).

-spec flag_for_context_check_valid(ldclient_flag:flag(), ldclient_context:context(), atom(), atom(), result_value()) -> result().
flag_for_context_check_valid(Flag, Context, FeatureStore, Tag, DefaultValue) ->
    case ldclient_context:is_valid(Context, true) of
        true -> flag_for_context(Flag, Context, FeatureStore, Tag, DefaultValue);
        false -> flag_for_invalid_context(Flag, Context, DefaultValue)
    end.

-spec flag_for_invalid_context(ldclient_flag:flag(), ldclient_context:context(), result_value()) -> result().
flag_for_invalid_context(_Flag, _Context, DefaultValue) ->
    Reason = {error, user_not_specified},
    {{null, DefaultValue, Reason}, []}.

-spec flag_for_context(ldclient_flag:flag(), ldclient_context:context(), atom(), atom(), result_value()) -> result().
flag_for_context(Flag, Context, FeatureStore, Tag, DefaultValue) ->
    {{Variation, VariationValue, Reason}, Events} = flag_for_context_valid(Flag, Context, FeatureStore, Tag, DefaultValue),
    FlagEvalEvent = ldclient_event:new_flag_eval(Variation, VariationValue, DefaultValue, Context, Reason, Flag),
    {{Variation, VariationValue, Reason}, [FlagEvalEvent | Events]}.

-spec flag_for_context_valid(ldclient_flag:flag(), ldclient_context:context(), atom(), atom(), result_value()) -> result().
flag_for_context_valid(#{on := false, offVariation := OffVariation} = Flag, _Context, _FeatureStore, _Tag, DefaultValue)
    when is_integer(OffVariation), OffVariation >= 0 ->
    result_for_variation_index(OffVariation, off, Flag, [], DefaultValue);
flag_for_context_valid(#{on := false, offVariation := OffVariation} = _Flag, _Context, _FeatureStore, _Tag, DefaultValue)
% offVariation is negative. The flag is malformed.
    when is_integer(OffVariation), OffVariation =< 0 ->
    Reason = {error, malformed_flag},
    {{null, DefaultValue, Reason}, []};
flag_for_context_valid(#{on := false} = _Flag, _Context, _FeatureStore, _Tag, DefaultValue) ->
    % offVariation is null or not set
    {{null, DefaultValue, off}, []};
flag_for_context_valid(#{prerequisites := Prerequisites} = Flag, Context, FeatureStore, Tag, DefaultValue) ->
    check_prerequisites(Prerequisites, Flag, Context, FeatureStore, Tag, DefaultValue).

-spec check_prerequisites([ldclient_flag:prerequisite()], ldclient_flag:flag(), ldclient_context:context(), atom(), atom(), result_value()) ->
    result().
check_prerequisites(Prerequisites, #{key := FlagKey} = Flag, Context, FeatureStore, Tag, DefaultValue) ->
    check_prerequisites(Prerequisites, Flag, Context, FeatureStore, Tag, DefaultValue, [], [FlagKey]).

-spec check_prerequisites(
    Prerequisites :: [ldclient_flag:prerequisite()],
    Flag :: ldclient_flag:flag(),
    Context :: ldclient_context:context(),
    FeatureStore :: atom(),
    Tag :: atom(),
    DefaultValue :: result_value(),
    Events :: [ldclient_event:event()],
    VisitedFlags :: [binary()]
) -> result().
check_prerequisites(
    [],
    Flag,
    Context,
    FeatureStore,
    Tag,
    DefaultValue,
    Events,
    _VisitedFlags
) ->
    flag_for_context_prerequisites(success, Flag, Context, FeatureStore, Tag, DefaultValue, Events);
check_prerequisites(
    [#{key := PrerequisiteKey, variation := Variation} | Rest],
    #{key := FlagKey} = Flag,
    Context,
    FeatureStore,
    Tag,
    DefaultValue,
    Events,
    VisitedFlags
) ->
    case lists:member(PrerequisiteKey, VisitedFlags) of
        true ->
            error_logger:error_msg("Prerequisite of ~p causing a circular reference."
            " This is probably a temporary condition due to an incomplete update.", [FlagKey]),
            flag_for_context_prerequisites({malformed_flag, {error, malformed_flag}},
                Flag, Context, FeatureStore, Tag, DefaultValue, Events);
        false ->
            PrerequisiteFlagRecs = FeatureStore:get(Tag, features, PrerequisiteKey),
            check_prerequisite_recs(PrerequisiteFlagRecs, PrerequisiteKey, Variation,
                Rest, Flag, Context, FeatureStore, Tag, DefaultValue, Events, [PrerequisiteKey | VisitedFlags])
    end.

-spec check_prerequisite_recs(
    PrerequisiteFlagRecs :: [ldclient_flag:flag()], %% Contains 0 or 1 flags.
    PrerequisiteKey :: binary(),
    Variation :: non_neg_integer(),
    Prerequisites :: [binary()],
    Flag :: ldclient_flag:flag(),
    Context :: ldclient_context:context(),
    FeatureStore :: atom(),
    Tag :: atom(),
    DefaultValue :: result_value(),
    Events :: [ldclient_event:event()],
    VisitedFlags :: [binary()]
) -> result().
check_prerequisite_recs([], PrerequisiteKey, _Variation, _Prerequisites, #{key := FlagKey} = Flag,
    Context, FeatureStore, Tag, DefaultValue, Events, _VisitedFlags) ->
    % Short circuit if prerequisite flag is not found
    error_logger:error_msg("Could not retrieve prerequisite flag ~p when evaluating ~p", [PrerequisiteKey, FlagKey]),
    flag_for_context_prerequisites({fail, {prerequisite_failed, [PrerequisiteKey]}}, Flag, Context,
        FeatureStore, Tag, DefaultValue, Events);
check_prerequisite_recs([{PrerequisiteKey, PrerequisiteFlag} | _], PrerequisiteKey, Variation, Prerequisites, Flag,
    Context, FeatureStore, Tag, DefaultValue, Events, VisitedFlags) ->
    check_prerequisite_flag(PrerequisiteFlag, Variation, Prerequisites, Flag, Context,
        FeatureStore, Tag, DefaultValue, Events, VisitedFlags).

-spec check_prerequisite_flag(
    PrerequisiteFlag :: ldclient_flag:flag(),
    Variation :: non_neg_integer(),
    Prerequisites :: [binary()],
    Flag :: ldclient_flag:flag(),
    Context :: ldclient_context:context(),
    FeatureStore :: atom(),
    Tag :: atom(),
    DefaultValue :: result_value(),
    Events :: [ldclient_event:event()],
    VisitedFlags :: [binary()]
) -> result().
check_prerequisite_flag(#{key := PrerequisiteKey, deleted := true}, _, _, Flag, Context, FeatureStore,
    Tag, DefaultValue, Events, _VisitedFlags) ->
    % Prerequisite flag is deleted, short circuit fail
    Result = {fail, {prerequisite_failed, [PrerequisiteKey]}},
    flag_for_context_prerequisites(Result, Flag, Context, FeatureStore, Tag, DefaultValue, Events);
check_prerequisite_flag(#{key := PrerequisiteKey, on := false}, _, _, Flag, Context, FeatureStore, Tag,
    DefaultValue, Events, _VisitedFlags) ->
    % Prerequisite flag is off, short circuit fail
    Result = {fail, {prerequisite_failed, [PrerequisiteKey]}},
    flag_for_context_prerequisites(Result, Flag, Context, FeatureStore, Tag, DefaultValue, Events);
check_prerequisite_flag(#{prerequisites := SubPrerequisites} = PrerequisiteFlag, Variation, Prerequisites,
    #{key := FlagKey} = Flag, Context, FeatureStore, Tag, DefaultValue, Events, VisitedFlags) ->
    {{ResultVariation, ResultVariationValue, ResultReason}, ResultEvents} =
        check_prerequisites(SubPrerequisites, PrerequisiteFlag, Context, FeatureStore,
            Tag, DefaultValue, Events, VisitedFlags),
    NewEvents = [
        ldclient_event:new_prerequisite_eval(ResultVariation, ResultVariationValue, FlagKey,
            Context, ResultReason, PrerequisiteFlag) | ResultEvents
    ],
    case ResultReason of
        %% If there was a malformed dependent flag, when we want to short circuit, not treat it as a prerequisite
        %% failure.
        {error, malformed_flag} -> {{ResultVariation, ResultVariationValue, ResultReason}, ResultEvents};
        _ -> check_prerequisite_flag_result(PrerequisiteFlag, Variation =:= ResultVariation, Prerequisites,
            Flag, Context, FeatureStore, Tag, DefaultValue, NewEvents, VisitedFlags)
    end.

-spec check_prerequisite_flag_result(
    PrerequisiteFlag :: ldclient_flag:flag(),
    MatchedVariation :: boolean(),
    Prerequisites :: [binary()],
    Flag :: ldclient_flag:flag(),
    Context :: ldclient_context:context(),
    FeatureStore :: atom(),
    Tag :: atom(),
    DefaultValue :: result_value(),
    Events :: [ldclient_event:event()],
    VisitedFlags :: [binary()]
) -> result().
check_prerequisite_flag_result(#{key := PrerequisiteKey}, false, _Prerequisites,
    Flag, Context, FeatureStore, Tag, DefaultValue, Events, _VisitedFlags) ->
    % Prerequisite flag variation didn't match: short-circuit fail and move on
    Result = {fail, {prerequisite_failed, [PrerequisiteKey]}},
    flag_for_context_prerequisites(Result, Flag, Context, FeatureStore, Tag, DefaultValue, Events);
check_prerequisite_flag_result(_PrerequisiteFlag, true, Prerequisites, Flag,
    Context, FeatureStore, Tag, DefaultValue, Events, VisitedFlags) ->
    %% This moves to the next flag is the list of prerequisites, so we want to remove the head from the VisitedFlags
    check_prerequisites(Prerequisites, Flag, Context, FeatureStore, Tag, DefaultValue, Events, tl(VisitedFlags)).

-spec flag_for_context_prerequisites(
    Reason :: {fail | malformed_flag, Reason :: reason()} | success,
    Flag :: ldclient_flag:flag(),
    Context :: ldclient_context:context(),
    FeatureStore :: atom(),
    Tag :: atom(),
    DefaultValue :: result_value(),
    Events :: [ldclient_event:event()]
) -> result().
flag_for_context_prerequisites({malformed_flag, Reason}, _Flag, _Context, _FeatureStore, _Tag, DefaultValue, Events) ->
    {{null, DefaultValue, Reason}, Events};
flag_for_context_prerequisites({fail, Reason}, #{offVariation := OffVariation} = Flag, _Context, _FeatureStore, _Tag, DefaultValue, Events)
    when is_integer(OffVariation), OffVariation >= 0 ->
    result_for_variation_index(OffVariation, Reason, Flag, Events, DefaultValue);
flag_for_context_prerequisites({fail, Reason}, _Flag, _Context, _FeatureStore, _Tag, DefaultValue, Events) ->
    % prerequisite failed, but offVariation is null or not set
    {{null, DefaultValue, Reason}, Events};
flag_for_context_prerequisites(success, Flag, Context, FeatureStore, Tag, DefaultValue, Events) ->
    check_targets(Flag, Context, FeatureStore, Tag, DefaultValue, Events).

check_targets(Flag, Context, FeatureStore, Tag, DefaultValue, Events) ->
    Result = ldclient_targets:eval_flag_targets(Flag, Context),
    flag_for_context_targets(Result, Flag, Context, FeatureStore, Tag, DefaultValue, Events).

flag_for_context_targets({match, Variation}, Flag, _Context, _FeatureStore, _Tag, DefaultValue, Events) ->
    Reason = target_match,
    result_for_variation_index(Variation, Reason, Flag, Events, DefaultValue);
flag_for_context_targets(no_match, #{rules := Rules} = Flag, Context, FeatureStore, Tag, DefaultValue, Events) ->
    check_rules(Rules, Flag, Context, FeatureStore, Tag, DefaultValue, Events, 0).

check_rules([], Flag, Context, _FeatureStore, _Tag, DefaultValue, Events, _) ->
    flag_for_context_rules(no_match, Flag, Context, DefaultValue, Events);
check_rules([Rule | Rest], Flag, Context, FeatureStore, Tag, DefaultValue, Events, Index) ->
    Result = ldclient_rule:match_context(Rule, Context, FeatureStore, Tag),
    check_rule_result({Result, Rule, Index}, Rest, Flag, Context, FeatureStore, Tag, DefaultValue, Events).

check_rule_result({malformed_flag, _Rule, _Index}, _Rest, #{key := FlagKey} = _Flag, _Context, _FeatureStore, _Tag, DefaultValue, _Events) ->
    error_logger:warning_msg("Data inconsistency in feature flag ~p: clause was malformed", [FlagKey]),
    Reason = {error, malformed_flag},
    {{null, DefaultValue, Reason}, []};
check_rule_result({no_match, _Rule, Index}, Rest, Flag, Context, FeatureStore, Tag, DefaultValue, Events) ->
    check_rules(Rest, Flag, Context, FeatureStore, Tag, DefaultValue, Events, Index + 1);
check_rule_result({match, Rule, Index}, _Rest, Flag, Context, _FeatureStore, _Tag, DefaultValue, Events) ->
    % Rule matched: short-circuit
    flag_for_context_rules({match, Rule, Index}, Flag, Context, DefaultValue, Events).

flag_for_context_rules({match, #{id := Id, variationOrRollout := VorR}, Index}, Flag, Context, DefaultValue, Events) ->
    Reason = {rule_match, Index, Id},
    flag_for_context_variation_or_rollout(VorR, Reason, Flag, Context, DefaultValue, Events);
flag_for_context_rules(no_match, #{fallthrough := Fallthrough} = Flag, Context, DefaultValue, Events) ->
    flag_for_context_variation_or_rollout(Fallthrough, fallthrough, Flag, Context, DefaultValue, Events).

flag_for_context_variation_or_rollout(Variation, Reason, Flag, _Context, DefaultValue, Events) when is_integer(Variation) ->
    result_for_variation_index(Variation, Reason, Flag, Events, DefaultValue);
flag_for_context_variation_or_rollout(Rollout, Reason, #{key := FlagKey} = Flag, Context, DefaultValue, Events) when is_map(Rollout) ->
    case ldclient_rollout:rollout_context(Rollout, Flag, Context) of
        malformed_flag ->
            error_logger:warning_msg("Data inconsistency in feature flag ~p: rollout was malformed", [FlagKey]),
            {{null, DefaultValue, {error, malformed_flag}}, []};
        {Result, InExperiment} ->
            UpdatedReason = experimentize_reason(InExperiment, Reason),
            flag_for_context_rollout_result(Result, UpdatedReason, Flag, DefaultValue, Events)
    end;
flag_for_context_variation_or_rollout(null, _Reason, #{key := FlagKey}, _Context, DefaultValue, Events) ->
    error_logger:warning_msg("Data inconsistency in feature flag ~p: rule object with no variation or rollout", [FlagKey]),
    Reason = {error, malformed_flag},
    {{null, DefaultValue, Reason}, Events}.

flag_for_context_rollout_result(null, _Reason, #{key := FlagKey}, DefaultValue, Events) ->
    error_logger:warning_msg("Data inconsistency in feature flag ~p: variation/rollout object with no variation or rollout", [FlagKey]),
    Reason = {error, malformed_flag},
    {{null, DefaultValue, Reason}, Events};

flag_for_context_rollout_result(Variation, Reason, Flag, DefaultValue, Events) ->
    result_for_variation_index(Variation, Reason, Flag, Events, DefaultValue).

result_for_variation_index(Variation, Reason, Flag, Events, DefaultValue) ->
    VariationValue = ldclient_flag:get_variation(Flag, Variation),
    result_for_variation_value(VariationValue, Variation, Reason, Flag, Events, DefaultValue).

result_for_variation_value(null, _Variation, _Reason, _Flag, Events, DefaultValue) ->
    Reason = {error, malformed_flag},
    {{null, DefaultValue, Reason}, Events};
result_for_variation_value(VariationValue, Variation, Reason, _Flag, Events, _DefaultValue) ->
    {{Variation, VariationValue, Reason}, Events}.

-spec experimentize_reason(InExperiment :: boolean(), Reason :: reason()) -> reason().
experimentize_reason(true, fallthrough) ->
    {fallthrough, in_experiment};
experimentize_reason(true, {rule_match, RuleIndex, RuleUUID}) ->
    {rule_match, RuleIndex, RuleUUID, in_experiment};
experimentize_reason(_InExperiment, Reason) ->
    Reason.
