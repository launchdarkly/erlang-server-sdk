%%-------------------------------------------------------------------
%% @doc Evaluation
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_eval).

%% API
-export([flag_key_for_user/4]).
-export([all_flags_eval/2]).

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
    | {prerequisite_failed, FlagKeys :: [binary()]}
    | {error, error_type()}
    | fallthrough
    | off.
-type error_type() :: client_not_ready | flag_not_found | malformed_flag
    | user_not_specified | wrong_type | exception.
-type feature_flags_state() :: #{
    flag_values => #{binary() => any()}
}.

-export_type([detail/0]).
-export_type([reason/0]).
-export_type([result_value/0]).
-export_type([feature_flags_state/0]).

%%===================================================================
%% API
%%===================================================================

%% @doc Evaluates feature flag for a given user
%%
%% @end
-spec flag_key_for_user(
    Tag :: atom(),
    FlagKey :: ldclient_flag:key(),
    User :: ldclient_user:user(),
    DefaultValue :: result_value()
) -> result().
flag_key_for_user(Tag, FlagKey, User, DefaultValue) ->
    try
        flag_key_for_user(Tag, FlagKey, User, DefaultValue, get_state(Tag), get_initialization_state(Tag))
    catch _:_->
        Reason = {error, exception},
        Events = [ldclient_event:new_for_unknown_flag(FlagKey, User, DefaultValue, Reason)],
        {{null, DefaultValue, Reason}, Events}
    end.

-spec flag_key_for_user(
    Tag :: atom(),
    FlagKey :: ldclient_flag:key(),
    User :: ldclient_user:user(),
    DefaultValue :: result_value(),
    Offline :: atom(),
    Initialized :: atom()
) -> result().
flag_key_for_user(_Tag, _FlagKey, _User, DefaultValue, offline, _) ->
    {{null, DefaultValue, {error, client_not_ready}}, []};
flag_key_for_user(_Tag, _FlagKey, _User, DefaultValue, _, not_initialized) ->
    {{null, DefaultValue, {error, client_not_ready}}, []};
flag_key_for_user(Tag, FlagKey, User, DefaultValue, online, initialized) ->
    FeatureStore = ldclient_config:get_value(Tag, feature_store),
    FlagRecs = FeatureStore:get(Tag, features, FlagKey),
    flag_recs_for_user(FlagKey, FlagRecs, User, FeatureStore, Tag, DefaultValue).

%% @doc Returns all flags for a given user
%%
%% @end
-spec all_flags_eval(
    User :: ldclient_user:user(),
    Tag :: atom()
) -> feature_flags_state().
all_flags_eval(User, Tag) ->
    all_flags_eval(User, Tag, get_state(Tag), get_initialization_state(Tag)).

-spec all_flags_eval(
    User :: ldclient_user:user(),
    Tag :: atom(),
    Offline :: atom(),
    Initialized :: atom()
) -> feature_flags_state().
all_flags_eval(_User, _Tag, offline, _) ->
    error_logger:warning_msg("Called all_flags_state in offline mode. Returning empty state"),
    #{flag_values => #{}};
all_flags_eval(_User, _Tag, _, not_initialized) ->
    error_logger:warning_msg("Called all_flags_state before client initialization. Returning empty state"),
    #{flag_values => #{}};
all_flags_eval(User, Tag, online, initialized) ->
    FeatureStore = ldclient_config:get_value(Tag, feature_store),
    AllFlags = [FlagKey || {FlagKey, _} <- FeatureStore:all(Tag, features)],
    EvalFun = fun(FlagKey, Acc) ->
        {{_, V, _}, _Events} = flag_key_for_user(Tag, FlagKey, User, null),
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

-spec get_initialization_state(Tag :: atom()) -> atom().
get_initialization_state(Tag) -> get_initialization_state(Tag, ldclient:initialized(Tag)).

-spec get_initialization_state(Tag :: atom(), Initialized :: boolean()) -> atom().
get_initialization_state(_Tag, true) -> initialized;
get_initialization_state(_Tag, false) -> not_initialized.

-spec flag_recs_for_user(
    FlagKey :: ldclient_flag:key(),
    FlagRecs :: [{ldclient_flag:key(), ldclient_flag:flag()}],
    User :: ldclient_user:user(),
    FeatureStore :: atom(),
    Tag :: atom(),
    DefaultValue :: result_value()
) -> result().
flag_recs_for_user(FlagKey, [], User, _FeatureStore, _Tag, DefaultValue) ->
    % Flag not found
    error_logger:warning_msg("Unknown feature flag ~p; returning default value", [FlagKey]),
    Reason = {error, flag_not_found},
    Events = [ldclient_event:new_for_unknown_flag(FlagKey, User, DefaultValue, Reason)],
    {{null, DefaultValue, Reason}, Events};
flag_recs_for_user(FlagKey, [{FlagKey, #{deleted := true}}|_], User, _FeatureStore, _Tag, DefaultValue) ->
    % Flag found, but it's deleted
    error_logger:warning_msg("Unknown feature flag ~p; returning default value", [FlagKey]),
    Reason = {error, flag_not_found},
    Events = [ldclient_event:new_for_unknown_flag(FlagKey, User, DefaultValue, Reason)],
    {{null, DefaultValue, Reason}, Events};
flag_recs_for_user(FlagKey, [{FlagKey, Flag}|_], User, FeatureStore, Tag, DefaultValue) ->
    % Flag found
    flag_for_user_check_empty_key(Flag, User, FeatureStore, Tag, DefaultValue).

-spec flag_for_user_check_empty_key(ldclient_flag:flag(), ldclient_user:user(), atom(), atom(), result_value()) -> result().
flag_for_user_check_empty_key(Flag, #{key := <<>>} = User, FeatureStore, Tag, DefaultValue) ->
    error_logger:warning_msg("User key is blank. Flag evaluation will proceed, but the user will not be stored in Launchdarkly"),
    flag_for_user(Flag, User, FeatureStore, Tag, DefaultValue);
flag_for_user_check_empty_key(Flag, #{key := null} = User, _FeatureStore, _Tag, DefaultValue) ->
    % User has null key
    flag_for_user_with_no_key(Flag, User, DefaultValue);
flag_for_user_check_empty_key(Flag, #{key := _Key} = User, FeatureStore, Tag, DefaultValue) ->
    flag_for_user(Flag, User, FeatureStore, Tag, DefaultValue);
flag_for_user_check_empty_key(Flag, User, _FeatureStore, _Tag, DefaultValue) ->
    % User has no key
    flag_for_user_with_no_key(Flag, User, DefaultValue).

-spec flag_for_user_with_no_key(ldclient_flag:flag(), ldclient_user:user(), result_value()) -> result().
flag_for_user_with_no_key(Flag, User, DefaultValue) ->
    Reason = {error, user_not_specified},
    Events = [ldclient_event:new_flag_eval(null, DefaultValue, DefaultValue, User, Reason, Flag)],
    {{null, DefaultValue, Reason}, Events}.

-spec flag_for_user(ldclient_flag:flag(), ldclient_user:user(), atom(), atom(), result_value()) -> result().
flag_for_user(Flag, User, FeatureStore, Tag, DefaultValue) ->
    {{Variation, VariationValue, Reason}, Events} = flag_for_user_valid(Flag, User, FeatureStore, Tag, DefaultValue),
    FlagEvalEvent = ldclient_event:new_flag_eval(Variation, VariationValue, DefaultValue, User, Reason, Flag),
    {{Variation, VariationValue, Reason}, [FlagEvalEvent|Events]}.

-spec flag_for_user_valid(ldclient_flag:flag(), ldclient_user:user(), atom(), atom(), result_value()) -> result().
flag_for_user_valid(#{on := false, offVariation := OffVariation} = Flag, _User, _FeatureStore, _Tag, _DefaultValue)
    when is_integer(OffVariation), OffVariation >= 0 ->
    result_for_variation_index(OffVariation, off, Flag, []);
flag_for_user_valid(#{on := false}, _User, _FeatureStore, _Tag, DefaultValue) ->
    % offVariation is null or not set
    {{null, DefaultValue, off}, []};
flag_for_user_valid(#{prerequisites := Prerequisites} = Flag, User, FeatureStore, Tag, DefaultValue) ->
    check_prerequisites(Prerequisites, Flag, User, FeatureStore, Tag, DefaultValue).

-spec check_prerequisites([ldclient_flag:prerequisite()], ldclient_flag:flag(), ldclient_user:user(), atom(), atom(), result_value()) ->
    result().
check_prerequisites(Prerequisites, Flag, User, FeatureStore, Tag, DefaultValue) ->
    check_prerequisites(Prerequisites, Flag, User, FeatureStore, Tag, DefaultValue, []).

check_prerequisites([], Flag, User, FeatureStore, Tag, DefaultValue, Events) ->
    flag_for_user_prerequisites(success, Flag, User, FeatureStore, Tag, DefaultValue, Events);
check_prerequisites([#{key := PrerequisiteKey, variation := Variation}|Rest], Flag, User, FeatureStore, Tag, DefaultValue, Events) ->
    PrerequisiteFlagRecs = FeatureStore:get(Tag, features, PrerequisiteKey),
    check_prerequisite_recs(PrerequisiteFlagRecs, PrerequisiteKey, Variation, Rest, Flag, User, FeatureStore, Tag, DefaultValue, Events).

check_prerequisite_recs([], PrerequisiteKey, _Variation, _Prerequisites, #{key := FlagKey} = Flag, User, FeatureStore, Tag, DefaultValue, Events) ->
    % Short circuit if prerequisite flag is not found
    error_logger:error_msg("Could not retrieve prerequisite flag ~p when evaluating ~p", [PrerequisiteKey, FlagKey]),
    flag_for_user_prerequisites({fail, {prerequisite_failed, [PrerequisiteKey]}}, Flag, User, FeatureStore, Tag, DefaultValue, Events);
check_prerequisite_recs([{PrerequisiteKey, PrerequisiteFlag}|_], PrerequisiteKey, Variation, Prerequisites, Flag, User, FeatureStore, Tag, DefaultValue, Events) ->
    check_prerequisite_flag(PrerequisiteFlag, Variation, Prerequisites, Flag, User, FeatureStore, Tag, DefaultValue, Events).

check_prerequisite_flag(#{key := PrerequisiteKey, deleted := true}, _, _, Flag, User, FeatureStore, Tag, DefaultValue, Events) ->
    % Prerequisite flag is deleted, short circuit fail
    Result = {fail, {prerequisite_failed, [PrerequisiteKey]}},
    flag_for_user_prerequisites(Result, Flag, User, FeatureStore, Tag, DefaultValue, Events);
check_prerequisite_flag(#{key := PrerequisiteKey, on := false}, _, _, Flag, User, FeatureStore, Tag, DefaultValue, Events) ->
    % Prerequisite flag is off, short circuit fail
    Result = {fail, {prerequisite_failed, [PrerequisiteKey]}},
    flag_for_user_prerequisites(Result, Flag, User, FeatureStore, Tag, DefaultValue, Events);
check_prerequisite_flag(#{prerequisites := SubPrerequisites} = PrerequisiteFlag, Variation, Prerequisites, #{key := FlagKey} = Flag, User, FeatureStore, Tag, DefaultValue, Events) ->
    {{ResultVariation, ResultVariationValue, ResultReason}, ResultEvents} = check_prerequisites(SubPrerequisites, PrerequisiteFlag, User, FeatureStore, Tag, DefaultValue, Events),
    NewEvents = [ldclient_event:new_prerequisite_eval(ResultVariation, ResultVariationValue, FlagKey, User, ResultReason, PrerequisiteFlag)|ResultEvents],
    check_prerequisite_flag_result(PrerequisiteFlag, Variation =:= ResultVariation, Prerequisites, Flag, User, FeatureStore, Tag, DefaultValue, NewEvents).

check_prerequisite_flag_result(#{key := PrerequisiteKey}, false, _Prerequisites, Flag, User, FeatureStore, Tag, DefaultValue, Events) ->
    % Prerequisite flag variation didn't match: short-circuit fail and move on
    Result = {fail, {prerequisite_failed, [PrerequisiteKey]}},
    flag_for_user_prerequisites(Result, Flag, User, FeatureStore, Tag, DefaultValue, Events);
check_prerequisite_flag_result(_PrerequisiteFlag, true, Prerequisites, Flag, User, FeatureStore, Tag, DefaultValue, Events) ->
    check_prerequisites(Prerequisites, Flag, User, FeatureStore, Tag, DefaultValue, Events).

flag_for_user_prerequisites({fail, Reason}, #{offVariation := OffVariation} = Flag, _User, _FeatureStore, _Tag, _DefaultValue, Events)
    when is_integer(OffVariation), OffVariation >= 0 ->
    result_for_variation_index(OffVariation, Reason, Flag, Events);
flag_for_user_prerequisites({fail, Reason}, _Flag, _User, _FeatureStore, _Tag, DefaultValue, Events) ->
    % prerequisite failed, but offVariation is null or not set
    {{null, DefaultValue, Reason}, Events};
flag_for_user_prerequisites(success, #{targets := Targets} = Flag, User, FeatureStore, Tag, _DefaultValue, Events) ->
    check_targets(Targets, Flag, User, FeatureStore, Tag, Events).

check_targets([], Flag, User, FeatureStore, Tag, Events) ->
    flag_for_user_targets(no_match, Flag, User, FeatureStore, Tag, Events);
check_targets([#{values := Values, variation := Variation}|Rest], Flag, #{key := UserKey} = User, FeatureStore, Tag, Events) ->
    Result = {lists:member(UserKey, Values), Variation},
    check_target_result(Result, Rest, Flag, User, FeatureStore, Tag, Events).

check_target_result({false, _}, Rest, Flag, User, FeatureStore, Tag, Events) ->
    check_targets(Rest, Flag, User, FeatureStore, Tag, Events);
check_target_result({true, Variation}, _Rest, Flag, User, FeatureStore, Tag, Events) ->
    % Target matched: short-circuit
    flag_for_user_targets({match, Variation}, Flag, User, FeatureStore, Tag, Events).

flag_for_user_targets({match, Variation}, Flag, _User, _FeatureStore, _Tag, Events) ->
    Reason = target_match,
    result_for_variation_index(Variation, Reason, Flag, Events);
flag_for_user_targets(no_match, #{rules := Rules} = Flag, User, FeatureStore, Tag, Events) ->
    check_rules(Rules, Flag, User, FeatureStore, Tag, Events, 0).

check_rules([], Flag, User, _FeatureStore, _Tag, Events, _) ->
    flag_for_user_rules(no_match, Flag, User, Events);
check_rules([Rule|Rest], Flag, User, FeatureStore, Tag, Events, Index) ->
    Result = ldclient_rule:match_user(Rule, User, FeatureStore, Tag),
    check_rule_result({Result, Rule, Index}, Rest, Flag, User, FeatureStore, Tag, Events).

check_rule_result({no_match, _Rule, Index}, Rest, Flag, User, FeatureStore, Tag, Events) ->
    check_rules(Rest, Flag, User, FeatureStore, Tag, Events, Index + 1);
check_rule_result({match, Rule, Index}, _Rest, Flag, User, _FeatureStore, _Tag, Events) ->
    % Rule matched: short-circuit
    flag_for_user_rules({match, Rule, Index}, Flag, User, Events).

flag_for_user_rules({match, #{id := Id, variationOrRollout := VorR}, Index}, Flag, User, Events) ->
    Reason = {rule_match, Index, Id},
    flag_for_user_variation_or_rollout(VorR, Reason, Flag, User, Events);
flag_for_user_rules(no_match, #{fallthrough := Fallthrough} = Flag, User, Events) ->
    flag_for_user_variation_or_rollout(Fallthrough, fallthrough, Flag, User, Events).

flag_for_user_variation_or_rollout(Variation, Reason, Flag, _User, Events) when is_integer(Variation) ->
    result_for_variation_index(Variation, Reason, Flag, Events);
flag_for_user_variation_or_rollout(Rollout, Reason, Flag, User, Events) when is_map(Rollout) ->
    Result = ldclient_rollout:rollout_user(Rollout, Flag, User),
    flag_for_user_rollout_result(Result, Reason, Flag, Events).

flag_for_user_rollout_result(null, _Reason, #{key := FlagKey}, Events) ->
    error_logger:warning_msg("Data inconsistency in feature flag ~p: variation/rollout object with no variation or rollout", [FlagKey]),
    Reason = {error, malformed_flag},
    {{null, null, Reason}, Events};
flag_for_user_rollout_result(Variation, Reason, Flag, Events) ->
    result_for_variation_index(Variation, Reason, Flag, Events).

result_for_variation_index(Variation, Reason, Flag, Events) ->
    VariationValue = ldclient_flag:get_variation(Flag, Variation),
    result_for_variation_value(VariationValue, Variation, Reason, Flag, Events).

result_for_variation_value(null, _Variation, _Reason, _Flag, Events) ->
    Reason = {error, malformed_flag},
    {{null, null, Reason}, Events};
result_for_variation_value(VariationValue, Variation, Reason, _Flag, Events) ->
    {{Variation, VariationValue, Reason}, Events}.
