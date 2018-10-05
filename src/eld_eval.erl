%%%-------------------------------------------------------------------
%%% @doc Evaluation
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(eld_eval).

%% API
-export([flag_key_for_user/3]).

%% Types
-type result() :: {
    Detail :: detail(),
    Events :: [eld_event:event()]
}.
-type detail() :: {
    VariationIndex :: variation_index(),
    Value :: value(),
    Reason :: reason()
}.
-type value() :: undefined | eld_flag:variation_value().
-type variation_index() :: undefined | non_neg_integer().
-type reason() ::
    target_match
    | {rule_match, RuleIndex :: non_neg_integer(), RuleUUID :: binary()}
    | {prerequisite_failed, FlagKeys :: [binary()]}
    | {error, error_type()}
    | fallthrough
    | off.

-type error_type() :: client_not_ready | flag_not_found | malformed_flag
    | user_not_specified | wrong_type | exception.

-export_type([detail/0]).
-export_type([reason/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Evaluates feature flag for a given user
%%
%% @end
-spec flag_key_for_user(
    FlagKey :: binary(),
    User :: eld_user:user(),
    DefaultValue :: eld_flag:variation_value()
) -> result().
flag_key_for_user(FlagKey, User, DefaultValue) ->
    {ok, StorageBackend} = eld_app:get_env(storage_backend),
    FlagRecs = StorageBackend:get(flags, FlagKey),
    flag_recs_for_user(FlagKey, FlagRecs, User, StorageBackend, DefaultValue).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec flag_recs_for_user(
    FlagKey :: eld_flag:key(),
    FlagRecs :: [{eld_flag:key(), map()}],
    User :: eld_user:user(),
    StorageBackend :: atom(),
    DefaultValue :: eld_flag:variation_value()
) -> result().
flag_recs_for_user(FlagKey, [], User, _StorageBackend, DefaultValue) ->
    % Flag not found
    error_logger:warning_msg("Unknown feature flag ~p; returning default value", [FlagKey]),
    Reason = {error, flag_not_found},
    Events = [eld_event:new_for_unknown_flag(FlagKey, User, DefaultValue, Reason)],
    {{undefined, DefaultValue, Reason}, Events};
flag_recs_for_user(FlagKey, [{FlagKey, FlagProperties}|_], User, StorageBackend, DefaultValue) ->
    % Flag found
    Flag = eld_flag:new(FlagKey, FlagProperties),
    flag_for_user_check_empty_key(Flag, User, StorageBackend, DefaultValue).

-spec flag_for_user_check_empty_key(eld_flag:flag(), eld_user:user(), atom(), eld_flag:variation_value()) -> result().
flag_for_user_check_empty_key(Flag, #{key := <<>>} = User, StorageBackend, DefaultValue) ->
    error_logger:warning_msg("User key is blank. Flag evaluation will proceed, but the user will not be stored in Launchdarkly"),
    flag_for_user(Flag, User, StorageBackend, DefaultValue);
flag_for_user_check_empty_key(Flag, User, StorageBackend, DefaultValue) ->
    flag_for_user(Flag, User, StorageBackend, DefaultValue).

-spec flag_for_user(eld_flag:flag(), eld_user:user(), atom(), eld_flag:variation_value()) -> result().
flag_for_user(Flag, User, StorageBackend, DefaultValue) ->
    {{Variation, VariationValue, Reason}, Events} = flag_for_user_valid(Flag, User, StorageBackend),
    FlagEvalEvent = eld_event:new_flag_eval(Variation, VariationValue, DefaultValue, User, Reason, Flag),
    {{Variation, VariationValue, Reason}, [FlagEvalEvent|Events]}.

-spec flag_for_user_valid(eld_flag:flag(), eld_user:user(), atom()) -> result().
flag_for_user_valid(#{on := false, off_variation := OffVariation} = Flag, _User, _StorageBackend) ->
    result_for_variation_index(OffVariation, off, Flag, []);
flag_for_user_valid(#{prerequisites := Prerequisites} = Flag, User, StorageBackend) ->
    check_prerequisites(Prerequisites, Flag, User, StorageBackend).

-spec check_prerequisites([eld_flag:prerequisite()], eld_flag:flag(), eld_user:user(), atom()) ->
    result().
check_prerequisites(Prerequisites, Flag, User, StorageBackend) ->
    check_prerequisites(Prerequisites, Flag, User, StorageBackend, []).

check_prerequisites([], Flag, User, StorageBackend, Events) ->
    flag_for_user_prerequisites(success, Flag, User, StorageBackend, Events);
check_prerequisites([#{key := PrerequisiteKey, variation := Variation}|Rest], Flag, User, StorageBackend, Events) ->
    PrerequisiteFlagRecs = StorageBackend:get(flags, PrerequisiteKey),
    check_prerequisite_recs(PrerequisiteFlagRecs, PrerequisiteKey, Variation, Rest, Flag, User, StorageBackend, Events).

check_prerequisite_recs([], PrerequisiteKey, _Variation, _Prerequisites, #{key := FlagKey} = Flag, User, StorageBackend, Events) ->
    % Short circuit if prerequisite flag is not found
    error_logger:error_msg("Could not retrieve prerequisite flag ~p when evaluating ~p", [PrerequisiteKey, FlagKey]),
    flag_for_user_prerequisites({fail, {prerequisite_failed, [PrerequisiteKey]}}, Flag, User, StorageBackend, Events);
check_prerequisite_recs([{PrerequisiteKey, PrerequisiteProperties}|_], PrerequisiteKey, Variation, Prerequisites, Flag, User, StorageBackend, Events) ->
    PrerequisiteFlag = eld_flag:new(PrerequisiteKey, PrerequisiteProperties),
    check_prerequisite_flag(PrerequisiteFlag, Variation, Prerequisites, Flag, User, StorageBackend, Events).

check_prerequisite_flag(#{prerequisites := SubPrerequisites} = PrerequisiteFlag, Variation, Prerequisites, #{key := FlagKey} = Flag, User, StorageBackend, Events) ->
    {{ResultVariation, ResultVariationValue, ResultReason}, ResultEvents} = check_prerequisites(SubPrerequisites, PrerequisiteFlag, User, StorageBackend, Events),
    NewEvents = [eld_event:new_prerequisite_eval(ResultVariation, ResultVariationValue, FlagKey, User, ResultReason, PrerequisiteFlag)|ResultEvents],
    check_prerequisite_flag_result(PrerequisiteFlag, Variation =:= ResultVariation, Prerequisites, Flag, User, StorageBackend, NewEvents).

check_prerequisite_flag_result(#{key := PrerequisiteKey, on := false}, _, _Prerequisites, Flag, User, StorageBackend, Events) ->
    % Prerequisite flag is off: short-circuit fail and move on
    Result = {fail, {prerequisite_failed, [PrerequisiteKey]}},
    flag_for_user_prerequisites(Result, Flag, User, StorageBackend, Events);
check_prerequisite_flag_result(#{key := PrerequisiteKey}, false, _Prerequisites, Flag, User, StorageBackend, Events) ->
    % Prerequisite flag variation didn't match: short-circuit fail and move on
    Result = {fail, {prerequisite_failed, [PrerequisiteKey]}},
    flag_for_user_prerequisites(Result, Flag, User, StorageBackend, Events);
check_prerequisite_flag_result(_PrerequisiteFlag, true, Prerequisites, Flag, User, StorageBackend, Events) ->
    check_prerequisites(Prerequisites, Flag, User, StorageBackend, Events).

flag_for_user_prerequisites({fail, Reason}, #{off_variation := OffVariation} = Flag, _User, _StorageBackend, Events) ->
    result_for_variation_index(OffVariation, Reason, Flag, Events);
flag_for_user_prerequisites(success, #{targets := Targets} = Flag, User, StorageBackend, Events) ->
    check_targets(Targets, Flag, User, StorageBackend, Events).

check_targets([], Flag, User, StorageBackend, Events) ->
    flag_for_user_targets(no_match, Flag, User, StorageBackend, Events);
check_targets([#{values := Values, variation := Variation}|Rest], Flag, #{key := UserKey} = User, StorageBackend, Events) ->
    Result = {lists:member(UserKey, Values), Variation},
    check_target_result(Result, Rest, Flag, User, StorageBackend, Events).

check_target_result({false, _}, Rest, Flag, User, StorageBackend, Events) ->
    check_targets(Rest, Flag, User, StorageBackend, Events);
check_target_result({true, Variation}, _Rest, Flag, User, StorageBackend, Events) ->
    % Target matched: short-circuit
    flag_for_user_targets({match, Variation}, Flag, User, StorageBackend, Events).

flag_for_user_targets({match, Variation}, Flag, _User, _StorageBackend, Events) ->
    Reason = target_match,
    result_for_variation_index(Variation, Reason, Flag, Events);
flag_for_user_targets(no_match, #{rules := Rules} = Flag, User, StorageBackend, Events) ->
    check_rules(Rules, Flag, User, StorageBackend, Events, 0).

check_rules([], Flag, User, _StorageBackend, Events, _) ->
    flag_for_user_rules(no_match, Flag, User, Events);
check_rules([Rule|Rest], Flag, User, StorageBackend, Events, Index) ->
    Result = eld_rule:match_user(Rule, User, StorageBackend),
    check_rule_result({Result, Rule, Index}, Rest, Flag, User, StorageBackend, Events).

check_rule_result({no_match, _Rule, Index}, Rest, Flag, User, StorageBackend, Events) ->
    check_rules(Rest, Flag, User, StorageBackend, Events, Index + 1);
check_rule_result({match, Rule, Index}, _Rest, Flag, User, _StorageBackend, Events) ->
    % Rule matched: short-circuit
    flag_for_user_rules({match, Rule, Index}, Flag, User, Events).

flag_for_user_rules({match, #{id := Id, variation_or_rollout := VorR}, Index}, Flag, User, Events) ->
    Reason = {rule_match, Index, Id},
    flag_for_user_variation_or_rollout(VorR, Reason, Flag, User, Events);
flag_for_user_rules(no_match, #{fallthrough := Fallthrough} = Flag, User, Events) ->
    flag_for_user_variation_or_rollout(Fallthrough, fallthrough, Flag, User, Events).

flag_for_user_variation_or_rollout(Variation, Reason, Flag, _User, Events) when is_integer(Variation) ->
    result_for_variation_index(Variation, Reason, Flag, Events);
flag_for_user_variation_or_rollout(Rollout, Reason, Flag, User, Events) when is_map(Rollout) ->
    Result = eld_rollout:rollout_user(Rollout, Flag, User),
    flag_for_user_rollout_result(Result, Reason, Flag, Events).

flag_for_user_rollout_result(undefined, _Reason, #{key := FlagKey}, Events) ->
    error_logger:warning_msg("Data inconsistency in feature flag ~p: variation/rollout object with no variation or rollout", [FlagKey]),
    Reason = {error, malformed_flag},
    {{undefined, undefined, Reason}, Events};
flag_for_user_rollout_result(Variation, Reason, Flag, Events) ->
    result_for_variation_index(Variation, Reason, Flag, Events).

result_for_variation_index(Variation, Reason, Flag, Events) ->
    VariationValue = eld_flag:get_variation(Flag, Variation),
    result_for_variation_value(VariationValue, Variation, Reason, Flag, Events).

result_for_variation_value(undefined, _Variation, _Reason, _Flag, Events) ->
    Reason = {error, malformed_flag},
    {{undefined, undefined, Reason}, Events};
result_for_variation_value(VariationValue, Variation, Reason, _Flag, Events) ->
    {{Variation, VariationValue, Reason}, Events}.
