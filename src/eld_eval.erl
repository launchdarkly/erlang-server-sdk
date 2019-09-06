%%-------------------------------------------------------------------
%% @doc Evaluation
%%
%% @end
%%-------------------------------------------------------------------

-module(eld_eval).

%% API
-export([flag_key_for_user/4]).

%% Types
-type result() :: {
    Detail :: detail(),
    Events :: [eld_event:event()]
}.
-type detail() :: {
    VariationIndex :: variation_index(),
    Value :: result_value(),
    Reason :: reason()
}.
-type result_value() :: null | eld_flag:variation_value().
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

-export_type([detail/0]).
-export_type([reason/0]).
-export_type([result_value/0]).

%%===================================================================
%% API
%%===================================================================

%% @doc Evaluates feature flag for a given user
%%
%% @end
-spec flag_key_for_user(
    Tag :: atom(),
    FlagKey :: eld_flag:key(),
    User :: eld_user:user(),
    DefaultValue :: result_value()
) -> result().
flag_key_for_user(Tag, FlagKey, User, DefaultValue) ->
    StorageBackend = eld_settings:get_value(Tag, storage_backend),
    FlagRecs = StorageBackend:get(Tag, flags, FlagKey),
    flag_recs_for_user(FlagKey, FlagRecs, User, StorageBackend, Tag, DefaultValue).

%%===================================================================
%% Internal functions
%%===================================================================

-spec flag_recs_for_user(
    FlagKey :: eld_flag:key(),
    FlagRecs :: [{eld_flag:key(), map()}],
    User :: eld_user:user(),
    StorageBackend :: atom(),
    Tag :: atom(),
    DefaultValue :: result_value()
) -> result().
flag_recs_for_user(FlagKey, [], User, _StorageBackend, _Tag, DefaultValue) ->
    % Flag not found
    error_logger:warning_msg("Unknown feature flag ~p; returning default value", [FlagKey]),
    Reason = {error, flag_not_found},
    Events = [eld_event:new_for_unknown_flag(FlagKey, User, DefaultValue, Reason)],
    {{null, DefaultValue, Reason}, Events};
flag_recs_for_user(FlagKey,[{FlagKey, #{<<"on">> := false}} | _], User, _StorageBackend, _Tag, DefaultValue) ->
    % Flag found, but it's archived/not "on"
    error_logger:warning_msg("Archived feature flag ~p; returning default value", [FlagKey]),
    Reason = {error, flag_not_found},
    Events = [eld_event:new_for_unknown_flag(FlagKey, User, DefaultValue, Reason)],
    {{null, DefaultValue, Reason}, Events};
flag_recs_for_user(FlagKey, [{FlagKey, #{<<"deleted">> := true}}|_], User, _StorageBackend, _Tag, DefaultValue) ->
    % Flag found, but it's deleted
    error_logger:warning_msg("Deleted feature flag ~p; returning default value", [FlagKey]),
    Reason = {error, flag_not_found},
    Events = [eld_event:new_for_unknown_flag(FlagKey, User, DefaultValue, Reason)],
    {{null, DefaultValue, Reason}, Events};
flag_recs_for_user(FlagKey, [{FlagKey, FlagProperties}|_], User, StorageBackend, Tag, DefaultValue) ->
    % Flag found
    Flag = eld_flag:new(FlagKey, FlagProperties),
    flag_for_user_check_empty_key(Flag, User, StorageBackend, Tag, DefaultValue).

-spec flag_for_user_check_empty_key(eld_flag:flag(), eld_user:user(), atom(), atom(), result_value()) -> result().
flag_for_user_check_empty_key(Flag, #{key := <<>>} = User, StorageBackend, Tag, DefaultValue) ->
    error_logger:warning_msg("User key is blank. Flag evaluation will proceed, but the user will not be stored in Launchdarkly"),
    flag_for_user(Flag, User, StorageBackend, Tag, DefaultValue);
flag_for_user_check_empty_key(Flag, #{key := null} = User, _StorageBackend, _Tag, DefaultValue) ->
    % User has null key
    flag_for_user_with_no_key(Flag, User, DefaultValue);
flag_for_user_check_empty_key(Flag, #{key := _Key} = User, StorageBackend, Tag, DefaultValue) ->
    flag_for_user(Flag, User, StorageBackend, Tag, DefaultValue);
flag_for_user_check_empty_key(Flag, User, _StorageBackend, _Tag, DefaultValue) ->
    % User has no key
    flag_for_user_with_no_key(Flag, User, DefaultValue).

-spec flag_for_user_with_no_key(eld_flag:flag(), eld_user:user(), result_value()) -> result().
flag_for_user_with_no_key(Flag, User, DefaultValue) ->
    Reason = {error, user_not_specified},
    Events = [eld_event:new_flag_eval(null, DefaultValue, DefaultValue, User, Reason, Flag)],
    {{null, DefaultValue, Reason}, Events}.

-spec flag_for_user(eld_flag:flag(), eld_user:user(), atom(), atom(), result_value()) -> result().
flag_for_user(Flag, User, StorageBackend, Tag, DefaultValue) ->
    {{Variation, VariationValue, Reason}, Events} = flag_for_user_valid(Flag, User, StorageBackend, Tag, DefaultValue),
    FlagEvalEvent = eld_event:new_flag_eval(Variation, VariationValue, DefaultValue, User, Reason, Flag),
    {{Variation, VariationValue, Reason}, [FlagEvalEvent|Events]}.

-spec flag_for_user_valid(eld_flag:flag(), eld_user:user(), atom(), atom(), result_value()) -> result().
flag_for_user_valid(#{on := false, off_variation := OffVariation} = Flag, _User, _StorageBackend, _Tag, _DefaultValue)
    when is_integer(OffVariation), OffVariation >= 0 ->
    result_for_variation_index(OffVariation, off, Flag, []);
flag_for_user_valid(#{on := false}, _User, _StorageBackend, _Tag, DefaultValue) ->
    % off_variation is null or not set
    {{null, DefaultValue, off}, []};
flag_for_user_valid(#{prerequisites := Prerequisites} = Flag, User, StorageBackend, Tag, DefaultValue) ->
    check_prerequisites(Prerequisites, Flag, User, StorageBackend, Tag, DefaultValue).

-spec check_prerequisites([eld_flag:prerequisite()], eld_flag:flag(), eld_user:user(), atom(), atom(), result_value()) ->
    result().
check_prerequisites(Prerequisites, Flag, User, StorageBackend, Tag, DefaultValue) ->
    check_prerequisites(Prerequisites, Flag, User, StorageBackend, Tag, DefaultValue, []).

check_prerequisites([], Flag, User, StorageBackend, Tag, DefaultValue, Events) ->
    flag_for_user_prerequisites(success, Flag, User, StorageBackend, Tag, DefaultValue, Events);
check_prerequisites([#{key := PrerequisiteKey, variation := Variation}|Rest], Flag, User, StorageBackend, Tag, DefaultValue, Events) ->
    PrerequisiteFlagRecs = StorageBackend:get(Tag, flags, PrerequisiteKey),
    check_prerequisite_recs(PrerequisiteFlagRecs, PrerequisiteKey, Variation, Rest, Flag, User, StorageBackend, Tag, DefaultValue, Events).

check_prerequisite_recs([], PrerequisiteKey, _Variation, _Prerequisites, #{key := FlagKey} = Flag, User, StorageBackend, Tag, DefaultValue, Events) ->
    % Short circuit if prerequisite flag is not found
    error_logger:error_msg("Could not retrieve prerequisite flag ~p when evaluating ~p", [PrerequisiteKey, FlagKey]),
    flag_for_user_prerequisites({fail, {prerequisite_failed, [PrerequisiteKey]}}, Flag, User, StorageBackend, Tag, DefaultValue, Events);
check_prerequisite_recs([{PrerequisiteKey, PrerequisiteProperties}|_], PrerequisiteKey, Variation, Prerequisites, Flag, User, StorageBackend, Tag, DefaultValue, Events) ->
    PrerequisiteFlag = eld_flag:new(PrerequisiteKey, PrerequisiteProperties),
    check_prerequisite_flag(PrerequisiteFlag, Variation, Prerequisites, Flag, User, StorageBackend, Tag, DefaultValue, Events).

check_prerequisite_flag(#{key := PrerequisiteKey, deleted := true}, _, _, Flag, User, StorageBackend, Tag, DefaultValue, Events) ->
    % Prerequisite flag is deleted, short circuit fail
    Result = {fail, {prerequisite_failed, [PrerequisiteKey]}},
    flag_for_user_prerequisites(Result, Flag, User, StorageBackend, Tag, DefaultValue, Events);
check_prerequisite_flag(#{key := PrerequisiteKey, on := false}, _, _, Flag, User, StorageBackend, Tag, DefaultValue, Events) ->
    % Prerequisite flag is off, short circuit fail
    Result = {fail, {prerequisite_failed, [PrerequisiteKey]}},
    flag_for_user_prerequisites(Result, Flag, User, StorageBackend, Tag, DefaultValue, Events);
check_prerequisite_flag(#{prerequisites := SubPrerequisites} = PrerequisiteFlag, Variation, Prerequisites, #{key := FlagKey} = Flag, User, StorageBackend, Tag, DefaultValue, Events) ->
    {{ResultVariation, ResultVariationValue, ResultReason}, ResultEvents} = check_prerequisites(SubPrerequisites, PrerequisiteFlag, User, StorageBackend, Tag, DefaultValue, Events),
    NewEvents = [eld_event:new_prerequisite_eval(ResultVariation, ResultVariationValue, FlagKey, User, ResultReason, PrerequisiteFlag)|ResultEvents],
    check_prerequisite_flag_result(PrerequisiteFlag, Variation =:= ResultVariation, Prerequisites, Flag, User, StorageBackend, Tag, DefaultValue, NewEvents).

check_prerequisite_flag_result(#{key := PrerequisiteKey}, false, _Prerequisites, Flag, User, StorageBackend, Tag, DefaultValue, Events) ->
    % Prerequisite flag variation didn't match: short-circuit fail and move on
    Result = {fail, {prerequisite_failed, [PrerequisiteKey]}},
    flag_for_user_prerequisites(Result, Flag, User, StorageBackend, Tag, DefaultValue, Events);
check_prerequisite_flag_result(_PrerequisiteFlag, true, Prerequisites, Flag, User, StorageBackend, Tag, DefaultValue, Events) ->
    check_prerequisites(Prerequisites, Flag, User, StorageBackend, Tag, DefaultValue, Events).

flag_for_user_prerequisites({fail, Reason}, #{off_variation := OffVariation} = Flag, _User, _StorageBackend, _Tag, _DefaultValue, Events)
    when is_integer(OffVariation), OffVariation >= 0 ->
    result_for_variation_index(OffVariation, Reason, Flag, Events);
flag_for_user_prerequisites({fail, Reason}, _Flag, _User, _StorageBackend, _Tag, DefaultValue, Events) ->
    % prerequisite failed, but off_variation is null or not set
    {{null, DefaultValue, Reason}, Events};
flag_for_user_prerequisites(success, #{targets := Targets} = Flag, User, StorageBackend, Tag, _DefaultValue, Events) ->
    check_targets(Targets, Flag, User, StorageBackend, Tag, Events).

check_targets([], Flag, User, StorageBackend, Tag, Events) ->
    flag_for_user_targets(no_match, Flag, User, StorageBackend, Tag, Events);
check_targets([#{values := Values, variation := Variation}|Rest], Flag, #{key := UserKey} = User, StorageBackend, Tag, Events) ->
    Result = {lists:member(UserKey, Values), Variation},
    check_target_result(Result, Rest, Flag, User, StorageBackend, Tag, Events).

check_target_result({false, _}, Rest, Flag, User, StorageBackend, Tag, Events) ->
    check_targets(Rest, Flag, User, StorageBackend, Tag, Events);
check_target_result({true, Variation}, _Rest, Flag, User, StorageBackend, Tag, Events) ->
    % Target matched: short-circuit
    flag_for_user_targets({match, Variation}, Flag, User, StorageBackend, Tag, Events).

flag_for_user_targets({match, Variation}, Flag, _User, _StorageBackend, _Tag, Events) ->
    Reason = target_match,
    result_for_variation_index(Variation, Reason, Flag, Events);
flag_for_user_targets(no_match, #{rules := Rules} = Flag, User, StorageBackend, Tag, Events) ->
    check_rules(Rules, Flag, User, StorageBackend, Tag, Events, 0).

check_rules([], Flag, User, _StorageBackend, _Tag, Events, _) ->
    flag_for_user_rules(no_match, Flag, User, Events);
check_rules([Rule|Rest], Flag, User, StorageBackend, Tag, Events, Index) ->
    Result = eld_rule:match_user(Rule, User, StorageBackend, Tag),
    check_rule_result({Result, Rule, Index}, Rest, Flag, User, StorageBackend, Tag, Events).

check_rule_result({no_match, _Rule, Index}, Rest, Flag, User, StorageBackend, Tag, Events) ->
    check_rules(Rest, Flag, User, StorageBackend, Tag, Events, Index + 1);
check_rule_result({match, Rule, Index}, _Rest, Flag, User, _StorageBackend, _Tag, Events) ->
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

flag_for_user_rollout_result(null, _Reason, #{key := FlagKey}, Events) ->
    error_logger:warning_msg("Data inconsistency in feature flag ~p: variation/rollout object with no variation or rollout", [FlagKey]),
    Reason = {error, malformed_flag},
    {{null, null, Reason}, Events};
flag_for_user_rollout_result(Variation, Reason, Flag, Events) ->
    result_for_variation_index(Variation, Reason, Flag, Events).

result_for_variation_index(Variation, Reason, Flag, Events) ->
    VariationValue = eld_flag:get_variation(Flag, Variation),
    result_for_variation_value(VariationValue, Variation, Reason, Flag, Events).

result_for_variation_value(null, _Variation, _Reason, _Flag, Events) ->
    Reason = {error, malformed_flag},
    {{null, null, Reason}, Events};
result_for_variation_value(VariationValue, Variation, Reason, _Flag, Events) ->
    {{Variation, VariationValue, Reason}, Events}.
