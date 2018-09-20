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
    % TODO(1) return error(s) here, or log errors individually inline?
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
    flag_recs_for_user(FlagKey, FlagRecs, User, DefaultValue).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec flag_recs_for_user(
    FlagKey :: eld_flag:key(),
    FlagRecs :: [{eld_flag:key(), map()}],
    User :: eld_user:user(),
    DefaultValue :: eld_flag:variation_value()
) -> result().
flag_recs_for_user(FlagKey, [], User, DefaultValue) ->
    % Flag not found
    % TODO log or return error
    Reason = {error, flag_not_found},
    Events = [eld_event:new_for_unknown_flag(FlagKey, User, DefaultValue, Reason)],
    {{undefined, DefaultValue, Reason}, Events};
flag_recs_for_user(FlagKey, [{FlagKey, FlagProperties}|_], User, DefaultValue) ->
    % Flag found
    Flag = eld_flag:new(FlagKey, FlagProperties),
    flag_for_user(Flag, User, DefaultValue).

-spec flag_for_user(eld_flag:flag(), eld_user:user(), eld_flag:variation_value()) -> result().
flag_for_user(#{on := false, off_variation := OffVariation} = Flag, User, DefaultValue) ->
    VariationValue = eld_flag:get_variation(Flag, OffVariation),
    Reason = off,
    Events = [eld_event:new_flag_eval(OffVariation, VariationValue, DefaultValue, User, Reason, Flag)],
    {{OffVariation, VariationValue, Reason}, Events}.
