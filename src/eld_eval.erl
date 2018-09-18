%%%-------------------------------------------------------------------
%%% @doc Evaluation
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(eld_eval).

%% API
-export([flag_key_for_user/2]).

%% Types
-type detail() :: {
    Value :: value(),
    VariationIndex :: variation_index(),
    Reason :: reason(),
    Events :: [eld_event:event()]
    % TODO(1) return error(s) here, or log errors individually inline?
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

%%%===================================================================
%%% API
%%%===================================================================

-spec flag_key_for_user(FlagKey :: binary(), eld_user:user()) -> detail().
flag_key_for_user(FlagKey, User) ->
    {ok, StorageBackend} = eld_app:get_env(storage_backend),
    FlagRecs = StorageBackend:get(flags, FlagKey),
    flag_recs_for_user(FlagKey, FlagRecs, User).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec flag_recs_for_user(FlagKey :: eld_flag:key(), FlagRecs :: [tuple()], User :: eld_user:user()) ->
    detail().
flag_recs_for_user(FlagKey, [], User) ->
    % Flag not found
    % TODO log or return error
    EventData = #{
        key                     => FlagKey,
        variation               => undefined,
        value                   => undefined,
        default                 => undefined,
        version                 => undefined,
        prereq_of               => undefined,
        track_events            => undefined,
        debug_events_until_date => undefined,
        eval_reason             => undefined,
        debug                   => undefined
    },
    Events = [eld_event:new(feature_request, User, erlang:system_time(), EventData)],
    Reason = {error, flag_not_found},
    {undefined, undefined, Reason, Events};
flag_recs_for_user(_FlagKey, [{FlagKey, FlagProperties}|_], User) ->
    % Flag found
    Flag = eld_flag:new(FlagKey, FlagProperties),
    flag_for_user(Flag, User).

-spec flag_for_user(eld_flag:flag(), eld_user:user()) -> detail().
flag_for_user(#{on := false, off_variation := OffVariation} = Flag, _User) ->
    % TODO return detail
    eld_flag:get_variation(Flag, OffVariation).
