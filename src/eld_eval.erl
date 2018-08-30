%%%-------------------------------------------------------------------
%%% @doc Evaluation
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(eld_eval).

%% API
-export([flag_key_for_user/2]).

%% Types
-type result() :: {
    Value :: value(),
    VariationIndex :: variation_index(),
    Detail :: detail(),
    Events :: [event()]
    % TODO(1) return error(s) here, or log errors individually inline?
}.
-type value() :: undefined | boolean() | float() | integer() | binary().
-type variation_index() :: undefined | non_neg_integer(). % TODO 0-based?
-type detail() :: {
    target_match |
    {rule_match, RuleIndex :: non_neg_integer(), RuleUUID :: binary()} |
    {prerequisite_failed, FlagKeys :: [binary()]} |
    {error, error_type()} |
    fallthrough |
    off
}.
-type error_type() :: client_not_ready | flag_not_found | user_not_specified | exception.
-type event() :: tuple(). % TODO define event type

%%%===================================================================
%%% API
%%%===================================================================

-spec flag_key_for_user(FlagKey :: binary(), eld_user:user()) -> result().
flag_key_for_user(FlagKey, User) ->
    FlagRecs = eld_storage_server:get(flags, FlagKey),
    flag_recs_for_user(FlagKey, FlagRecs, User).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec flag_recs_for_user(FlagKey :: binary(), FlagRecs :: [tuple()], User :: eld_user:user()) ->
    result().
flag_recs_for_user(FlagKey, [], User) ->
    % Flag not found
    % TODO log or return error
    EventData = #{
        key       => FlagKey,
        variation => undefined,
        value     => undefined,
        default   => undefined,
        version   => undefined,
        prereq_of => undefined
    },
    Events = [eld_event:new(feature, User, erlang:system_time(), EventData)],
    % TODO return proper detail here
    Detail = {flag_not_found, FlagKey},
    {undefined, undefined, Detail, Events};
flag_recs_for_user(_FlagKey, [FlagRec|_], User) ->
    % Flag found
    Flag = eld_flag:new(FlagRec),
    flag_for_user(Flag, User).

-spec flag_for_user(eld_flag:flag(), eld_user:user()) -> result().
flag_for_user(#{on := false, off_variation := OffVariation} = Flag, _User) ->
    eld_flag:get_variation(Flag, OffVariation).
