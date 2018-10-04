%%%-------------------------------------------------------------------
%%% @doc Rollout data type
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(eld_rollout).

%% API
-export([new/1]).
-export([rollout_user/2]).

%% Types
-type rollout() :: #{
    variations => [weighted_variation()],
    bucket_by  => eld_user:attribute()
}.
%% Describes how users will be bucketed into variations during a percentage rollout

-type weighted_variation() :: #{
    variation => eld_flag:variation(),
    weight    => non_neg_integer() % 0 to 100000
}.
%% Describes a fraction of users who will receive a specific variation

-export_type([rollout/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(map()) -> rollout().
new(#{
    <<"variations">> := Variations,
    <<"bucket_by">>  := BucketBy
}) ->
    #{
        variations => parse_variations(Variations),
        bucket_by  => BucketBy
    };
new(#{<<"variations">> := Variations}) ->
    #{
        variations => parse_variations(Variations),
        bucket_by  => key
    }.

-spec rollout_user(rollout(), eld_user:user()) -> eld_flag:variation().
rollout_user(_Rollout, _User) ->
    % TODO implement
    0.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec parse_variations([map()]) -> [weighted_variation()].
parse_variations(Variations) ->
    F = fun(#{<<"variation">> := Variation, <<"weight">> := Weight}) ->
            #{variation => Variation, weight => Weight}
        end,
    lists:map(F, Variations).
