%%-------------------------------------------------------------------
%% @doc Rollout data type
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_rollout).

%% API
-export([new/1]).
-export([bucket_user/4]).
-export([rollout_user/3]).

%% Types
-type rollout() :: #{
    variations => [weighted_variation()],
    bucketBy  => ldclient_user:attribute()
}.
%% Describes how users will be bucketed into variations during a percentage rollout

-type weighted_variation() :: #{
    variation => ldclient_flag:variation(),
    weight    => non_neg_integer() % 0 to 100000
}.
%% Describes a fraction of users who will receive a specific variation

-export_type([rollout/0]).

%%===================================================================
%% API
%%===================================================================

-spec new(map()) -> rollout().
new(#{
    <<"variations">> := Variations,
    <<"bucketBy">>  := BucketBy
}) ->
    #{
        variations => parse_variations(Variations),
        bucketBy  => BucketBy
    };
new(#{<<"variations">> := Variations}) ->
    #{
        variations => parse_variations(Variations),
        bucketBy  => key
    }.

-spec rollout_user(rollout(), ldclient_flag:flag(), ldclient_user:user()) -> ldclient_flag:variation() | null.
rollout_user(#{variations := WeightedVariations, bucketBy := BucketBy}, #{key := FlagKey, salt := FlagSalt}, User) ->
    Bucket = bucket_user(FlagKey, FlagSalt, User, BucketBy),
    match_weighted_variations(Bucket, WeightedVariations).

-spec bucket_user(ldclient_flag:key(), binary(), ldclient_user:user(), ldclient_user:attribute()) -> float().
bucket_user(Key, Salt, User, BucketBy) ->
    UserValue = ldclient_user:get(BucketBy, User),
    UserSecondary = ldclient_user:get(secondary, User),
    bucket_user_value(Key, Salt, bucketable_value(UserValue), UserSecondary).

%%===================================================================
%% Internal functions
%%===================================================================

-spec parse_variations([map()]) -> [weighted_variation()].
parse_variations(Variations) ->
    F = fun(#{<<"variation">> := Variation, <<"weight">> := Weight}, Acc) ->
            [#{variation => Variation, weight => Weight}|Acc];
        (#{<<"variation">> := Variation}, Acc) ->
            [#{variation => Variation, weight => 0}|Acc];
        (_, Acc)  ->
            Acc
        end,
    lists:foldr(F, [], Variations).

-spec match_weighted_variations(float(), [weighted_variation()]) -> ldclient_flag:variation().
match_weighted_variations(_, []) -> null;
match_weighted_variations(Bucket, WeightedVariations) ->
    match_weighted_variations(Bucket, WeightedVariations, 0.0).

-spec match_weighted_variations(float(), [weighted_variation()], float()) -> ldclient_flag:variation().
match_weighted_variations(_Bucket, [], _Sum) -> null;
match_weighted_variations(_Bucket, [#{variation := Variation}|[]], _Sum) -> Variation;
match_weighted_variations(Bucket, [#{variation := Variation, weight := Weight}|_], Sum)
    when Bucket < Sum + Weight / 100000 ->
    Variation;
match_weighted_variations(Bucket, [#{weight := Weight}|Rest], Sum) ->
    match_weighted_variations(Bucket, Rest, Sum + Weight / 100000).

-spec bucket_user_value(ldclient_flag:key(), binary(), binary() | null, binary()) -> float().
bucket_user_value(_Key, _Salt, null, _Secondary) -> 0.0;
bucket_user_value(Key, Salt, UserAttribute, null) ->
    bucket_hash(<<Key/binary, $., Salt/binary, $., UserAttribute/binary>>);
bucket_user_value(Key, Salt, UserAttribute, Secondary) ->
    bucket_hash(<<Key/binary, $., Salt/binary, $., UserAttribute/binary, $., Secondary/binary>>).

-spec bucket_hash(binary()) -> float().
bucket_hash(Hash) ->
    Sha1 = crypto:hash(sha, Hash),
    Sha1Hex = lists:flatten([[io_lib:format("~2.16.0B",[X]) || <<X:8>> <= Sha1 ]]),
    Sha1_15 = string:substr(Sha1Hex, 1, 15),
    Int = list_to_integer(Sha1_15, 16),
    Int / 1152921504606846975.

-spec bucketable_value(any()) -> binary() | null.
bucketable_value(V) when is_binary(V) -> V;
bucketable_value(V) when is_integer(V) -> list_to_binary(integer_to_list(V));
bucketable_value(V) when is_float(V) -> if V == trunc(V) -> list_to_binary(integer_to_list(trunc(V))); true -> null end;
bucketable_value(_) -> null.
