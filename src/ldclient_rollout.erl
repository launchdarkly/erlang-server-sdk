%%-------------------------------------------------------------------
%% @doc Rollout data type
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_rollout).

%% API
-export([new/1]).
-export([bucket_user/5]).
-export([rollout_user/3]).

%% Types

-type seed() :: non_neg_integer() | null.

-type rollout() :: #{
    variations  => [weighted_variation()],
    bucketBy    => ldclient_user:attribute(),
    kind        => rollout_kind(),
    seed        => seed()
}.
%% Describes how users will be bucketed into variations during a percentage rollout

-type rollout_kind() :: rollout | experiment.
%% the type of rollout to use, 
%% - rollout uses old hashing ignoring rollout:seed
%% - experiment uses new hashing 

-type weighted_variation() :: #{
    variation   => ldclient_flag:variation(),
    weight      => non_neg_integer(), % 0 to 100000
    untracked   => boolean()
}.
%% Describes a fraction of users who will receive a specific variation

-type rollout_result() :: {
    Variation :: ldclient_flag:variation(),
    InExperiment :: boolean()
}.

-export_type([rollout/0]).
-export_type([rollout_result/0]).

%%===================================================================
%% API
%%===================================================================

-spec new(map()) -> rollout().
new(#{<<"variations">> := Variations} = Map) ->
    #{
        variations => parse_variations(Variations),
        bucketBy  => maps:get(<<"bucketBy">>, Map, key),
        kind => parse_rollout_kind(maps:get(<<"kind">>, Map, <<"rollout">>)),
        seed => maps:get(<<"seed">>, Map, null)
    }.

-spec parse_rollout_kind(Kind :: binary()) -> rollout_kind().
parse_rollout_kind(<<"experiment">>) -> experiment;
parse_rollout_kind(<<"rollout">>) -> rollout;
parse_rollout_kind(Kind) ->
    %% If we are not familiar with this kind, then log it and default to rollout.
    error_logger:warning_msg("Unrecognized rollout type: ~p", [Kind]),
    rollout.


-spec rollout_user(rollout(), ldclient_flag:flag(), ldclient_user:user()) -> rollout_result().
rollout_user(#{kind := experiment, seed := Seed, variations := WeightedVariations, bucketBy := BucketBy},
    #{key := FlagKey, salt := FlagSalt}, User) ->
    Bucket = bucket_user(Seed, FlagKey, FlagSalt, User, BucketBy),
    #{variation := Variation, untracked := Untracked} = match_weighted_variations(Bucket, WeightedVariations),
    {Variation, not Untracked};
rollout_user(#{variations := WeightedVariations, bucketBy := BucketBy, seed := Seed} = _Rollout, #{key := FlagKey, salt := FlagSalt}, User) ->
    Bucket = bucket_user(Seed, FlagKey, FlagSalt, User, BucketBy),
    VariationBucket = match_weighted_variations(Bucket, WeightedVariations),
    extract_variation(VariationBucket, false).


extract_variation(null, false) -> {null, false};
extract_variation(#{variation := Variation}, InExperiment) ->
    {Variation, InExperiment}.

-spec bucket_user(seed(), ldclient_flag:key(), binary(), ldclient_user:user(), ldclient_user:attribute()) -> float().
bucket_user(Seed, Key, Salt, User, BucketBy) ->
    UserValue = ldclient_user:get(BucketBy, User),
    UserSecondary = ldclient_user:get(secondary, User),
    bucket_user_value(Seed, Key, Salt, bucketable_value(UserValue), UserSecondary).

%%===================================================================
%% Internal functions
%%===================================================================

-spec parse_variations([map()]) -> [weighted_variation()].
parse_variations(Variations) ->
    F = fun(#{<<"variation">> := Variation} = Map, Acc) ->
            Data = #{
                variation => Variation,
                weight => maps:get(<<"weight">>, Map, 0),
                untracked => maps:get(<<"untracked">>, Map, false)
            },
            [Data|Acc];
        (_, Acc)  ->
            Acc
        end,
    lists:foldr(F, [], Variations).

-spec match_weighted_variations(float(), [weighted_variation()]) -> weighted_variation() | null.
match_weighted_variations(_, []) -> null;
match_weighted_variations(Bucket, WeightedVariations) ->
    match_weighted_variations(Bucket, WeightedVariations, 0.0).

-spec match_weighted_variations(float(), [weighted_variation()], float()) -> weighted_variation() | null.
match_weighted_variations(_Bucket, [], _Sum) -> null;
match_weighted_variations(_Bucket, [WeightedVariation|[]], _Sum) -> WeightedVariation;
match_weighted_variations(Bucket, [#{weight := Weight} = WeightedVariation|_], Sum)
    when Bucket < Sum + Weight / 100000 ->
    WeightedVariation;
match_weighted_variations(Bucket, [#{weight := Weight}|Rest], Sum) ->
    match_weighted_variations(Bucket, Rest, Sum + Weight / 100000).

-spec bucket_user_value(seed(), ldclient_flag:key(), binary(), binary() | null, binary()) -> float().
bucket_user_value(null, _Key, _Salt, null, _Secondary) -> 0.0;
%% when no seed is present hash with `key.salt.attribute[.secondary]`
bucket_user_value(null, Key, Salt, UserAttribute, null) ->
    bucket_hash(<<Key/binary, $., Salt/binary, $., UserAttribute/binary>>);
bucket_user_value(null, Key, Salt, UserAttribute, Secondary) ->
    bucket_hash(<<Key/binary, $., Salt/binary, $., UserAttribute/binary, $., Secondary/binary>>);
%% when a seed is present hash with `seed.attribute[.secondary]`
bucket_user_value(Seed, _Key, _Salt, UserAttribute, null) ->
    Prefix = integer_to_binary(Seed),
    bucket_hash(<<Prefix/binary, $., UserAttribute/binary>>);
bucket_user_value(Seed, _Key, _Salt, UserAttribute, Secondary) ->
    Prefix = integer_to_binary(Seed),
    bucket_hash(<<Prefix/binary, $., UserAttribute/binary, $., Secondary/binary>>).

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
