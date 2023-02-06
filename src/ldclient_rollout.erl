%%-------------------------------------------------------------------
%% @doc Rollout data type
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_rollout).

%% API
-export([new/1]).
-export([bucket_context/6]).
-export([rollout_context/3]).

%% Types

-type seed() :: non_neg_integer() | null.

-type rollout() :: #{
    variations  => [weighted_variation()],
    bucketBy    => ldclient_attribute_reference:attribute_reference(),
    contextKind => ldclient_context:kind_value(),
    kind        => rollout_kind(),
    seed        => seed()
}.
%% Describes how contexts will be bucketed into variations during a percentage rollout

-type rollout_kind() :: rollout | experiment.
%% the type of rollout to use, 
%% - rollout uses old hashing ignoring rollout:seed
%% - experiment uses new hashing 

-type weighted_variation() :: #{
    variation   => ldclient_flag:variation(),
    weight      => non_neg_integer(), % 0 to 100000
    untracked   => boolean()
}.
%% Describes a fraction of contexts who will receive a specific variation

-type rollout_result() :: {
    Variation :: ldclient_flag:variation(),
    InExperiment :: boolean()
} | malformed_flag.

-export_type([rollout/0]).
-export_type([rollout_result/0]).

%%===================================================================
%% API
%%===================================================================

-spec new(map()) -> rollout().
new(#{<<"variations">> := Variations} = Map) ->
    #{
        variations => parse_variations(Variations),
        bucketBy => get_bucket_by(Map),
        kind => parse_rollout_kind(maps:get(<<"kind">>, Map, <<"rollout">>)),
        seed => maps:get(<<"seed">>, Map, null),
        contextKind => maps:get(<<"contextKind">>, Map, <<"user">>)
    }.

-spec parse_rollout_kind(Kind :: binary()) -> rollout_kind().
parse_rollout_kind(<<"experiment">>) -> experiment;
parse_rollout_kind(<<"rollout">>) -> rollout;
parse_rollout_kind(Kind) ->
    %% If we are not familiar with this kind, then log it and default to rollout.
    error_logger:warning_msg("Unrecognized rollout type: ~p", [Kind]),
    rollout.

-spec rollout_context(
    Rollout :: rollout(),
    Flag :: ldclient_flag:flag(),
    Context :: ldclient_context:context()) -> rollout_result().
rollout_context(#{bucketBy := #{valid := false}} = _Rollout, _Flag, _Context) ->
    malformed_flag;
rollout_context(#{
        kind := experiment,
        seed := Seed,
        variations := WeightedVariations,
        bucketBy := BucketBy,
        contextKind := ContextKind
    } = _Rollout,
    #{key := FlagKey, salt := FlagSalt} = _Flag, Context) ->
    HasContext = lists:member(ContextKind, ldclient_context:get_kinds(Context)),
    Bucket = bucket_context(Seed, FlagKey, FlagSalt, Context, BucketBy, ContextKind),
    #{variation := Variation, untracked := Untracked} = match_weighted_variations(Bucket, WeightedVariations),
    %% If the context did not contain the kind, then we are not in an experiment.
    {Variation, (not Untracked) and HasContext};
rollout_context(#{
        variations := WeightedVariations,
        bucketBy := BucketBy,
        seed := Seed,
        contextKind := ContextKind
    } = _Rollout, #{key := FlagKey, salt := FlagSalt} = _Flag, Context) ->
    Bucket = bucket_context(Seed, FlagKey, FlagSalt, Context, BucketBy,ContextKind),
    VariationBucket = match_weighted_variations(Bucket, WeightedVariations),
    extract_variation(VariationBucket, false).

extract_variation(null, false) -> {null, false};
extract_variation(#{variation := Variation}, InExperiment) ->
    {Variation, InExperiment}.

-spec bucket_context(
    Seed :: seed(),
    Key :: ldclient_flag:key(),
    Salt :: binary(),
    Context :: ldclient_context:context(),
    BucketBy :: ldclient_attribute_reference:attribute_reference(),
    ContextKind :: ldclient_context:kind_value()) -> float().
bucket_context(Seed, Key, Salt, Context, BucketBy, ContextKind) ->
    ContextValue = ldclient_context:get(ContextKind, BucketBy, Context),
    bucket_context_value(Seed, Key, Salt, bucketable_value(ContextValue)).

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

-spec bucket_context_value(
    Seed :: seed(),
    FlagKey :: ldclient_flag:key(),
    Salt :: binary(),
    ContextValue :: binary() | null) -> float().
bucket_context_value(null, _FlagKey, _Salt, null) -> 0.0;
%% when no seed is present hash with `key.salt.attribute`
bucket_context_value(null, FlagKey, Salt, ContextValue) ->
    bucket_hash(<<FlagKey/binary, $., Salt/binary, $., ContextValue/binary>>);
%% when a seed is present hash with `seed.attribute`
bucket_context_value(Seed, _FlagKey, _Salt, ContextValue) ->
    Prefix = integer_to_binary(Seed),
    bucket_hash(<<Prefix/binary, $., ContextValue/binary>>).

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

%% @doc Get the attribute reference to bucket by.
%%
%% For experiments we always bucket by key, for other rollout kinds we bucket by the bucketBy field. This is done when
%% parsing the flag, instead of when evaluating the flag, for performance and simplicity of evaluation.
%%
%% For backward compatibility we escape the attribute using our logic for legacy attributes when there is not
%% a context kind. New data should always have a contextKind.
%% @end
-spec get_bucket_by(RawRollout :: map()) -> ldclient_attribute_reference:attribute_reference().
get_bucket_by(#{<<"kind">> := <<"experiment">>} = _Rollout) -> ldclient_attribute_reference:new(<<"key">>);
get_bucket_by(RawRollout) ->
    case maps:is_key(<<"contextKind">>, RawRollout) of
        %% Had context kind, so it is new data and we do not need to escape.
        true -> ldclient_attribute_reference:new(maps:get(<<"bucketBy">>, RawRollout, <<"key">>));
        %% Did not have context kind, so we assume the data to be old, and we need to escape the attribute.
        false -> ldclient_attribute_reference:new_from_legacy(maps:get(<<"bucketBy">>, RawRollout, <<"key">>))
    end.
