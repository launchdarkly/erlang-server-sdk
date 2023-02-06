%%-------------------------------------------------------------------
%% @doc Segment data type
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_segment).

%% API
-export([new/1]).
-export([match_context/2]).

%% Types
-type segment() :: #{
    key      => binary(),
    deleted  => boolean(),
    excluded => [binary()],
    included => [binary()],
    includedContexts => [segment_target()],
    excludedContexts => [segment_target()],
    rules    => [rule()],
    salt     => binary(),
    version  => pos_integer()
}.

-type segment_target() :: #{
    contextKind => binary(),
    values => [binary()]
}.

-type rule() :: #{
    clauses      => [ldclient_clause:clause()],
    weight       => null | non_neg_integer(),
    bucketBy    => ldclient_attribute_reference:attribute_reference(),
    segmentKey  => binary(),
    segmentSalt => binary(),
    rolloutContextKind => ldclient_context:kind_value()
}.

-export_type([segment/0]).

%%===================================================================
%% API
%%===================================================================

-spec new(map()) -> segment().
new(RawSegmentMap) ->
    SegmentTemplate = #{
        <<"key">> => <<>>,
        <<"deleted">> => false,
        <<"excluded">> => [],
        <<"included">> => [],
        <<"rules">> => [],
        <<"salt">> => <<>>,
        <<"version">> => 0,
        <<"includedContexts">> => [],
        <<"excludedContexts">> => []
    },
    SegmentMap = maps:merge(SegmentTemplate, RawSegmentMap),
    new_from_template(SegmentMap).

-spec match_context(segment(), ldclient_context:context()) -> match | no_match | malformed_flag.
match_context(Segment, Context) ->
    check_context_in_segment(Segment, Context).

%%===================================================================
%% Internal functions
%%===================================================================

-spec new_from_template(map()) -> segment().
new_from_template(#{
    <<"key">>      := Key,
    <<"deleted">>  := Deleted,
    <<"excluded">> := Excluded,
    <<"included">> := Included,
    <<"rules">>    := Rules,
    <<"salt">>     := Salt,
    <<"version">>  := Version,
    <<"includedContexts">> := IncludedContexts,
    <<"excludedContexts">> := ExcludedContexts
}) ->
    #{
        key      => Key,
        deleted  => Deleted,
        excluded => Excluded,
        included => Included,
        rules    => parse_rules(Key, Salt, Rules),
        salt     => Salt,
        version  => Version,
        includedContexts => parse_targets(IncludedContexts),
        excludedContexts => parse_targets(ExcludedContexts)
    }.

-spec parse_rules(binary(), binary(), [map()]) -> [rule()].
parse_rules(SegmentKey, SegmentSalt, Rules) ->
    F = fun(#{<<"clauses">> := Clauses} = RuleRaw, Acc) ->
            Rule = #{
                segmentKey  => SegmentKey,
                segmentSalt => SegmentSalt,
                clauses      => parse_clauses(Clauses)
            },
            [parse_rule_optional_attributes(Rule, RuleRaw)|Acc];
        (_, Acc) ->
            Acc
        end,
    lists:foldr(F, [], Rules).

-spec parse_rule_optional_attributes(map(), map()) -> rule().
parse_rule_optional_attributes(Rule, RuleRaw) ->
    Weight = maps:get(<<"weight">>, RuleRaw, null),
    RolloutContextKind = maps:get(<<"rolloutContextKind">>, RuleRaw, null),
    %% Rules before U2C would have had literals for attributes.
    %% So use the rolloutContextKind to indicate if this is new or old data.
    BucketBy = case RolloutContextKind of
        null -> ldclient_attribute_reference:new_from_legacy(maps:get(<<"bucketBy">>, RuleRaw, <<"key">>));
        _ -> ldclient_attribute_reference:new(maps:get(<<"bucketBy">>, RuleRaw, <<"key">>))
    end,
    Rule#{weight => Weight, bucketBy => BucketBy, rolloutContextKind => RolloutContextKind}.

-spec parse_clauses([map()]) -> [ldclient_clause:clause()].
parse_clauses(Clauses) ->
    F = fun(Clause) -> ldclient_clause:new(Clause) end,
    lists:map(F, Clauses).

-spec parse_targets(ContextTargets :: [map()]) -> [segment_target()].
parse_targets(ContextTargets) ->
    parse_targets(ContextTargets, []).

-spec parse_targets(ContextTargets :: [map()], AccIn :: [segment_target()]) -> [segment_target()].
parse_targets([], AccIn) -> AccIn;
parse_targets([#{<<"contextKind">> := ContextKind, <<"values">> := Values} = _Target|RemainingTargets], AccIn) ->
    %% The order of the parsed targets needs to be the same as the original targets.
    parse_targets(RemainingTargets, AccIn ++ [#{contextKind => ContextKind, values => Values}]).

-spec context_target_search(
    Context :: ldclient_context:context(),
    ContextTargets :: [segment_target()],
    Targets :: [binary()]
    ) -> boolean().
context_target_search(Context, ContextTargets, Targets) ->
    case lists:search(fun(Target) ->
            #{contextKind := ContextKind, values := Values} = Target,
            ContextKey = ldclient_context:get_key(ContextKind, Context),
            case ContextKey of
                %% There was not a context of the specified kind.
                null -> false;
                %% There was a context, so we need to check if the key is in the values list.
                _ -> lists:member(ContextKey, Values)
            end
        end, ContextTargets) of
        %% There was not a context targets match, so we should check user targets.
        false ->
            ContextKey = ldclient_context:get_key(<<"user">>, Context),
            case ContextKey of
                %% There was not a user context.
                null -> false;
                _ -> lists:member(ContextKey, Targets)
            end;
        %% The context targets contained a match.
        _  -> true
    end.


check_context_in_segment(Segment, Context) ->
    check_context_included(Segment, Context).

check_context_included(#{included := Included, includedContexts := IncludedContexts} = Segment, Context) ->
    Result = context_target_search(Context, IncludedContexts, Included),
    check_context_included_result(Result, Segment, Context).

check_context_included_result(true, _Segment, _Context) -> match;
check_context_included_result(false, Segment, Context) ->
    check_context_excluded(Segment, Context).

check_context_excluded(#{excluded := Excluded, excludedContexts := ExcludedContexts} = Segment, Context) ->
    Result = context_target_search(Context, ExcludedContexts, Excluded),
    check_context_excluded_result(Result, Segment, Context).

%% These are excluded, so it is inverted. true = no_match, versus included  where true = match.
check_context_excluded_result(true, _Segment, _Context) -> no_match;
check_context_excluded_result(false, #{rules := Rules}, Context) ->
    check_rules(Rules, Context).

check_rules([], _Context) -> no_match;
check_rules([Rule|Rest], Context) ->
    Result = check_rule(Rule, Context),
    check_rule_result({Result, Rule}, Rest, Context).

check_rule_result({malformed_flag, _Rule}, _Rest, _Context) -> malformed_flag;
check_rule_result({match, _Rule}, _Rest, _Context) -> match;
check_rule_result({no_match, _Rule}, Rest, Context) ->
    check_rules(Rest, Context).

check_rule(#{clauses := Clauses} = Rule, Context) ->
    Result = check_clauses(Clauses, Context),
    check_clauses_result(Result, Rule, Context).

-spec check_clauses([ldclient_clause:clause()], ldclient_context:context()) -> match | no_match.
check_clauses([], _Context) -> match;
check_clauses([Clause|Rest], Context) ->
    % Non-segment match
    Result = ldclient_clause:match_context(Clause, Context),
    check_clause_result(Result, Rest, Context).

check_clauses_result(no_match, _Rule, _Context) -> no_match;
check_clauses_result(match, Rule, Context) ->
    check_rule_weight(Rule, Context).

-spec check_clause_result(match | no_match, [ldclient_clause:clause()], ldclient_context:context()) -> match | no_match.
check_clause_result(no_match, _Rest, _Context) -> no_match;
check_clause_result(match, Rest, Context) ->
    check_clauses(Rest, Context).

check_rule_weight(#{weight := null}, _Context) -> match;
check_rule_weight(Rule, Context) ->
    check_context_bucket(Rule, Context).

check_context_bucket(#{bucketBy := #{valid := false}} = _SegmentRule, _Context) -> malformed_flag;
check_context_bucket(#{
    segmentKey := SegmentKey,
    segmentSalt := SegmentSalt,
    bucketBy := BucketBy,
    weight := Weight,
    rolloutContextKind := RolloutContextKind
} = _SegmentRule, Context) ->
    Bucket = ldclient_rollout:bucket_context(null, SegmentKey, SegmentSalt, Context, BucketBy, RolloutContextKind),
    check_context_bucket_result(Bucket, Weight).

check_context_bucket_result(Bucket, Weight) when Bucket < Weight / 100000 -> match;
check_context_bucket_result(_, _) -> no_match.
