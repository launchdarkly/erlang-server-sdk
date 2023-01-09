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
    rules    => [rule()],
    salt     => binary(),
    version  => pos_integer()
}.

-type rule() :: #{
    clauses      => [ldclient_clause:clause()],
    weight       => null | non_neg_integer(),
    bucketBy    => ldclient_attribute_reference:attribute_reference(),
    segmentKey  => binary(),
    segmentSalt => binary()
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
        <<"version">> => 0
        %% TODO: More U2C stuff to add here.
    },
    SegmentMap = maps:merge(SegmentTemplate, RawSegmentMap),
    new_from_template(SegmentMap).

-spec match_context(segment(), ldclient_context:context()) -> match | no_match.
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
    <<"version">>  := Version
}) ->
    #{
        key      => Key,
        deleted  => Deleted,
        excluded => Excluded,
        included => Included,
        rules    => parse_rules(Key, Salt, Rules),
        salt     => Salt,
        version  => Version
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
    %% TODO: Handle legacy attribute vs attribute reference.
    BucketBy = ldclient_attribute_reference:new(maps:get(<<"bucketBy">>, RuleRaw, <<"/key">>)),
    Rule#{weight => Weight, bucketBy => BucketBy}.

-spec parse_clauses([map()]) -> [ldclient_clause:clause()].
parse_clauses(Clauses) ->
    F = fun(Clause) -> ldclient_clause:new(Clause) end,
    lists:map(F, Clauses).

check_context_in_segment(Segment, Context) ->
    check_context_included(Segment, Context).

check_context_included(#{included := Included} = Segment, #{key := ContextKey} = Context) ->
    Result = lists:member(ContextKey, Included),
    check_context_included_result(Result, Segment, Context).

check_context_included_result(true, _Segment, _Context) -> match;
check_context_included_result(false, Segment, Context) ->
    check_context_excluded(Segment, Context).

check_context_excluded(#{excluded := Excluded} = Segment, #{key := ContextKey} = Context) ->
    Result = lists:member(ContextKey, Excluded),
    check_context_excluded_result(Result, Segment, Context).

check_context_excluded_result(true, _Segment, _Context) -> no_match;
check_context_excluded_result(false, #{rules := Rules}, Context) ->
    check_rules(Rules, Context).

check_rules([], _Context) -> no_match;
check_rules([Rule|Rest], Context) ->
    Result = check_rule(Rule, Context),
    check_rule_result({Result, Rule}, Rest, Context).

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

check_context_bucket(#{segmentKey := SegmentKey, segmentSalt := SegmentSalt, bucketBy := BucketBy, weight := Weight}, Context) ->
    Bucket = ldclient_rollout:bucket_context(null, SegmentKey, SegmentSalt, Context, BucketBy),
    check_context_bucket_result(Bucket, Weight).

check_context_bucket_result(Bucket, Weight) when Bucket < Weight / 100000 -> match;
check_context_bucket_result(_, _) -> no_match.
