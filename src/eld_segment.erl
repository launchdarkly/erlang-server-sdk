%%-------------------------------------------------------------------
%% @doc Segment data type
%%
%% @end
%%-------------------------------------------------------------------

-module(eld_segment).

%% API
-export([new/1]).
-export([match_user/2]).

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
    clauses      => [eld_clause:clause()],
    weight       => null | non_neg_integer(),
    bucket_by    => eld_user:attribute(),
    segment_key  => binary(),
    segment_salt => binary()
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
    },
    SegmentMap = maps:merge(SegmentTemplate, RawSegmentMap),
    new_from_template(SegmentMap).

-spec match_user(segment(), eld_user:user()) -> match | no_match.
match_user(Segment, User) ->
    check_user_in_segment(Segment, User).

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
                segment_key  => SegmentKey,
                segment_salt => SegmentSalt,
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
    BucketBy = maps:get(<<"bucketBy">>, RuleRaw, key),
    Rule#{weight => Weight, bucket_by => BucketBy}.

-spec parse_clauses([map()]) -> [eld_clause:clause()].
parse_clauses(Clauses) ->
    F = fun(Clause) -> eld_clause:new(Clause) end,
    lists:map(F, Clauses).

check_user_in_segment(Segment, User) ->
    check_user_included(Segment, User).

check_user_included(#{included := Included} = Segment, #{key := UserKey} = User) ->
    Result = lists:member(UserKey, Included),
    check_user_included_result(Result, Segment, User).

check_user_included_result(true, _Segment, _User) -> match;
check_user_included_result(false, Segment, User) ->
    check_user_excluded(Segment, User).

check_user_excluded(#{excluded := Excluded} = Segment, #{key := UserKey} = User) ->
    Result = lists:member(UserKey, Excluded),
    check_user_excluded_result(Result, Segment, User).

check_user_excluded_result(true, _Segment, _User) -> no_match;
check_user_excluded_result(false, #{rules := Rules}, User) ->
    check_rules(Rules, User).

check_rules([], _User) -> no_match;
check_rules([Rule|Rest], User) ->
    Result = check_rule(Rule, User),
    check_rule_result({Result, Rule}, Rest, User).

check_rule_result({match, _Rule}, _Rest, _User) -> match;
check_rule_result({no_match, _Rule}, Rest, User) ->
    check_rules(Rest, User).

check_rule(#{clauses := Clauses} = Rule, User) ->
    Result = check_clauses(Clauses, User),
    check_clauses_result(Result, Rule, User).

-spec check_clauses([eld_clause:clause()], eld_user:user()) -> match | no_match.
check_clauses([], _User) -> match;
check_clauses([Clause|Rest], User) ->
    % Non-segment match
    Result = eld_clause:match_user(Clause, User),
    check_clause_result(Result, Rest, User).

check_clauses_result(no_match, _Rule, _User) -> no_match;
check_clauses_result(match, Rule, User) ->
    check_rule_weight(Rule, User).

-spec check_clause_result(match | no_match, [eld_clause:clause()], eld_user:user()) -> match | no_match.
check_clause_result(no_match, _Rest, _User) -> no_match;
check_clause_result(match, Rest, User) ->
    check_clauses(Rest, User).

check_rule_weight(#{weight := null}, _User) -> match;
check_rule_weight(Rule, User) ->
    check_user_bucket(Rule, User).

check_user_bucket(#{segment_key := SegmentKey, segment_salt := SegmentSalt, bucket_by := BucketBy, weight := Weight}, User) ->
    Bucket = eld_rollout:bucket_user(SegmentKey, SegmentSalt, User, BucketBy),
    check_user_bucket_result(Bucket, Weight).

check_user_bucket_result(Bucket, Weight) when Bucket < Weight / 100000 -> match;
check_user_bucket_result(_, _) -> no_match.
