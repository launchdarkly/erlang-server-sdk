%%%-------------------------------------------------------------------
%%% @doc Flag rule data type
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(eld_rule).

%% API
-export([new/1]).
-export([match_user/3]).

%% Types
-type rule() :: #{
    id                   => binary(),
    clauses              => [clause()],
    variation_or_rollout => eld_flag:variation_or_rollout()
}.
%% Expresses a set of AND-ed matching conditions for a user, along with either
%% a fixed variation or a set of rollout percentages

-type clause() :: #{
    attribute => binary(),
    op        => operator(),
    values    => [eld_flag:variation_value()],
    negate    => boolean()
}.
%% Describes an individual clause within a targeting rule

-type operator() :: in | ends_with | starts_with | matches | contains
    | less_than | less_than_or_equal | greater_than | greater_than_or_equal
    | before | 'after' | segment_match | semver_equal | semver_less_than
    | semver_greater_than.
%% List of available operators

-export_type([rule/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(map()) -> eld_rule:rule().
new(#{<<"id">> := Id, <<"clauses">> := Clauses, <<"variation">> := Variation}) ->
    #{id => Id, clauses => parse_clauses(Clauses), variation_or_rollout => Variation};
new(#{<<"id">> := Id, <<"clauses">> := Clauses, <<"rollout">> := Rollout}) ->
    #{id => Id, clauses => parse_clauses(Clauses), variation_or_rollout => Rollout}.

-spec match_user(rule(), eld_user:user(), atom()) -> match | no_match.
match_user(#{clauses := Clauses}, User, StorageBackend) ->
    check_clauses(Clauses, User, StorageBackend).

%%====================================================================
%% Internal functions
%%====================================================================

-spec parse_clauses([map()]) -> [clause()].
parse_clauses(Clauses) ->
    F = fun(#{<<"attribute">> := Attribute, <<"negate">> := Negate, <<"op">> := Op, <<"values">> := Values}) ->
            #{attribute => Attribute, negate => Negate, op => parse_operator(Op), values => Values}
        end,
    lists:map(F, Clauses).

-spec parse_operator(binary()) -> operator().
parse_operator(<<"in">>) -> in;
parse_operator(<<"endsWith">>) -> ends_with;
parse_operator(<<"startsWith">>) -> starts_with;
parse_operator(<<"matches">>) -> matches;
parse_operator(<<"contains">>) -> contains;
parse_operator(<<"lessThan">>) -> less_than;
parse_operator(<<"lessThanOrEqual">>) -> less_than_or_equal;
parse_operator(<<"greaterThan">>) -> greater_than;
parse_operator(<<"greaterThanOrEqual">>) -> greater_than_or_equal;
parse_operator(<<"before">>) -> before;
parse_operator(<<"after">>) -> 'after';
parse_operator(<<"segmentMatch">>) -> segment_match;
parse_operator(<<"semVerEqual">>) -> semver_equal;
parse_operator(<<"semVerLessThan">>) -> semver_less_than;
parse_operator(<<"semVerGreaterThan">>) -> semver_greater_than.

-spec check_clauses([clause()], eld_user:user(), atom()) -> match | no_match.
check_clauses([], _User, _StorageBackend) -> match;
check_clauses([Clause|Rest], User, StorageBackend) ->
    Result = check_clause(Clause, User, StorageBackend),
    check_clause_result(Result, Rest, User, StorageBackend).

-spec check_clause(clause(), eld_user:user(), atom()) -> match | no_match.
check_clause(#{op := segment_match}, _User, _StorageBackend) ->
    % TODO implement
    no_match.

check_clause_result(no_match, _Rest, _User, _StorageBackend) -> no_match;
check_clause_result(match, Rest, User, StorageBackend) ->
    check_clauses(Rest, User, StorageBackend).

