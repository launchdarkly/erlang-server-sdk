%%-------------------------------------------------------------------
%% @doc Flag rule data type
%%
%% @end
%%-------------------------------------------------------------------

-module(eld_rule).

%% API
-export([new/1]).
-export([match_user/4]).

%% Types
-type rule() :: #{
    id                   => binary(),
    clauses              => [eld_clause:clause()],
    variation_or_rollout => eld_flag:variation_or_rollout()
}.
%% Expresses a set of AND-ed matching conditions for a user, along with either
%% a fixed variation or a set of rollout percentages

-export_type([rule/0]).

%%===================================================================
%% API
%%===================================================================

-spec new(map()) -> rule().
new(#{<<"id">> := Id, <<"clauses">> := Clauses, <<"variation">> := Variation}) ->
    #{id => Id, clauses => parse_clauses(Clauses), variation_or_rollout => Variation};
new(#{<<"id">> := Id, <<"clauses">> := Clauses, <<"rollout">> := Rollout}) ->
    #{id => Id, clauses => parse_clauses(Clauses), variation_or_rollout => parse_rollout(Rollout)}.

%% @doc Match all clauses to user, includes segment_match
%%
%% @end
-spec match_user(rule(), User :: eld_user:user(), StorageBackend :: atom(), Tag :: atom()) -> match | no_match.
match_user(#{clauses := Clauses}, User, StorageBackend, Tag) ->
    check_clauses(Clauses, User, StorageBackend, Tag).

%%===================================================================
%% Internal functions
%%===================================================================

-spec parse_clauses([map()]) -> [eld_clause:clause()].
parse_clauses(Clauses) ->
    F = fun(Clause) -> eld_clause:new(Clause) end,
    lists:map(F, Clauses).

-spec parse_rollout(map()) -> eld_rollout:rollout().
parse_rollout(RolloutRaw) ->
    eld_rollout:new(RolloutRaw).

-spec check_clauses([eld_clause:clause()], eld_user:user(), atom(), atom()) -> match | no_match.
check_clauses([], _User, _StorageBackend, _Tag) -> match;
check_clauses([Clause|Rest], User, StorageBackend, Tag) ->
    Result = eld_clause:match_user(Clause, User, StorageBackend, Tag),
    check_clause_result(Result, Rest, User, StorageBackend, Tag).

-spec check_clause_result(match | no_match, [eld_clause:clause()], eld_user:user(), atom(), atom()) -> match | no_match.
check_clause_result(no_match, _Rest, _User, _StorageBackend, _Tag) -> no_match;
check_clause_result(match, Rest, User, StorageBackend, Tag) ->
    check_clauses(Rest, User, StorageBackend, Tag).
