%%-------------------------------------------------------------------
%% @doc Flag rule data type
%%
%% @end
%%-------------------------------------------------------------------

-module(ldclient_rule).

%% API
-export([new/1]).
-export([match_user/4]).

%% Types
-type rule() :: #{
    id                   => binary(),
    clauses              => [ldclient_clause:clause()],
    track_events         => boolean(),
    variation_or_rollout => ldclient_flag:variation_or_rollout()
}.
%% Expresses a set of AND-ed matching conditions for a user, along with either
%% a fixed variation or a set of rollout percentages

-export_type([rule/0]).

%%===================================================================
%% API
%%===================================================================

-spec new(map()) -> rule().
new(RawRuleMap) ->
    RuleTemplate = #{
        <<"id">> => <<>>,
        <<"clauses">> => [],
        <<"trackEvents">> => false
    },
    RuleMap = maps:merge(RuleTemplate, RawRuleMap),
    new_from_template(RuleMap).

%% @doc Match all clauses to user, includes segment_match
%%
%% @end
-spec match_user(rule(), User :: ldclient_user:user(), StorageBackend :: atom(), Tag :: atom()) -> match | no_match.
match_user(#{clauses := Clauses}, User, StorageBackend, Tag) ->
    check_clauses(Clauses, User, StorageBackend, Tag).

%%===================================================================
%% Internal functions
%%===================================================================

-spec new_from_template(map()) -> rule().
new_from_template(#{<<"id">> := Id, <<"clauses">> := Clauses, <<"trackEvents">> := TrackEvents, <<"variation">> := Variation}) ->
    #{id => Id, clauses => parse_clauses(Clauses), track_events => TrackEvents, variation_or_rollout => Variation};
new_from_template(#{<<"id">> := Id, <<"clauses">> := Clauses, <<"trackEvents">> := TrackEvents, <<"rollout">> := #{
        <<"variations">> := Variations
    } = Rollout}) when is_list(Variations) ->
    #{
        id => Id,
        clauses => parse_clauses(Clauses),
        track_events => TrackEvents,
        variation_or_rollout => ldclient_rollout:new(Rollout)
    };
new_from_template(#{<<"id">> := Id, <<"clauses">> := Clauses, <<"trackEvents">> := TrackEvents}) ->
    #{id => Id, clauses => parse_clauses(Clauses), track_events => TrackEvents, variation_or_rollout => 0}.

-spec parse_clauses([map()]) -> [ldclient_clause:clause()].
parse_clauses(Clauses) ->
    F = fun(Clause) -> ldclient_clause:new(Clause) end,
    lists:map(F, Clauses).

-spec check_clauses([ldclient_clause:clause()], ldclient_user:user(), atom(), atom()) -> match | no_match.
check_clauses([], _User, _StorageBackend, _Tag) -> match;
check_clauses([Clause|Rest], User, StorageBackend, Tag) ->
    Result = ldclient_clause:match_user(Clause, User, StorageBackend, Tag),
    check_clause_result(Result, Rest, User, StorageBackend, Tag).

-spec check_clause_result(match | no_match, [ldclient_clause:clause()], ldclient_user:user(), atom(), atom()) -> match | no_match.
check_clause_result(no_match, _Rest, _User, _StorageBackend, _Tag) -> no_match;
check_clause_result(match, Rest, User, StorageBackend, Tag) ->
    check_clauses(Rest, User, StorageBackend, Tag).
