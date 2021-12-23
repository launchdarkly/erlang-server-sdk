%%-------------------------------------------------------------------
%% @doc Flag rule data type
%% @private
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
    trackEvents         => boolean(),
    variationOrRollout => ldclient_flag:variation_or_rollout()
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

%% @doc Match all clauses to user, includes segmentMatch
%%
%% @end
-spec match_user(rule(), User :: ldclient_user:user(), FeatureStore :: atom(), Tag :: atom()) -> match | no_match.
match_user(#{clauses := Clauses}, User, FeatureStore, Tag) ->
    check_clauses(Clauses, User, FeatureStore, Tag).

%%===================================================================
%% Internal functions
%%===================================================================

-spec new_from_template(map()) -> rule().
new_from_template(#{<<"id">> := Id, <<"clauses">> := Clauses, <<"trackEvents">> := TrackEvents, <<"variationOrRollout">> := Variation}) when is_integer(Variation) ->
    #{id => Id, clauses => parse_clauses(Clauses), trackEvents => TrackEvents, variationOrRollout => Variation};
new_from_template(#{<<"id">> := Id, <<"clauses">> := Clauses, <<"trackEvents">> := TrackEvents, <<"variationOrRollout">> := #{
        <<"variations">> := Variations
    } = Rollout}) when is_list(Variations) ->
    #{
        id => Id,
        clauses => parse_clauses(Clauses),
        trackEvents => TrackEvents,
        variationOrRollout => ldclient_rollout:new(Rollout)
    };
new_from_template(#{<<"id">> := Id, <<"clauses">> := Clauses, <<"trackEvents">> := TrackEvents}) ->
    #{id => Id, clauses => parse_clauses(Clauses), trackEvents => TrackEvents, variationOrRollout => null}.

-spec parse_clauses([map()]) -> [ldclient_clause:clause()].
parse_clauses(Clauses) ->
    F = fun(Clause) -> ldclient_clause:new(Clause) end,
    lists:map(F, Clauses).

-spec check_clauses([ldclient_clause:clause()], ldclient_user:user(), atom(), atom()) -> match | no_match.
check_clauses([], _User, _FeatureStore, _Tag) -> match;
check_clauses([Clause|Rest], User, FeatureStore, Tag) ->
    Result = ldclient_clause:match_user(Clause, User, FeatureStore, Tag),
    check_clause_result(Result, Rest, User, FeatureStore, Tag).

-spec check_clause_result(match | no_match, [ldclient_clause:clause()], ldclient_user:user(), atom(), atom()) -> match | no_match.
check_clause_result(no_match, _Rest, _User, _FeatureStore, _Tag) -> no_match;
check_clause_result(match, Rest, User, FeatureStore, Tag) ->
    check_clauses(Rest, User, FeatureStore, Tag).
