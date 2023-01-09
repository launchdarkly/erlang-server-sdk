%%-------------------------------------------------------------------
%% @doc Flag rule data type
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_rule).

%% API
-export([new/1]).
-export([match_context/4]).

%% Types
-type rule() :: #{
    id                   => binary(),
    clauses              => [ldclient_clause:clause()],
    trackEvents         => boolean(),
    variationOrRollout => ldclient_flag:variation_or_rollout()
}.
%% Expresses a set of AND-ed matching conditions for a context, along with either
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

%% @doc Match all clauses to context, includes segmentMatch
%%
%% @end
-spec match_context(rule(), Context :: ldclient_context:context(), FeatureStore :: atom(), Tag :: atom()) -> match | no_match | malformed_flag.
match_context(#{clauses := Clauses}, Context, FeatureStore, Tag) ->
    check_clauses(Clauses, Context, FeatureStore, Tag).

%%===================================================================
%% Internal functions
%%===================================================================

-spec new_from_template(map()) -> rule().
new_from_template(#{<<"id">> := Id, <<"clauses">> := Clauses, <<"trackEvents">> := TrackEvents, <<"variation">> := Variation}) ->
    #{id => Id, clauses => parse_clauses(Clauses), trackEvents => TrackEvents, variationOrRollout => Variation};
new_from_template(#{<<"id">> := Id, <<"clauses">> := Clauses, <<"trackEvents">> := TrackEvents, <<"rollout">> := #{
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

-spec check_clauses(
    Clauses:: [ldclient_clause:clause()],
    Context :: ldclient_context:context(),
    FeatureStore :: atom(),
    Tag :: atom()) -> match | no_match | malformed_flag.
check_clauses([], _Context, _FeatureStore, _Tag) -> match;
check_clauses([Clause|Rest], Context, FeatureStore, Tag) ->
    Result = ldclient_clause:match_context(Clause, Context, FeatureStore, Tag),
    check_clause_result(Result, Rest, Context, FeatureStore, Tag).

-spec check_clause_result(Result :: match | no_match | malformed_flag,
    Clauses :: [ldclient_clause:clause()],
    Context :: ldclient_context:context(),
    FeatureStore :: atom(),
    Tag :: atom()) -> match | no_match | malformed_flag.
check_clause_result(malformed_flag, _Rest, _Context, _FeatureStore, _Tag) -> malformed_flag;
check_clause_result(no_match, _Rest, _Context, _FeatureStore, _Tag) -> no_match;
check_clause_result(match, Rest, Context, FeatureStore, Tag) ->
    check_clauses(Rest, Context, FeatureStore, Tag).
