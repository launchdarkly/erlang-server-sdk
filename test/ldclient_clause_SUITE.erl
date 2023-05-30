-module(ldclient_clause_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    check_attribute_against_clause_value/1,
    match_context_kinds/1,
    malformed_flag_for_invalid_reference/1,
    already_visited_segment_results_in_malformed_flag/1,
    supports_nested_segments/1,
    check_operators/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        check_attribute_against_clause_value,
        match_context_kinds,
        malformed_flag_for_invalid_reference,
        already_visited_segment_results_in_malformed_flag,
        supports_nested_segments,
        check_operators
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%%====================================================================
%% Helpers
%%====================================================================

%%====================================================================
%% Tests
%%====================================================================

check_attribute_against_clause_value(_) ->
    % Invalid date types
    false = ldclient_clause:check_attribute_against_clause_value(true, before, 123456789),
    false = ldclient_clause:check_attribute_against_clause_value(123456789, before, [false]),
    false = ldclient_clause:check_attribute_against_clause_value([], before, []),
    false = ldclient_clause:check_attribute_against_clause_value(true, 'after', 123456789),
    false = ldclient_clause:check_attribute_against_clause_value(123456789, 'after', [false]),
    false = ldclient_clause:check_attribute_against_clause_value([], 'after', []),
    % Semver equal
    true = ldclient_clause:check_attribute_against_clause_value(<<"2.0.0">>, semVerEqual, <<"2.0.0">>),
    true = ldclient_clause:check_attribute_against_clause_value(<<"2.0">>, semVerEqual, <<"2.0.0">>),
    true = ldclient_clause:check_attribute_against_clause_value(<<"2">>, semVerEqual, <<"2.0.0">>),
    true = ldclient_clause:check_attribute_against_clause_value(<<"2-rc1">>, semVerEqual, <<"2.0.0-rc1">>),
    true = ldclient_clause:check_attribute_against_clause_value(<<"2+build2">>, semVerEqual, <<"2.0.0+build2">>),
    true = ldclient_clause:check_attribute_against_clause_value(<<"2+build123">>, semVerEqual, <<"2.0.0+build2">>),
    false = ldclient_clause:check_attribute_against_clause_value(<<"2.0.0">>, semVerEqual, <<"2.0.1">>),
    false = ldclient_clause:check_attribute_against_clause_value(<<"bad.vers.ion">>, semVerEqual, <<"2.0.1">>),
    % Semver less than
    true = ldclient_clause:check_attribute_against_clause_value(<<"2.0.0">>, semVerLessThan, <<"2.0.1">>),
    true = ldclient_clause:check_attribute_against_clause_value(<<"2.0">>, semVerLessThan, <<"2.0.1">>),
    true = ldclient_clause:check_attribute_against_clause_value(<<"2">>, semVerLessThan, <<"2.0.1">>),
    true = ldclient_clause:check_attribute_against_clause_value(<<"2.0.0-rc">>, semVerLessThan, <<"2.0.0-rc.beta">>),
    false = ldclient_clause:check_attribute_against_clause_value(<<"2.0.1">>, semVerLessThan, <<"2.0.0">>),
    false = ldclient_clause:check_attribute_against_clause_value(<<"2.0.1">>, semVerLessThan, <<"2.0">>),
    false = ldclient_clause:check_attribute_against_clause_value(<<"2.0.1">>, semVerLessThan, <<"2">>),
    false = ldclient_clause:check_attribute_against_clause_value(<<"bad.vers.ion">>, semVerLessThan, <<"2">>),
    % Semver greater than
    true = ldclient_clause:check_attribute_against_clause_value(<<"2.0.1">>, semVerGreaterThan, <<"2.0.0">>),
    true = ldclient_clause:check_attribute_against_clause_value(<<"2.0.1">>, semVerGreaterThan, <<"2.0">>),
    true = ldclient_clause:check_attribute_against_clause_value(<<"2.0.1">>, semVerGreaterThan, <<"2">>),
    true = ldclient_clause:check_attribute_against_clause_value(<<"2.0.0-rc1">>, semVerGreaterThan, <<"2.0.0-rc0">>),
    false = ldclient_clause:check_attribute_against_clause_value(<<"2.0.0">>, semVerGreaterThan, <<"2.0.0">>),
    false = ldclient_clause:check_attribute_against_clause_value(<<"2.0.0">>, semVerGreaterThan, <<"2.0">>),
    false = ldclient_clause:check_attribute_against_clause_value(<<"2.0.0">>, semVerGreaterThan, <<"2">>),
    false = ldclient_clause:check_attribute_against_clause_value(<<"bad.vers.ion">>, semVerGreaterThan, <<"2">>),
    ok.

match_context_kinds(_) ->
    ClauseMatchingUserKind = #{
        attribute => ldclient_attribute_reference:new(<<"kind">>),
        op => in,
        values => [<<"user">>],
        negate => false,
        context_kind => <<"user">> %% Unused for this type of rule.
    },
    SingleKindUser = #{kind => <<"user">>, key => <<"user-key">>},
    MultiKindWithUser = #{
        kind => <<"multi">>,
        <<"org">> => #{key => <<"org-key">>},
        <<"user">> => #{key => <<"user-key">>}},
    %% Feature store and client tag should not be hit in these cases. So we can pass null.
    match = ldclient_clause:match_context(ClauseMatchingUserKind, SingleKindUser, null, null),
    match = ldclient_clause:match_context(ClauseMatchingUserKind, MultiKindWithUser, null, null),
    SingleKindNotUser = #{kind => <<"org">>, key => <<"org-key">>},
    MultiKindNotUser = #{
        kind => <<"multi">>,
        <<"org">> => #{key => <<"org-key">>},
        <<"potato">> => #{key => <<"potato-key">>}},
    no_match = ldclient_clause:match_context(ClauseMatchingUserKind, SingleKindNotUser, null, null),
    no_match = ldclient_clause:match_context(ClauseMatchingUserKind, MultiKindNotUser, null, null).

malformed_flag_for_invalid_reference(_) ->
    BadClause = #{
        attribute => ldclient_attribute_reference:new(<<"">>),
        op => in,
        values => [<<"user">>],
        negate => false,
        context_kind => <<"user">> %% Unused for this type of rule.
    },
    %% Feature store and client tag should not be hit in these cases. So we can pass null.
    malformed_flag = ldclient_clause:match_context(BadClause, ldclient_context:new(<<"user-key">>), null, null).

already_visited_segment_results_in_malformed_flag(_) ->
    ClauseWithSegment = #{
        attribute => ldclient_attribute_reference:new(<<"key">>),
        op => segmentMatch,
        values => [<<"the-segment">>],
        negate => false,
        context_kind => <<"user">>
    },
    malformed_flag = ldclient_clause:match_context(ClauseWithSegment,
        ldclient_context:new(<<"user-key">>), null, null, [<<"the-segment">>]).

supports_nested_segments(_) ->
    FirstSegment = ldclient_segment:new(#{
        <<"key">> => <<"first-segment">>,
        <<"rules">> => [#{
            <<"clauses">> => [#{
                <<"op">> => <<"segmentMatch">>,
                <<"values">> => [<<"second-segment">>]
            }]
        }],
        <<"deleted">> => false
    }),
    SecondSegment = ldclient_segment:new(#{
        <<"key">> => <<"second-segment">>,
        <<"included">> => [<<"user-key">>],
        <<"rules">> => [],
        <<"deleted">> => false
    }),
    Storage = #{
        <<"first-segment">> => FirstSegment,
        <<"second-segment">> => SecondSegment
    },
    meck:new(mock_store,  [non_strict]),
    meck:expect(mock_store, get, fun(mock_client, segments, Key) ->
                                    [{Key, maps:get(Key, Storage)}]
                                 end),
    ClauseWithSegment = #{
        attribute => ldclient_attribute_reference:new(<<"key">>),
        op => segmentMatch,
        values => [<<"first-segment">>],
        negate => false,
        context_kind => <<"user">>
    },
    match = ldclient_clause:match_context(ClauseWithSegment,
        ldclient_context:new(<<"user-key">>), mock_store, mock_client, []),
    true = meck:validate(mock_store).

check_operators(_) ->
    %% [[Operator, ContextValue, SegmentValue, Expected Result],]
    TestData = [
        %% numeric comparisons
        [<<"in">>, 99, 99, match],
        [<<"in">>, 99.0001, 99.0001, match],
        [<<"in">>, 99, 99.0001, no_match],
        [<<"in">>, 99.0001, 99, no_match],
        [<<"lessThan">>, 99, 99.0001, match],
        [<<"lessThan">>, 99.0001, 99, no_match],
        [<<"lessThan">>, 99, 99, no_match],
        [<<"lessThanOrEqual">>, 99, 99.0001, match],
        [<<"lessThanOrEqual">>, 99.0001, 99, no_match],
        [<<"lessThanOrEqual">>, 99, 99, match],
        [<<"greaterThan">>, 99.0001, 99, match],
        [<<"greaterThan">>, 99, 99.0001, no_match],
        [<<"greaterThan">>, 99, 99, no_match],
        [<<"greaterThanOrEqual">>, 99.0001, 99, match],
        [<<"greaterThanOrEqual">>, 99, 99.0001, no_match],
        [<<"greaterThanOrEqual">>, 99, 99, match],

        %% boolean
        [<<"in">>, true, true, match],
        [<<"in">>, false, false, match],
        [<<"in">>, true, false, no_match],
        [<<"in">>, false, true, no_match],

        %% string comparisons
        [<<"in">>, <<"x">>, <<"x">>, match],
        [<<"in">>, <<"x">>, <<"xyz">>, no_match],
        [<<"startsWith">>, <<"xyz">>, <<"x">>, match],
        [<<"startsWith">>, <<"x">>, <<"xyz">>, no_match],
        [<<"endsWith">>, <<"xyz">>, <<"z">>, match],
        [<<"endsWith">>, <<"z">>, <<"xyz">>, no_match],
        [<<"contains">>, <<"xyz">>, <<"y">>, match],
        [<<"contains">>, <<"y">>, <<"xyz">>, no_match],

        %% mixed strings and numbers
        [<<"in">>, <<"99">>, 99, no_match],
        [<<"in">>, 99, <<"99">>, no_match],
        [<<"contains">>, <<"99">>, 99, no_match],
        [<<"startsWith">>, <<"99">>, 99, no_match],
        [<<"endsWith">>, <<"99">>, 99, no_match],
        [<<"lessThanOrEqual">>, <<"99">>, 99, no_match],
        [<<"lessThanOrEqual">>, 99, <<"99">>, no_match],
        [<<"greaterThanOrEqual">>, <<"99">>, 99, no_match],
        [<<"greaterThanOrEqual">>, 99, <<"99">>, no_match],

        %% regex
        [<<"matches">>, <<"hello world">>, <<"hello.*rld">>, match],
        [<<"matches">>, <<"hello world">>, <<"hello.*rl">>, match],
        [<<"matches">>, <<"hello world">>, <<"l+">>, match],
        [<<"matches">>, <<"hello world">>, <<"(world|planet)">>, match],
        [<<"matches">>, <<"hello world">>, <<"aloha">>, no_match],
        [<<"matches">>, <<"hello world">>, <<"***not a regex">>, no_match],
        [<<"matches">>, <<"hello world">>, 3, no_match],
        [<<"matches">>, 3, <<"hello">>, no_match],

        %% dates
        [<<"before">>, 0, 1, match],
        [<<"before">>, -100, 0, match],
        [<<"before">>, <<"1970-01-01T00:00:00Z">>, 1000, match],
        [<<"before">>, <<"1970-01-01T00:00:00.500Z">>, 1000, match],
        [<<"before">>, true, 1000, no_match], %% wrong type
        [<<"after">>, <<"1970-01-01T00:00:02.500Z">>, 1000, match],
        %% The erlang RFC3339 implementation in erlang is more lenient than our specification.
        %%[<<"after">>, <<"1970-01-01 00:00:02.500Z">>, 1000, no_match], %% malformed timestamp
        [<<"before">>, <<"1970-01-01T00:00:02+01:00">>, 1000, match],
        [<<"before">>, -1000, 1000, match],
        [<<"after">>, <<"1970-01-01T00:00:01.001Z">>, 1000, match],
        [<<"after">>, <<"1970-01-01T00:00:00-01:00">>, 1000, match],

        %% semver
        [<<"semVerEqual">>, <<"2.0.1">>, <<"2.0.1">>, match],
        [<<"semVerEqual">>, <<"2.0.1">>, <<"02.0.1">>, no_match], %% leading zeroes should be disallowed
        [<<"semVerEqual">>, <<"2.0">>, <<"2.0.0">>, match],
        [<<"semVerEqual">>, <<"2">>, <<"2.0.0">>, match],
        [<<"semVerEqual">>, <<"2-rc1">>, <<"2.0.0-rc1">>, match],
        [<<"semVerEqual">>, <<"2+build2">>, <<"2.0.0+build2">>, match],
        [<<"semVerEqual">>, <<"2.0.0">>, <<"2.0.0+build2">>, match], %% build metadata should be ignored in comparison
        [<<"semVerEqual">>, <<"2.0.0">>, <<"2.0.0-rc1">>, no_match], %% prerelease should not be ignored
        [<<"semVerEqual">>, <<"2.0.0">>, <<"2.0.0+build_2">>, no_match], %% enforce allowable character set in build metadata
        [<<"semVerEqual">>, <<"2.0.0">>, <<"v2.0.0">>, no_match], %% disallow leading "v"
        [<<"semVerLessThan">>, <<"2.0.0">>, <<"2.0.1">>, match],
        [<<"semVerLessThan">>, <<"2.0">>, <<"2.0.1">>, match],
        [<<"semVerLessThan">>, <<"2.0.1">>, <<"2.0.0">>, no_match],
        [<<"semVerLessThan">>, <<"2.0.1">>, <<"2.0">>, no_match],
        [<<"semVerLessThan">>, <<"2.0.0-rc">>, <<"2.0.0-rc.beta">>, match],
        [<<"semVerLessThan">>, <<"2.0.0-rc">>, <<"2.0.0">>, match],
        [<<"semVerLessThan">>, <<"2.0.0-rc.3">>, <<"2.0.0-rc.29">>, match],
        [<<"semVerLessThan">>, <<"2.0.0-rc.x29">>, <<"2.0.0-rc.x3">>, match],
        [<<"semVerGreaterThan">>, <<"2.0.1">>, <<"2.0.0">>, match],
        [<<"semVerGreaterThan">>, <<"2.0.1">>, <<"2.0">>, match],
        [<<"semVerGreaterThan">>, <<"2.0.0">>, <<"2.0.1">>, no_match],
        [<<"semVerGreaterThan">>, <<"2.0">>, <<"2.0.1">>, no_match],
        [<<"semVerGreaterThan">>, <<"2.0.0-rc.1">>, <<"2.0.0-rc.0">>, match],
        [<<"semVerLessThan">>, <<"2.0.1">>, <<"xbad%ver">>, no_match],
        [<<"semVerGreaterThan">>, <<"2.0.1">>, <<"xbad%ver">>, no_match]
    ],
    %% Verify each as a user context.
    lists:foreach(fun([Operator, ContextValue, ClauseValue, Result] = _Scenario) ->
        Clause = ldclient_clause:new(#{
            <<"attribute">> => <<"attrToTest">>,
            <<"contextKind">> => <<"user">>,
            <<"op">> => Operator,
            <<"values">> => [ClauseValue]
        }),
        Context =
            ldclient_context:set(<<"attrToTest">>, ContextValue,
            ldclient_context:new(<<"user-key">>)),
        true = case Result =:= ldclient_clause:match_context(Clause, Context, null, null) of
            true -> true;
            false ->
                error_logger:error_msg("User context. Op ~p, ContextValue $p, ClauseValue ~p Expected ~p", [
                    Operator,
                    ContextValue,
                    ClauseValue,
                    Result
                ]),
                false
        end
      end, TestData),
    %% Verify each with a non-user kind.
    lists:foreach(fun([Operator, ContextValue, ClauseValue, Result] = _Scenario) ->
        Clause = ldclient_clause:new(#{
            <<"attribute">> => <<"attrToTest">>,
            <<"contextKind">> => <<"organization">>,
            <<"op">> => Operator,
            <<"values">> => [ClauseValue]
        }),
        Context =
            ldclient_context:set(<<"attrToTest">>, ContextValue,
                ldclient_context:new(<<"user-key">>, <<"organization">>)),
        true = case Result =:= ldclient_clause:match_context(Clause, Context, null, null) of
                   true -> true;
                   false ->
                       error_logger:error_msg("Non-user context. Op ~p, ContextValue $p, ClauseValue ~p Expected ~p", [
                           Operator,
                           ContextValue,
                           ClauseValue,
                           Result
                       ]),
                       false
               end
                  end, TestData),
    %% Verify that different kind never matches
    lists:foreach(fun([Operator, ContextValue, ClauseValue, Result] = _Scenario) ->
        Clause = ldclient_clause:new(#{
            <<"attribute">> => <<"attrToTest">>,
            <<"contextKind">> => <<"organization">>,
            <<"op">> => Operator,
            <<"values">> => [ClauseValue]
        }),
        Context =
            ldclient_context:set(<<"attrToTest">>, ContextValue,
                ldclient_context:new(<<"user-key">>, <<"company">>)),
        %% Never matches.
        true = case no_match =:= ldclient_clause:match_context(Clause, Context, null, null) of
                   true -> true;
                   false ->
                       error_logger:error_msg("Mistmatched context kinds, should never match."
                       " Op ~p, ContextValue $p, ClauseValue ~p Expected ~p", [
                           Operator,
                           ContextValue,
                           ClauseValue,
                           no_match
                       ]),
                       false
               end
                  end, TestData).
