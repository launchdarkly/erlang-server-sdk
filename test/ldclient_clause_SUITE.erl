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
    supports_nested_segments/1
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
        supports_nested_segments
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
