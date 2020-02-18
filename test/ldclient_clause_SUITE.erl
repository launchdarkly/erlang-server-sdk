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
    check_attribute_against_clause_value/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        check_attribute_against_clause_value
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
    true = ldclient_clause:check_attribute_against_clause_value(<<"2.0.0">>, semver_equal, <<"2.0.0">>),
    true = ldclient_clause:check_attribute_against_clause_value(<<"2.0">>, semver_equal, <<"2.0.0">>),
    true = ldclient_clause:check_attribute_against_clause_value(<<"2">>, semver_equal, <<"2.0.0">>),
    true = ldclient_clause:check_attribute_against_clause_value(<<"2-rc1">>, semver_equal, <<"2.0.0-rc1">>),
    true = ldclient_clause:check_attribute_against_clause_value(<<"2+build2">>, semver_equal, <<"2.0.0+build2">>),
    true = ldclient_clause:check_attribute_against_clause_value(<<"2+build123">>, semver_equal, <<"2.0.0+build2">>),
    false = ldclient_clause:check_attribute_against_clause_value(<<"2.0.0">>, semver_equal, <<"2.0.1">>),
    false = ldclient_clause:check_attribute_against_clause_value(<<"bad.vers.ion">>, semver_equal, <<"2.0.1">>),
    % Semver less than
    true = ldclient_clause:check_attribute_against_clause_value(<<"2.0.0">>, semver_less_than, <<"2.0.1">>),
    true = ldclient_clause:check_attribute_against_clause_value(<<"2.0">>, semver_less_than, <<"2.0.1">>),
    true = ldclient_clause:check_attribute_against_clause_value(<<"2">>, semver_less_than, <<"2.0.1">>),
    true = ldclient_clause:check_attribute_against_clause_value(<<"2.0.0-rc">>, semver_less_than, <<"2.0.0-rc.beta">>),
    false = ldclient_clause:check_attribute_against_clause_value(<<"2.0.1">>, semver_less_than, <<"2.0.0">>),
    false = ldclient_clause:check_attribute_against_clause_value(<<"2.0.1">>, semver_less_than, <<"2.0">>),
    false = ldclient_clause:check_attribute_against_clause_value(<<"2.0.1">>, semver_less_than, <<"2">>),
    false = ldclient_clause:check_attribute_against_clause_value(<<"bad.vers.ion">>, semver_less_than, <<"2">>),
    % Semver greater than
    true = ldclient_clause:check_attribute_against_clause_value(<<"2.0.1">>, semver_greater_than, <<"2.0.0">>),
    true = ldclient_clause:check_attribute_against_clause_value(<<"2.0.1">>, semver_greater_than, <<"2.0">>),
    true = ldclient_clause:check_attribute_against_clause_value(<<"2.0.1">>, semver_greater_than, <<"2">>),
    true = ldclient_clause:check_attribute_against_clause_value(<<"2.0.0-rc1">>, semver_greater_than, <<"2.0.0-rc0">>),
    false = ldclient_clause:check_attribute_against_clause_value(<<"2.0.0">>, semver_greater_than, <<"2.0.0">>),
    false = ldclient_clause:check_attribute_against_clause_value(<<"2.0.0">>, semver_greater_than, <<"2.0">>),
    false = ldclient_clause:check_attribute_against_clause_value(<<"2.0.0">>, semver_greater_than, <<"2">>),
    false = ldclient_clause:check_attribute_against_clause_value(<<"bad.vers.ion">>, semver_greater_than, <<"2">>),
    ok.
