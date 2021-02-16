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
