-module(ldclient_http_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    parse_uris/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        parse_uris
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

parse_uris(_) ->
    {ok, {http, "sdk.launchdarkly.com", 80, "/all", []}} = ldclient_http:uri_parse("http://sdk.launchdarkly.com/all"),
    {ok, {http, "sdk.launchdarkly.com", 8080, "/all", []}} = ldclient_http:uri_parse("http://sdk.launchdarkly.com:8080/all"),
    {ok, {https, "sdk.launchdarkly.com", 8080, "/all", []}} = ldclient_http:uri_parse("https://sdk.launchdarkly.com:8080/all"),
    {ok, {https, "sdk.launchdarkly.com", 8080, "/all", "yes=no&no=yes"}} = ldclient_http:uri_parse("https://sdk.launchdarkly.com:8080/all?yes=no&no=yes"),
    {ok, {https, "sdk.launchdarkly.com", 443, "/all", []}} = ldclient_http:uri_parse("https://sdk.launchdarkly.com/all").
