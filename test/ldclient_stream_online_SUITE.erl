-module(ldclient_stream_online_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    stream_sse_empty/1,
    stream_sse_simple_flag/1,
    stream_sse_put_no_path/1,
    stream_sse_timeout/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        stream_sse_empty,
        stream_sse_simple_flag,
        stream_sse_put_no_path,
        stream_sse_timeout
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(http_server),
    {ok, _} = application:ensure_all_started(ldclient),
    Config.

end_per_suite(_) ->
    ok = application:stop(ldclient),
    ok = application:stop(http_server),
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%%====================================================================
%% Helpers
%%====================================================================

sdk_options() ->
    #{
        base_uri => "http://localhost:8888",
        stream_uri => "http://localhost:8888",
        events_uri => "http://localhost:8888"
    }.

%%====================================================================
%% Tests
%%====================================================================

stream_sse_empty(_) ->
    ok = ldclient:start_instance("sdk-empty", sdk_options()),
    % Wait for SDK to initialize and process initial payload from server
    timer:sleep(500),
    [] = ldclient_storage_ets:list(default, flags),
    [] = ldclient_storage_ets:list(default, segments),
    ok = ldclient:stop_instance(),
    ok.

stream_sse_simple_flag(_) ->
    ok = ldclient:start_instance("sdk-simple-flag", sdk_options()),
    % Wait for SDK to initialize and process initial payload from server
    timer:sleep(500),
    {FlagSimpleKey, _FlagSimpleBin, FlagSimpleMap} = ldclient_test_utils:get_simple_flag(),
    ParsedFlagSimpleMap = ldclient_flag:new(FlagSimpleMap),
    [{FlagSimpleKey, ParsedFlagSimpleMap}] = ldclient_storage_ets:list(default, flags),
    [] = ldclient_storage_ets:list(default, segments),
    ok = ldclient:stop_instance(),
    ok.

stream_sse_put_no_path(_) ->
    ok = ldclient:start_instance("sdk-put-no-path", sdk_options()),
    % Wait for SDK to initialize and process initial payload from server
    timer:sleep(500),
    {FlagSimpleKey, _FlagSimpleBin, FlagSimpleMap} = ldclient_test_utils:get_simple_flag(),
    ParsedFlagSimpleMap = ldclient_flag:new(FlagSimpleMap),
    [{FlagSimpleKey, ParsedFlagSimpleMap}] = ldclient_storage_ets:list(default, flags),
    [] = ldclient_storage_ets:list(default, segments),
    ok = ldclient:stop_instance(),
    ok.

stream_sse_timeout(_) ->
    ok = ldclient:start_instance("sdk-timeout", sdk_options()),
    % Evaluation before SDK is initialized should return client_not_ready with default value
    {null, foo, {error, client_not_ready}} = ldclient:variation_detail(<<"abc">>, #{key => <<"123">>}, foo),
    % Wait for SDK to initialize and process initial payload from server
    timer:sleep(5500),
    {FlagSimpleKey, _FlagSimpleBin, FlagSimpleMap} = ldclient_test_utils:get_simple_flag(),
    ParsedFlagSimpleMap = ldclient_flag:new(FlagSimpleMap),
    [{FlagSimpleKey, ParsedFlagSimpleMap}] = ldclient_storage_ets:list(default, flags),
    [] = ldclient_storage_ets:list(default, segments),
    % Evaluation after SDK is initialized should return an expected flag variation value
    {0, true, fallthrough} = ldclient:variation_detail(<<"abc">>, #{key => <<"123">>}, foo),
    ok = ldclient:stop_instance(),
    ok.
