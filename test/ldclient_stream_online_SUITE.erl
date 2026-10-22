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
    stream_sse_timeout/1,
    stream_sse_read_timeout_reconnects/1,
    stream_sse_heartbeats_keep_connection_alive/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        stream_sse_empty,
        stream_sse_simple_flag,
        stream_sse_put_no_path,
        stream_sse_timeout,
        stream_sse_read_timeout_reconnects,
        stream_sse_heartbeats_keep_connection_alive
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
    [] = ldclient_storage_ets:all(default, features),
    [] = ldclient_storage_ets:all(default, segments),
    ok = ldclient:stop_instance(),
    ok.

stream_sse_simple_flag(_) ->
    ok = ldclient:start_instance("sdk-simple-flag", sdk_options()),
    % Wait for SDK to initialize and process initial payload from server
    timer:sleep(500),
    {FlagSimpleKey, _FlagSimpleBin, FlagSimpleMap} = ldclient_test_utils:get_simple_flag(),
    ParsedFlagSimpleMap = ldclient_flag:new(FlagSimpleMap),
    [{FlagSimpleKey, ParsedFlagSimpleMap}] = ldclient_storage_ets:all(default, features),
    [] = ldclient_storage_ets:all(default, segments),
    ok = ldclient:stop_instance(),
    ok.

stream_sse_put_no_path(_) ->
    ok = ldclient:start_instance("sdk-put-no-path", sdk_options()),
    % Wait for SDK to initialize and process initial payload from server
    timer:sleep(500),
    {FlagSimpleKey, _FlagSimpleBin, FlagSimpleMap} = ldclient_test_utils:get_simple_flag(),
    ParsedFlagSimpleMap = ldclient_flag:new(FlagSimpleMap),
    [{FlagSimpleKey, ParsedFlagSimpleMap}] = ldclient_storage_ets:all(default, features),
    [] = ldclient_storage_ets:all(default, segments),
    ok = ldclient:stop_instance(),
    ok.

stream_sse_timeout(_) ->
    ok = ldclient:start_instance("sdk-timeout", sdk_options()),
    % Evaluation before SDK is initialized should return client_not_ready with default value
    {null, foo, {error, client_not_ready}} = ldclient:variation_detail(<<"abc">>, #{key => <<"123">>}, foo),
    % Wait for SDK to initialize and process initial payload from server
    timer:sleep(6500),
    {FlagSimpleKey, _FlagSimpleBin, FlagSimpleMap} = ldclient_test_utils:get_simple_flag(),
    ParsedFlagSimpleMap = ldclient_flag:new(FlagSimpleMap),
    [{FlagSimpleKey, ParsedFlagSimpleMap}] = ldclient_storage_ets:all(default, features),
    [] = ldclient_storage_ets:all(default, segments),
    % Evaluation after SDK is initialized should return an expected flag variation value
    {0, true, fallthrough} = ldclient:variation_detail(<<"abc">>, #{key => <<"123">>}, foo),
    ok = ldclient:stop_instance(),
    ok.

stream_sse_read_timeout_reconnects(_) ->
    % Silent after the initial put: the SDK must reconnect and apply the updated flag
    true = register(read_timeout_test, self()),
    ok = http_server_sse_handler:reset_connections(<<"sdk-read-timeout">>),
    try
        ok = application:set_env(ldclient, stream_read_timeout_ms, 1000),
        ok = ldclient:start_instance("sdk-read-timeout", sdk_options()),
        First =
            receive {stream_connected, FirstPid} -> FirstPid
            after 2000 -> ct:fail(no_initial_connection)
            end,
        timer:sleep(300),
        {0, true, fallthrough} = ldclient:variation_detail(<<"abc">>, #{key => <<"123">>}, foo),
        _Second =
            receive {stream_connected, SecondPid} when SecondPid =/= First -> SecondPid
            after 4000 -> ct:fail(no_reconnect_after_read_timeout)
            end,
        timer:sleep(500),
        {FlagSimpleKey, _FlagSimpleBin, _FlagSimpleMap} = ldclient_test_utils:get_simple_flag(),
        [{FlagSimpleKey, UpdatedFlag}] = ldclient_storage_ets:all(default, features),
        6 = maps:get(version, UpdatedFlag),
        {1, false, fallthrough} = ldclient:variation_detail(<<"abc">>, #{key => <<"123">>}, foo),
        ok = ldclient:stop_instance()
    after
        application:unset_env(ldclient, stream_read_timeout_ms),
        unregister(read_timeout_test)
    end,
    ok.

stream_sse_heartbeats_keep_connection_alive(_) ->
    % Only heartbeat comments after the initial put: they must count as activity
    true = register(read_timeout_test, self()),
    ok = http_server_sse_handler:reset_connections(<<"sdk-heartbeat">>),
    try
        ok = application:set_env(ldclient, stream_read_timeout_ms, 1000),
        ok = ldclient:start_instance("sdk-heartbeat", sdk_options()),
        receive {stream_connected, _FirstPid} -> ok
        after 2000 -> ct:fail(no_initial_connection)
        end,
        receive {stream_connected, _SecondPid} -> ct:fail(reconnected_despite_heartbeats)
        after 3500 -> ok
        end,
        {0, true, fallthrough} = ldclient:variation_detail(<<"abc">>, #{key => <<"123">>}, foo),
        ok = ldclient:stop_instance()
    after
        application:unset_env(ldclient, stream_read_timeout_ms),
        unregister(read_timeout_test)
    end,
    ok.

