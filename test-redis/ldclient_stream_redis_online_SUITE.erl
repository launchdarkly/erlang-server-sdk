-module(ldclient_stream_redis_online_SUITE).

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

sdk_options(Prefix) ->
    #{
        base_uri => "http://localhost:8888",
        stream_uri => "http://localhost:8888",
        events_uri => "http://localhost:8888",
        feature_store => ldclient_storage_redis,
        redis_prefix => Prefix
    }.

%%====================================================================
%% Tests
%%====================================================================

stream_sse_empty(_) ->
    ok = ldclient:start_instance("sdk-empty", sdk_options("prefixA")),
    % Wait for SDK to initialize and process initial payload from server
    timer:sleep(500),
    [] = ldclient_storage_redis:all(default, features),
    [] = ldclient_storage_redis:all(default, segments),
    ok = ldclient:stop_instance(),
    ok.

stream_sse_simple_flag(_) ->
    ok = ldclient:start_instance("sdk-simple-flag", sdk_options("prefixB")),
    % Wait for SDK to initialize and process initial payload from server
    timer:sleep(500),
    {FlagSimpleKey, _FlagSimpleBin, FlagSimpleMap} = ldclient_test_utils:get_simple_flag(),
    ParsedFlagSimpleMap = ldclient_flag:new(FlagSimpleMap),
    [{FlagSimpleKey, ParsedFlagSimpleMap}] = ldclient_storage_redis:all(default, features),
    [] = ldclient_storage_redis:all(default, segments),
    ok = ldclient:stop_instance(),
    ok.

stream_sse_put_no_path(_) ->
    ok = ldclient:start_instance("sdk-put-no-path", sdk_options("prefixC")),
    % Wait for SDK to initialize and process initial payload from server
    timer:sleep(500),
    {FlagSimpleKey, _FlagSimpleBin, FlagSimpleMap} = ldclient_test_utils:get_simple_flag(),
    ParsedFlagSimpleMap = ldclient_flag:new(FlagSimpleMap),
    [{FlagSimpleKey, ParsedFlagSimpleMap}] = ldclient_storage_redis:all(default, features),
    [] = ldclient_storage_redis:all(default, segments),
    ok = ldclient:stop_instance(),
    ok.

stream_sse_timeout(_) ->
    ok = ldclient:start_instance("sdk-timeout", sdk_options("prefixD")),

    % Evaluation before redis has been initialized.
    {null,foo,{error,client_not_ready}} = ldclient:variation_detail(<<"abc">>, #{key => <<"123">>}, foo),
    #{flag_values := #{}} = ldclient:all_flags_state(#{key => <<"123">>}),
    #{<<"$flagsState">> := #{},<<"$valid">> := false} = ldclient:all_flags_state(#{key => <<"123">>}, #{}, default),

    % Wait for SDK to initialize and process initial payload from server
    timer:sleep(6500),
    {FlagSimpleKey, _FlagSimpleBin, FlagSimpleMap} = ldclient_test_utils:get_simple_flag(),
    ParsedFlagSimpleMap = ldclient_flag:new(FlagSimpleMap),
    [{FlagSimpleKey, ParsedFlagSimpleMap}] = ldclient_storage_redis:all(default, features),
    [] = ldclient_storage_redis:all(default, segments),
    % Evaluation after SDK is initialized should return an expected flag variation value
    {0, true, fallthrough} = ldclient:variation_detail(<<"abc">>, #{key => <<"123">>}, foo),

    % Start a second instance that uses the already initialized redis DB (PrefixD$inited is present).
    ok = ldclient:start_instance("sdk-timeout", with_init_redis, sdk_options("prefixD")),
    % Evaluation before SDK is initialized, but after the redis store has been initialized, should return
    % the cached values.
    {0,true,fallthrough} = ldclient:variation_detail(<<"abc">>, #{key => <<"123">>}, foo, with_init_redis),
    #{flag_values := #{<<"abc">> := true}} = ldclient:all_flags_state(#{key => <<"123">>}, with_init_redis),
    #{<<"$flagsState">> :=
    #{<<"abc">> :=
    #{<<"reason">> := #{kind := <<"FALLTHROUGH">>},
        <<"trackEvents">> := true,<<"variation">> := 0,
        <<"version">> := 5}},
        <<"$valid">> := true,<<"abc">> := true} = ldclient:all_flags_state(#{key => <<"123">>}, #{with_reasons => true}, with_init_redis),

    % Client should not be initialized yet.
    false = ldclient:initialized(with_init_redis),

    ok = ldclient:stop_instance(),
    ok = ldclient:stop_instance(with_init_redis),
    ok.
