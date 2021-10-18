-module(ldclient_stream_redis_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    server_process_event_put_patch/1,
    server_process_event_put_patch_flag_with_extra_property/1,
    server_process_event_put_patch_old_version/1,
    server_process_event_put_delete/1,
    server_process_event_put_delete_single/1,
    server_process_event_other/1,
    parse_shotgun_event/1,
    parse_shotgun_event_optional_spaces/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        server_process_event_put_patch,
        server_process_event_put_patch_flag_with_extra_property,
        server_process_event_put_patch_old_version,
        server_process_event_put_delete,
        server_process_event_put_delete_single,
        server_process_event_other,
        parse_shotgun_event,
        parse_shotgun_event_optional_spaces
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ldclient),
    Options = #{
        stream => false,
        polling_update_requestor => ldclient_update_requestor_test,
        feature_store => ldclient_storage_redis
    },
    ldclient:start_instance("", Options),
    Config.

end_per_suite(_) ->
    ok = application:stop(ldclient),
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok = ldclient_storage_redis:empty(default, features),
    ok = ldclient_storage_redis:empty(default, segments),
    ok.

%%====================================================================
%% Helpers
%%====================================================================

%%====================================================================
%% Tests
%%====================================================================

server_process_event_put_patch(_) ->
    {FlagSimpleKey, FlagSimpleBin, FlagSimpleMap} = ldclient_test_utils:get_simple_flag(),
    PutData = <<"{\"path\":\"/\",",
        "\"data\":{",
            "\"flags\":{",
                FlagSimpleBin/binary,
            "},",
            "\"segments\":{}",
        "}",
    "}">>,
    ok = ldclient_update_stream_server:process_event(#{event => <<"put">>, data => PutData}, ldclient_storage_redis, default),
    [] = ldclient_storage_redis:all(default, segments),
    ParsedFlagSimpleMap = ldclient_flag:new(FlagSimpleMap),
    [{FlagSimpleKey, ParsedFlagSimpleMap}] = ldclient_storage_redis:all(default, features),
    {FlagSimpleKey, FlagPatchBin, FlagPatchMap} = ldclient_test_utils:get_simple_flag_patch(),
    PatchData = <<"{\"path\":\"/flags/", FlagSimpleKey/binary, "\",", FlagPatchBin/binary, "}">>,
    ok = ldclient_update_stream_server:process_event(#{event => <<"patch">>, data => PatchData}, ldclient_storage_redis, default),
    ParsedFlagPatchMap = ldclient_flag:new(FlagPatchMap),
    [{FlagSimpleKey, ParsedFlagPatchMap}] = ldclient_storage_redis:all(default, features),
    ok.

server_process_event_put_patch_flag_with_extra_property(_) ->
    {FlagSimpleKey, FlagSimpleBin, FlagSimpleMap} = ldclient_test_utils:get_flag_with_extra_property(),
    PutData = <<"{\"path\":\"/\",",
        "\"data\":{",
            "\"flags\":{",
                FlagSimpleBin/binary,
            "},",
            "\"segments\":{}",
        "}",
    "}">>,
    ok = ldclient_update_stream_server:process_event(#{event => <<"put">>, data => PutData}, ldclient_storage_redis, default),
    [] = ldclient_storage_redis:all(default, segments),
    ParsedFlagSimpleMap = ldclient_flag:new(FlagSimpleMap),
    [{FlagSimpleKey, ParsedFlagSimpleMap}] = ldclient_storage_redis:all(default, features),
    {FlagSimpleKey, FlagPatchBin, FlagPatchMap} = ldclient_test_utils:get_simple_flag_patch(),
    PatchData = <<"{\"path\":\"/flags/", FlagSimpleKey/binary, "\",", FlagPatchBin/binary, "}">>,
    ok = ldclient_update_stream_server:process_event(#{event => <<"patch">>, data => PatchData}, ldclient_storage_redis, default),
    ParsedFlagPatchMap = ldclient_flag:new(FlagPatchMap),
    [{FlagSimpleKey, ParsedFlagPatchMap}] = ldclient_storage_redis:all(default, features),
    ok.

server_process_event_put_patch_old_version(_) ->
    {FlagSimpleKey, FlagSimpleBin, FlagSimpleMap} = ldclient_test_utils:get_simple_flag(),
    PutData = <<"{\"path\":\"/\",",
        "\"data\":{",
            "\"flags\":{",
                FlagSimpleBin/binary,
            "},",
            "\"segments\":{}",
        "}",
    "}">>,
    ok = ldclient_update_stream_server:process_event(#{event => <<"put">>, data => PutData}, ldclient_storage_redis, default),
    [] = ldclient_storage_redis:all(default, segments),
    ParsedFlagSimpleMap = ldclient_flag:new(FlagSimpleMap),
    [{FlagSimpleKey, ParsedFlagSimpleMap}] = ldclient_storage_redis:all(default, features),
    {FlagSimpleKey, FlagPatchBin, _FlagPatchMap} = ldclient_test_utils:get_simple_flag_patch_old(),
    PatchData = <<"{\"path\":\"/flags/", FlagSimpleKey/binary, "\",", FlagPatchBin/binary, "}">>,
    ok = ldclient_update_stream_server:process_event(#{event => <<"patch">>, data => PatchData}, ldclient_storage_redis, default),
    [{FlagSimpleKey, ParsedFlagSimpleMap}] = ldclient_storage_redis:all(default, features),
    ok.

server_process_event_put_delete(_) ->
    {FlagSimpleKey, FlagSimpleBin, FlagSimpleMap} = ldclient_test_utils:get_simple_flag(),
    PutData = <<"{\"path\":\"/\",",
        "\"data\":{",
            "\"flags\":{",
                FlagSimpleBin/binary,
            "},",
            "\"segments\":{}",
        "}",
    "}">>,
    ok = ldclient_update_stream_server:process_event(#{event => <<"put">>, data => PutData}, ldclient_storage_redis, default),
    [] = ldclient_storage_redis:all(default, segments),
    ParsedFlagSimpleMap = ldclient_flag:new(FlagSimpleMap),
    [{FlagSimpleKey, ParsedFlagSimpleMap}] = ldclient_storage_redis:all(default, features),
    ok = ldclient_update_stream_server:process_event(#{event => <<"delete">>, data => PutData}, ldclient_storage_redis, default),
    {FlagSimpleKey, _FlagDeleteBin, FlagDeleteMap} = ldclient_test_utils:get_simple_flag_delete(),
    ParsedFlagDeleteMap = ldclient_flag:new(FlagDeleteMap),
    [{FlagSimpleKey, ParsedFlagDeleteMap}] = ldclient_storage_redis:all(default, features),
    ok.

server_process_event_put_delete_single(_) ->
    {FlagSimpleKey, FlagSimpleBin, FlagSimpleMap} = ldclient_test_utils:get_simple_flag(),
    PutData = <<"{\"path\":\"/\",",
        "\"data\":{",
            "\"flags\":{",
                FlagSimpleBin/binary,
            "},",
            "\"segments\":{}",
        "}",
    "}">>,
    ok = ldclient_update_stream_server:process_event(#{event => <<"put">>, data => PutData}, ldclient_storage_redis, default),
    [] = ldclient_storage_redis:all(default, segments),
    ParsedFlagSimpleMap = ldclient_flag:new(FlagSimpleMap),
    [{FlagSimpleKey, ParsedFlagSimpleMap}] = ldclient_storage_redis:all(default, features),
    DeleteData = <<"{\"path\":\"/flags/", FlagSimpleKey/binary, "\"}">>,
    ok = ldclient_update_stream_server:process_event(#{event => <<"delete">>, data => DeleteData}, ldclient_storage_redis, default),
    {FlagSimpleKey, _FlagDeleteBin, FlagDeleteMap} = ldclient_test_utils:get_simple_flag_delete(),
    ParsedFlagDeleteMap = ldclient_flag:new(FlagDeleteMap),
    [{FlagSimpleKey, ParsedFlagDeleteMap}] = ldclient_storage_redis:all(default, features),
    ok.

server_process_event_other(_) ->
    ok = ldclient_update_stream_server:process_event(#{event => <<"unsupported-event">>, data => <<"foo">>}, ldclient_storage_redis, default),
    [] = ldclient_storage_redis:all(default, features),
    [] = ldclient_storage_redis:all(default, segments),
    ok.

parse_shotgun_event(_) ->
    EventBin = <<"event:put\ndata:foo">>,
    ExpectedEvent = #{event => <<"put">>, data => <<"foo\n">>},
    ExpectedEvent = ldclient_update_stream_server:parse_shotgun_event(EventBin).

parse_shotgun_event_optional_spaces(_) ->
    EventBin = <<"event: put\ndata: foo">>,
    ExpectedEvent = #{event => <<"put">>, data => <<"foo\n">>},
    ExpectedEvent = ldclient_update_stream_server:parse_shotgun_event(EventBin).
