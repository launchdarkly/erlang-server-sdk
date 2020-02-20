-module(ldclient_stream_SUITE).

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
    server_process_event_other/1
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
        server_process_event_other
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ldclient),
    Options = #{
        stream => false,
        polling_update_requestor => ldclient_update_requestor_test
    },
    ldclient:start_instance("", Options),
    Config.

end_per_suite(_) ->
    ok = application:stop(ldclient).

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok = ldclient_storage_ets:empty(default, flags),
    ok.

%%====================================================================
%% Helpers
%%====================================================================

get_simple_flag() ->
    {
        <<"abc">>,
        <<"\"abc\":{",
            "\"clientSide\":false,",
            "\"debugEventsUntilDate\":null,",
            "\"deleted\":false,",
            "\"fallthrough\":{\"variation\":0},",
            "\"key\":\"abc\",",
            "\"offVariation\":1,",
            "\"on\":true,",
            "\"prerequisites\":[],",
            "\"rules\":[],",
            "\"salt\":\"d0888ec5921e45c7af5bc10b47b033ba\",",
            "\"sel\":\"8b4d79c59adb4df492ebea0bf65dfd4c\",",
            "\"targets\":[],",
            "\"trackEvents\":true,",
            "\"variations\":[true,false],",
            "\"version\":5",
        "}">>,
        #{
            <<"clientSide">> => false,
            <<"debugEventsUntilDate">> => null,
            <<"deleted">> => false,
            <<"fallthrough">> => #{<<"variation">> => 0},
            <<"key">> => <<"abc">>,
            <<"offVariation">> => 1,
            <<"on">> => true,
            <<"prerequisites">> => [],
            <<"rules">> => [],
            <<"salt">> => <<"d0888ec5921e45c7af5bc10b47b033ba">>,
            <<"sel">> => <<"8b4d79c59adb4df492ebea0bf65dfd4c">>,
            <<"targets">> => [],
            <<"trackEvents">> => true,
            <<"variations">> => [true,false],
            <<"version">> => 5
        }
    }.

get_simple_flag_patch() ->
    {
        <<"abc">>,
        <<"\"data\":{",
            "\"clientSide\":false,",
            "\"debugEventsUntilDate\":null,",
            "\"deleted\":false,",
            "\"fallthrough\":{\"variation\":1},",
            "\"key\":\"abc\",",
            "\"offVariation\":1,",
            "\"on\":true,",
            "\"prerequisites\":[],",
            "\"rules\":[],",
            "\"salt\":\"d0888ec5921e45c7af5bc10b47b033ba\",",
            "\"sel\":\"8b4d79c59adb4df492ebea0bf65dfd4c\",",
            "\"targets\":[],",
            "\"trackEvents\":true,",
            "\"variations\":[true,false],",
            "\"version\":6",
        "}">>,
        #{
            <<"clientSide">> => false,
            <<"debugEventsUntilDate">> => null,
            <<"deleted">> => false,
            <<"fallthrough">> => #{<<"variation">> => 1},
            <<"key">> => <<"abc">>,
            <<"offVariation">> => 1,
            <<"on">> => true,
            <<"prerequisites">> => [],
            <<"rules">> => [],
            <<"salt">> => <<"d0888ec5921e45c7af5bc10b47b033ba">>,
            <<"sel">> => <<"8b4d79c59adb4df492ebea0bf65dfd4c">>,
            <<"targets">> => [],
            <<"trackEvents">> => true,
            <<"variations">> => [true,false],
            <<"version">> => 6
        }
    }.

get_simple_flag_patch_old() ->
    {
        <<"abc">>,
        <<"\"data\":{",
            "\"clientSide\":false,",
            "\"debugEventsUntilDate\":null,",
            "\"deleted\":false,",
            "\"fallthrough\":{\"variation\":1},",
            "\"key\":\"abc\",",
            "\"offVariation\":1,",
            "\"on\":true,",
            "\"prerequisites\":[],",
            "\"rules\":[],",
            "\"salt\":\"d0888ec5921e45c7af5bc10b47b033ba\",",
            "\"sel\":\"8b4d79c59adb4df492ebea0bf65dfd4c\",",
            "\"targets\":[],",
            "\"trackEvents\":true,",
            "\"variations\":[true,false],",
            "\"version\":4",
            "}">>,
        #{
            <<"clientSide">> => false,
            <<"debugEventsUntilDate">> => null,
            <<"deleted">> => false,
            <<"fallthrough">> => #{<<"variation">> => 1},
            <<"key">> => <<"abc">>,
            <<"offVariation">> => 1,
            <<"on">> => true,
            <<"prerequisites">> => [],
            <<"rules">> => [],
            <<"salt">> => <<"d0888ec5921e45c7af5bc10b47b033ba">>,
            <<"sel">> => <<"8b4d79c59adb4df492ebea0bf65dfd4c">>,
            <<"targets">> => [],
            <<"trackEvents">> => true,
            <<"variations">> => [true,false],
            <<"version">> => 4
        }
    }.

get_simple_flag_delete() ->
    {
        <<"abc">>,
        <<"\"abc\":{",
            "\"clientSide\":false,",
            "\"debugEventsUntilDate\":null,",
            "\"deleted\":true,",
            "\"fallthrough\":{\"variation\":0},",
            "\"key\":\"abc\",",
            "\"offVariation\":1,",
            "\"on\":true,",
            "\"prerequisites\":[],",
            "\"rules\":[],",
            "\"salt\":\"d0888ec5921e45c7af5bc10b47b033ba\",",
            "\"sel\":\"8b4d79c59adb4df492ebea0bf65dfd4c\",",
            "\"targets\":[],",
            "\"trackEvents\":true,",
            "\"variations\":[true,false],",
            "\"version\":5",
        "}">>,
        #{
            <<"clientSide">> => false,
            <<"debugEventsUntilDate">> => null,
            <<"deleted">> => true,
            <<"fallthrough">> => #{<<"variation">> => 0},
            <<"key">> => <<"abc">>,
            <<"offVariation">> => 1,
            <<"on">> => true,
            <<"prerequisites">> => [],
            <<"rules">> => [],
            <<"salt">> => <<"d0888ec5921e45c7af5bc10b47b033ba">>,
            <<"sel">> => <<"8b4d79c59adb4df492ebea0bf65dfd4c">>,
            <<"targets">> => [],
            <<"trackEvents">> => true,
            <<"variations">> => [true,false],
            <<"version">> => 5
        }
    }.

get_flag_with_extra_property() ->
    {
        <<"abc">>,
        <<"\"abc\":{",
            "\"clientSide\":false,",
            "\"debugEventsUntilDate\":null,",
            "\"deleted\":false,",
            "\"fallthrough\":{\"variation\":0},",
            "\"key\":\"abc\",",
            "\"offVariation\":1,",
            "\"on\":true,",
            "\"prerequisites\":[],",
            "\"rules\":[],",
            "\"salt\":\"d0888ec5921e45c7af5bc10b47b033ba\",",
            "\"sel\":\"8b4d79c59adb4df492ebea0bf65dfd4c\",",
            "\"targets\":[],",
            "\"trackEvents\":true,",
            "\"variations\":[true,false],",
            "\"foo\":\"bar\",",
            "\"version\":5",
            "}">>,
        #{
            <<"clientSide">> => false,
            <<"debugEventsUntilDate">> => null,
            <<"deleted">> => false,
            <<"fallthrough">> => #{<<"variation">> => 0},
            <<"key">> => <<"abc">>,
            <<"offVariation">> => 1,
            <<"on">> => true,
            <<"prerequisites">> => [],
            <<"rules">> => [],
            <<"salt">> => <<"d0888ec5921e45c7af5bc10b47b033ba">>,
            <<"sel">> => <<"8b4d79c59adb4df492ebea0bf65dfd4c">>,
            <<"targets">> => [],
            <<"trackEvents">> => true,
            <<"variations">> => [true,false],
            <<"foo">> => <<"bar">>,
            <<"version">> => 5
        }
    }.

%%====================================================================
%% Tests
%%====================================================================

server_process_event_put_patch(_) ->
    {FlagSimpleKey, FlagSimpleBin, FlagSimpleMap} = get_simple_flag(),
    PutData = <<"{\"path\":\"/\",",
        "\"data\":{",
            "\"flags\":{",
                FlagSimpleBin/binary,
            "},",
            "\"segments\":{}",
        "}",
    "}">>,
    ok = ldclient_update_stream_server:process_event(#{event => <<"put">>, data => PutData}, ldclient_storage_ets, default),
    [] = ldclient_storage_ets:list(default, segments),
    [{FlagSimpleKey, FlagSimpleMap}] = ldclient_storage_ets:list(default, flags),
    {FlagSimpleKey, FlagPatchBin, FlagPatchMap} = get_simple_flag_patch(),
    PatchData = <<"{\"path\":\"/flags/", FlagSimpleKey/binary, "\",", FlagPatchBin/binary, "}">>,
    ok = ldclient_update_stream_server:process_event(#{event => <<"patch">>, data => PatchData}, ldclient_storage_ets, default),
    [{FlagSimpleKey, FlagPatchMap}] = ldclient_storage_ets:list(default, flags),
    ok.

server_process_event_put_patch_flag_with_extra_property(_) ->
    {FlagSimpleKey, FlagSimpleBin, FlagSimpleMap} = get_flag_with_extra_property(),
    PutData = <<"{\"path\":\"/\",",
        "\"data\":{",
        "\"flags\":{",
        FlagSimpleBin/binary,
        "},",
        "\"segments\":{}",
        "}",
        "}">>,
    ok = ldclient_update_stream_server:process_event(#{event => <<"put">>, data => PutData}, ldclient_storage_ets, default),
    [] = ldclient_storage_ets:list(default, segments),
    [{FlagSimpleKey, FlagSimpleMap}] = ldclient_storage_ets:list(default, flags),
    {FlagSimpleKey, FlagPatchBin, FlagPatchMap} = get_simple_flag_patch(),
    PatchData = <<"{\"path\":\"/flags/", FlagSimpleKey/binary, "\",", FlagPatchBin/binary, "}">>,
    ok = ldclient_update_stream_server:process_event(#{event => <<"patch">>, data => PatchData}, ldclient_storage_ets, default),
    [{FlagSimpleKey, FlagPatchMap}] = ldclient_storage_ets:list(default, flags),
    ok.

server_process_event_put_patch_old_version(_) ->
    {FlagSimpleKey, FlagSimpleBin, FlagSimpleMap} = get_simple_flag(),
    PutData = <<"{\"path\":\"/\",",
        "\"data\":{",
        "\"flags\":{",
        FlagSimpleBin/binary,
        "},",
        "\"segments\":{}",
        "}",
        "}">>,
    ok = ldclient_update_stream_server:process_event(#{event => <<"put">>, data => PutData}, ldclient_storage_ets, default),
    [] = ldclient_storage_ets:list(default, segments),
    [{FlagSimpleKey, FlagSimpleMap}] = ldclient_storage_ets:list(default, flags),
    {FlagSimpleKey, FlagPatchBin, _FlagPatchMap} = get_simple_flag_patch_old(),
    PatchData = <<"{\"path\":\"/flags/", FlagSimpleKey/binary, "\",", FlagPatchBin/binary, "}">>,
    ok = ldclient_update_stream_server:process_event(#{event => <<"patch">>, data => PatchData}, ldclient_storage_ets, default),
    [{FlagSimpleKey, FlagSimpleMap}] = ldclient_storage_ets:list(default, flags),
    ok.

server_process_event_put_delete(_) ->
    {FlagSimpleKey, FlagSimpleBin, FlagSimpleMap} = get_simple_flag(),
    PutData = <<"{\"path\":\"/\",",
        "\"data\":{",
            "\"flags\":{",
                FlagSimpleBin/binary,
            "},",
            "\"segments\":{}",
        "}",
    "}">>,
    ok = ldclient_update_stream_server:process_event(#{event => <<"put">>, data => PutData}, ldclient_storage_ets, default),
    [] = ldclient_storage_ets:list(default, segments),
    [{FlagSimpleKey, FlagSimpleMap}] = ldclient_storage_ets:list(default, flags),
    ok = ldclient_update_stream_server:process_event(#{event => <<"delete">>, data => PutData}, ldclient_storage_ets, default),
    {FlagSimpleKey, _FlagDeleteBin, FlagDeleteMap} = get_simple_flag_delete(),
    [{FlagSimpleKey, FlagDeleteMap}] = ldclient_storage_ets:list(default, flags),
    ok.

server_process_event_put_delete_single(_) ->
    {FlagSimpleKey, FlagSimpleBin, FlagSimpleMap} = get_simple_flag(),
    PutData = <<"{\"path\":\"/\",",
        "\"data\":{",
        "\"flags\":{",
        FlagSimpleBin/binary,
        "},",
        "\"segments\":{}",
        "}",
        "}">>,
    ok = ldclient_update_stream_server:process_event(#{event => <<"put">>, data => PutData}, ldclient_storage_ets, default),
    [] = ldclient_storage_ets:list(default, segments),
    [{FlagSimpleKey, FlagSimpleMap}] = ldclient_storage_ets:list(default, flags),
    DeleteData = <<"{\"path\":\"/flags/", FlagSimpleKey/binary, "\"}">>,
    ok = ldclient_update_stream_server:process_event(#{event => <<"delete">>, data => DeleteData}, ldclient_storage_ets, default),
    {FlagSimpleKey, _FlagDeleteBin, FlagDeleteMap} = get_simple_flag_delete(),
    [{FlagSimpleKey, FlagDeleteMap}] = ldclient_storage_ets:list(default, flags),
    ok.

server_process_event_other(_) ->
    ok = ldclient_update_stream_server:process_event(#{event => <<"unsupported-event">>, data => <<"foo">>}, ldclient_storage_ets, default),
    [] = ldclient_storage_ets:list(default, flags),
    [] = ldclient_storage_ets:list(default, segments),
    ok.
