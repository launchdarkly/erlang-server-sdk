-module(eld_stream_SUITE).

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
    server_process_event_put_patch_old_version/1,
    server_process_event_put_delete/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        server_process_event_put_patch,
        server_process_event_put_patch_old_version,
        server_process_event_put_delete
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(eld),
    eld:start_instance("", #{start_stream => false}),
    Config.

end_per_suite(_) ->
    ok = application:stop(eld).

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok = eld_storage_ets:empty(default, flags),
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
    ok = eld_stream_server:process_event(#{event => <<"put">>, data => PutData}, eld_storage_ets, default),
    [] = eld_storage_ets:list(default, segments),
    [{FlagSimpleKey, FlagSimpleMap}] = eld_storage_ets:list(default, flags),
    {FlagSimpleKey, FlagPatchBin, FlagPatchMap} = get_simple_flag_patch(),
    PatchData = <<"{\"path\":\"/flags/", FlagSimpleKey/binary, "\",", FlagPatchBin/binary, "}">>,
    ok = eld_stream_server:process_event(#{event => <<"patch">>, data => PatchData}, eld_storage_ets, default),
    [{FlagSimpleKey, FlagPatchMap}] = eld_storage_ets:list(default, flags),
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
    ok = eld_stream_server:process_event(#{event => <<"put">>, data => PutData}, eld_storage_ets, default),
    [] = eld_storage_ets:list(default, segments),
    [{FlagSimpleKey, FlagSimpleMap}] = eld_storage_ets:list(default, flags),
    {FlagSimpleKey, FlagPatchBin, _FlagPatchMap} = get_simple_flag_patch_old(),
    PatchData = <<"{\"path\":\"/flags/", FlagSimpleKey/binary, "\",", FlagPatchBin/binary, "}">>,
    ok = eld_stream_server:process_event(#{event => <<"patch">>, data => PatchData}, eld_storage_ets, default),
    [{FlagSimpleKey, FlagSimpleMap}] = eld_storage_ets:list(default, flags),
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
    ok = eld_stream_server:process_event(#{event => <<"put">>, data => PutData}, eld_storage_ets, default),
    [] = eld_storage_ets:list(default, segments),
    [{FlagSimpleKey, FlagSimpleMap}] = eld_storage_ets:list(default, flags),
    ok = eld_stream_server:process_event(#{event => <<"delete">>, data => PutData}, eld_storage_ets, default),
    {FlagSimpleKey, _FlagDeleteBin, FlagDeleteMap} = get_simple_flag_delete(),
    [{FlagSimpleKey, FlagDeleteMap}] = eld_storage_ets:list(default, flags),
    ok.
