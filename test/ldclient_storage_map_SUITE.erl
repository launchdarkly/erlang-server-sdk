-module(ldclient_storage_map_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    server_init/1,
    server_bucket_exists/1,
    server_get_put/1,
    server_put_clean/1,
    server_list/1,
    server_process_events_put/1,
    server_process_events_patch/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        server_init,
        server_bucket_exists,
        server_get_put,
        server_put_clean,
        server_list,
        server_process_events_put,
        server_process_events_patch
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ldclient),
    Options = #{
        storage_backend => ldclient_storage_map,
        stream => false,
        polling_update_requestor => ldclient_update_requestor_test
    },
    ldclient:start_instance("", Options),
    ldclient:start_instance("", another1, Options),
    Config.

end_per_suite(_) ->
    ok = application:stop(ldclient).

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok = ldclient_storage_map:empty(default, flags),
    ok = ldclient_storage_map:empty(another1, flags),
    ok.

%%====================================================================
%% Helpers
%%====================================================================

%%====================================================================
%% Tests
%%====================================================================

server_init(_) ->
    % Verify flags and segments buckets are pre-created
    {error, already_exists, _} = ldclient_storage_map:create(default, flags),
    {error, already_exists, _} = ldclient_storage_map:create(default, segments),
    {error, already_exists, _} = ldclient_storage_map:create(another1, flags),
    {error, already_exists, _} = ldclient_storage_map:create(another1, segments).

server_bucket_exists(_) ->
    {error, bucket_not_found, _} = ldclient_storage_map:get(default, ldclient_testing, <<"testing">>),
    ok = ldclient_storage_map:create(default, ldclient_testing),
    [] = ldclient_storage_map:get(default, ldclient_testing, <<"testing">>),
    {error, bucket_not_found, _} = ldclient_storage_map:get(another1, ldclient_testing, <<"testing">>).

server_get_put(_) ->
    [] = ldclient_storage_map:get(default, flags, <<"flag1">>),
    ok = ldclient_storage_map:put(default, flags, #{<<"flag1">> => [{<<"value1">>, 0.5}]}),
    [] = ldclient_storage_map:get(another1, flags, <<"flag1">>),
    [{<<"flag1">>, [{<<"value1">>, 0.5}]}] = ldclient_storage_map:get(default, flags, <<"flag1">>),
    [] = ldclient_storage_map:get(another1, flags, <<"flag1">>),
    ok = ldclient_storage_map:put(another1, flags, #{<<"flag1">> => [{<<"valueA">>, 0.9}]}),
    [{<<"flag1">>, [{<<"valueA">>, 0.9}]}] = ldclient_storage_map:get(another1, flags, <<"flag1">>),
    [{<<"flag1">>, [{<<"value1">>, 0.5}]}] = ldclient_storage_map:get(default, flags, <<"flag1">>).

server_put_clean(_) ->
    ok = ldclient_storage_map:put(default, flags, #{<<"flag1">> => [{<<"value1">>, 0.5}]}),
    [{<<"flag1">>, [{<<"value1">>, 0.5}]}] = ldclient_storage_map:get(default, flags, <<"flag1">>),
    ok = ldclient_storage_map:put_clean(default, flags, #{<<"flag2">> => [{<<"value2">>, 0.9}]}),
    [{<<"flag2">>, [{<<"value2">>, 0.9}]}] = ldclient_storage_map:list(default, flags).

server_list(_) ->
    [] = ldclient_storage_map:list(default, flags),
    [] = ldclient_storage_map:list(another1, flags),
    ok = ldclient_storage_map:put(default, flags, #{<<"flag1">> => [{<<"value1">>, 0.5}]}),
    ok = ldclient_storage_map:put(default, flags, #{<<"flag2">> => [{<<"value2">>, 0.7}]}),
    ok = ldclient_storage_map:put(another1, flags, #{<<"flag1">> => [{<<"value1">>, 0.9}]}),
    ok = ldclient_storage_map:put(another1, flags, #{<<"flag5">> => [{<<"value2">>, 0.77}]}),
    [
        {<<"flag1">>, [{<<"value1">>, 0.5}]},
        {<<"flag2">>, [{<<"value2">>, 0.7}]}
    ] = lists:sort(ldclient_storage_map:list(default, flags)),
    [
        {<<"flag1">>, [{<<"value1">>, 0.9}]},
        {<<"flag5">>, [{<<"value2">>, 0.77}]}
    ] = lists:sort(ldclient_storage_map:list(another1, flags)).

server_process_events_put(_) ->
    Event = #{
        <<"path">> => <<"/">>,
        <<"data">> => #{
            <<"flags">> => #{
                <<"flag-key-1">> => #{<<"key">> => <<"flag-key-1">>, <<"on">> => true},
                <<"flag-key-2">> => #{<<"key">> => <<"flag-key-2">>, <<"on">> => true},
                <<"flag-key-3">> => #{<<"key">> => <<"flag-key-3">>, <<"on">> => true}
            },
            <<"segments">> => #{
                <<"segment-key-1">> => #{<<"key">> => <<"segment-value-1">>},
                <<"segment-key-2">> => #{<<"key">> => <<"segment-value-2">>}
            }
        }
    },
    ok = ldclient_update_stream_server:process_items(put, Event, ldclient_storage_map, default),
    [
        {<<"flag-key-1">>, #{debug_events_until_date := null,deleted := false,
               fallthrough := #{bucket_by := key,variations := []},
               key := <<"flag-key-1">>,off_variation := 0,on := true,
               prerequisites := [],rules := [],salt := <<>>,targets := [],
               track_events := false,track_events_fallthrough := false,
               variations := [],version := 0}},
        {<<"flag-key-2">>, #{debug_events_until_date := null,deleted := false,
               fallthrough := #{bucket_by := key,variations := []},
               key := <<"flag-key-2">>,off_variation := 0,on := true,
               prerequisites := [],rules := [],salt := <<>>,targets := [],
               track_events := false,track_events_fallthrough := false,
               variations := [],version := 0}},
        {<<"flag-key-3">>, #{debug_events_until_date := null,deleted := false,
               fallthrough := #{bucket_by := key,variations := []},
               key := <<"flag-key-3">>,off_variation := 0,on := true,
               prerequisites := [],rules := [],salt := <<>>,targets := [],
               track_events := false,track_events_fallthrough := false,
               variations := [],version := 0}}
    ] = lists:sort(ldclient_storage_map:list(default, flags)),
    [
        {<<"segment-key-1">>, #{deleted := false,excluded := [],included := [],key := <<"segment-value-1">>,rules := [],
              salt := <<>>,version := 0}},
        {<<"segment-key-2">>, #{deleted := false,excluded := [],included := [],key := <<"segment-value-2">>,rules := [],
              salt := <<>>,version := 0}}
    ] = lists:sort(ldclient_storage_map:list(default, segments)).

server_process_events_patch(_) ->
    PutEvent = #{
        <<"path">> => <<"/">>,
        <<"data">> => #{
            <<"flags">> => #{
                <<"flag-key-1">> => #{<<"key">> => <<"flag-key-1">>, <<"on">> => true},
                <<"flag-key-2">> => #{<<"key">> => <<"flag-key-2">>, <<"on">> => true}
            },
            <<"segments">> => #{}
        }
    },
    ok = ldclient_update_stream_server:process_items(put, PutEvent, ldclient_storage_map, default),
    PatchEvent = #{
        <<"path">> => <<"/flags/flag-key-2">>,
        <<"data">> => #{debug_events_until_date => null,deleted => false,
               fallthrough => #{bucket_by => key,variations => []},
               key => <<"flag-key-2">>,off_variation => 0,on => false,
               prerequisites =>[],rules => [],salt => <<>>,targets => [],
               track_events => false,track_events_fallthrough => false,
               variations => [],version =>1}
    },
    ok = ldclient_update_stream_server:process_items(patch, PatchEvent, ldclient_storage_map, default),
    [
        {<<"flag-key-1">>, #{debug_events_until_date := null,deleted := false,
               fallthrough := #{bucket_by := key,variations := []},
               key := <<"flag-key-1">>,off_variation := 0,on := true,
               prerequisites := [],rules := [],salt := <<>>,targets := [],
               track_events := false,track_events_fallthrough := false,
               variations := [],version := 0}},
        {<<"flag-key-2">>, #{debug_events_until_date := null,deleted := false,
               fallthrough := #{bucket_by := key,variations := []},
               key := <<"flag-key-2">>,off_variation := 0,on := false,
               prerequisites := [],rules := [],salt := <<>>,targets := [],
               track_events := false,track_events_fallthrough := false,
               variations := [],version := 1}}
    ] = lists:sort(ldclient_storage_map:list(default, flags)).
