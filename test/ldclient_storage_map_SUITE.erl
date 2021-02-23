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
    server_get_upsert/1,
    server_upsert_clean/1,
    server_all/1,
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
        server_get_upsert,
        server_upsert_clean,
        server_all,
        server_process_events_put,
        server_process_events_patch
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ldclient),
    Options = #{
        feature_store => ldclient_storage_map,
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
    ok = ldclient_storage_map:empty(default, features),
    ok = ldclient_storage_map:empty(another1, features),
    ok.

%%====================================================================
%% Helpers
%%====================================================================

%%====================================================================
%% Tests
%%====================================================================

server_init(_) ->
    % Verify features and segments buckets are pre-created
    {error, already_exists, _} = ldclient_storage_map:create(default, features),
    {error, already_exists, _} = ldclient_storage_map:create(default, segments),
    {error, already_exists, _} = ldclient_storage_map:create(another1, features),
    {error, already_exists, _} = ldclient_storage_map:create(another1, segments).

server_bucket_exists(_) ->
    {error, bucket_not_found, _} = ldclient_storage_map:get(default, ldclient_testing, <<"testing">>),
    ok = ldclient_storage_map:create(default, ldclient_testing),
    [] = ldclient_storage_map:get(default, ldclient_testing, <<"testing">>),
    {error, bucket_not_found, _} = ldclient_storage_map:get(another1, ldclient_testing, <<"testing">>).

server_get_upsert(_) ->
    [] = ldclient_storage_map:get(default, features, <<"flag1">>),
    ok = ldclient_storage_map:upsert(default, features, #{<<"flag1">> => [{<<"value1">>, 0.5}]}),
    [] = ldclient_storage_map:get(another1, features, <<"flag1">>),
    [{<<"flag1">>, [{<<"value1">>, 0.5}]}] = ldclient_storage_map:get(default, features, <<"flag1">>),
    [] = ldclient_storage_map:get(another1, features, <<"flag1">>),
    ok = ldclient_storage_map:upsert(another1, features, #{<<"flag1">> => [{<<"valueA">>, 0.9}]}),
    [{<<"flag1">>, [{<<"valueA">>, 0.9}]}] = ldclient_storage_map:get(another1, features, <<"flag1">>),
    [{<<"flag1">>, [{<<"value1">>, 0.5}]}] = ldclient_storage_map:get(default, features, <<"flag1">>).

server_upsert_clean(_) ->
    ok = ldclient_storage_map:upsert(default, features, #{<<"flag1">> => [{<<"value1">>, 0.5}]}),
    [{<<"flag1">>, [{<<"value1">>, 0.5}]}] = ldclient_storage_map:get(default, features, <<"flag1">>),
    ok = ldclient_storage_map:upsert_clean(default, features, #{<<"flag2">> => [{<<"value2">>, 0.9}]}),
    [{<<"flag2">>, [{<<"value2">>, 0.9}]}] = ldclient_storage_map:all(default, features).

server_all(_) ->
    [] = ldclient_storage_map:all(default, features),
    [] = ldclient_storage_map:all(another1, features),
    ok = ldclient_storage_map:upsert(default, features, #{<<"flag1">> => [{<<"value1">>, 0.5}]}),
    ok = ldclient_storage_map:upsert(default, features, #{<<"flag2">> => [{<<"value2">>, 0.7}]}),
    ok = ldclient_storage_map:upsert(another1, features, #{<<"flag1">> => [{<<"value1">>, 0.9}]}),
    ok = ldclient_storage_map:upsert(another1, features, #{<<"flag5">> => [{<<"value2">>, 0.77}]}),
    [
        {<<"flag1">>, [{<<"value1">>, 0.5}]},
        {<<"flag2">>, [{<<"value2">>, 0.7}]}
    ] = lists:sort(ldclient_storage_map:all(default, features)),
    [
        {<<"flag1">>, [{<<"value1">>, 0.9}]},
        {<<"flag5">>, [{<<"value2">>, 0.77}]}
    ] = lists:sort(ldclient_storage_map:all(another1, features)).

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
        {<<"flag-key-1">>, #{debugEventsUntilDate := null,deleted := false,
               fallthrough := #{bucketBy := key,variations := []},
               key := <<"flag-key-1">>,offVariation := 0,on := true,
               prerequisites := [],rules := [],salt := <<>>,targets := [],
               trackEvents := false,trackEventsFallthrough := false,
               variations := [],version := 0}},
        {<<"flag-key-2">>, #{debugEventsUntilDate := null,deleted := false,
               fallthrough := #{bucketBy := key,variations := []},
               key := <<"flag-key-2">>,offVariation := 0,on := true,
               prerequisites := [],rules := [],salt := <<>>,targets := [],
               trackEvents := false,trackEventsFallthrough := false,
               variations := [],version := 0}},
        {<<"flag-key-3">>, #{debugEventsUntilDate := null,deleted := false,
               fallthrough := #{bucketBy := key,variations := []},
               key := <<"flag-key-3">>,offVariation := 0,on := true,
               prerequisites := [],rules := [],salt := <<>>,targets := [],
               trackEvents := false,trackEventsFallthrough := false,
               variations := [],version := 0}}
    ] = lists:sort(ldclient_storage_map:all(default, features)),
    [
        {<<"segment-key-1">>, #{deleted := false,excluded := [],included := [],key := <<"segment-value-1">>,rules := [],
              salt := <<>>,version := 0}},
        {<<"segment-key-2">>, #{deleted := false,excluded := [],included := [],key := <<"segment-value-2">>,rules := [],
              salt := <<>>,version := 0}}
    ] = lists:sort(ldclient_storage_map:all(default, segments)).

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
        <<"data">> => #{<<"debugEventsUntilDate">> => null,<<"deleted">> => false,
               <<"fallthrough">> => #{<<"bucketBy">> => key,<<"variations">> => []},
               <<"key">> => <<"flag-key-2">>,<<"offVariation">> => 0,<<"on">> => false,
               <<"prerequisites">> => [],<<"rules">> => [],<<"salt">> => <<>>,<<"targets">> => [],
               <<"trackEvents">> => false,<<"trackEventsFallthrough">> => false,
               <<"variations">> => [],<<"version">> => 1}
    },
    ok = ldclient_update_stream_server:process_items(patch, PatchEvent, ldclient_storage_map, default),
    [
        {<<"flag-key-1">>, #{debugEventsUntilDate := null,deleted := false,
               fallthrough := #{bucketBy := key,variations := []},
               key := <<"flag-key-1">>,offVariation := 0,on := true,
               prerequisites := [],rules := [],salt := <<>>,targets := [],
               trackEvents := false,trackEventsFallthrough := false,
               variations := [],version := 0}},
        {<<"flag-key-2">>, #{debugEventsUntilDate := null,deleted := false,
               fallthrough := #{bucketBy := key,variations := []},
               key := <<"flag-key-2">>,offVariation := 0,on := false,
               prerequisites := [],rules := [],salt := <<>>,targets := [],
               trackEvents := false,trackEventsFallthrough := false,
               variations := [],version := 1}}
    ] = lists:sort(ldclient_storage_map:all(default, features)).
