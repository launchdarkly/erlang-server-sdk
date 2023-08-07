-module(ldclient_storage_cache_SUITE).

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
    server_get_cache_timeout_upsert/1,
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
        feature_store => ldclient_storage_redis,
        stream => false,
        polling_update_requestor => ldclient_update_requestor_test,
        redis_prefix => "default",
        redis_host => os:getenv("REDIS_HOST", "127.0.0.1")
    },
    ldclient:start_instance("", Options),
    AnotherOptions = #{
        feature_store => ldclient_storage_redis,
        stream => false,
        polling_update_requestor => ldclient_update_requestor_test,
        redis_prefix => "another1",
        redis_host => os:getenv("REDIS_HOST", "127.0.0.1")
    },
    ldclient:start_instance("", another1, AnotherOptions),
    TimeoutOptions = #{
        feature_store => ldclient_storage_redis,
        stream => false,
        polling_update_requestor => ldclient_update_requestor_test,
        redis_prefix => "timeout",
        cache_ttl => 3,
        redis_host => os:getenv("REDIS_HOST", "127.0.0.1")
    },
    ldclient:start_instance("", timeout, TimeoutOptions),
    Config.

end_per_suite(_) ->
    ok = application:stop(ldclient).

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok = ldclient_storage_redis:empty(default, features),
    ok = ldclient_storage_redis:empty(another1, features),
    ok.

%%====================================================================
%% Helpers
%%====================================================================

%%====================================================================
%% Tests
%%====================================================================

server_init(_) ->
    % Verify flags and segments buckets are pre-created
    {error, already_exists, _} = ldclient_storage_redis:create(default, features),
    {error, already_exists, _} = ldclient_storage_redis:create(default, segments),
    {error, already_exists, _} = ldclient_storage_redis:create(another1, features),
    {error, already_exists, _} = ldclient_storage_redis:create(another1, segments).

server_bucket_exists(_) ->
    {error, bucket_not_found, _} = ldclient_storage_redis:get(default, ldclient_testing, <<"testing">>),
    ok = ldclient_storage_redis:create(default, ldclient_testing),
    [] = ldclient_storage_redis:get(default, ldclient_testing, <<"testing">>),
    {error, bucket_not_found, _} = ldclient_storage_redis:get(another1, ldclient_testing, <<"testing">>).

server_get_upsert(_) ->
    [] = ldclient_storage_redis:get(default, features, <<"flag1">>),
    ok = ldclient_storage_redis:upsert(default, features, #{<<"flag1">> => #{debugEventsUntilDate => null,deleted => false,
               fallthrough => #{bucketBy => key,variations => []},
               key => <<"flag1">>,offVariation => 0,on => true,
               prerequisites => [],rules => [],salt => <<>>,targets => [],
               trackEvents => false,trackEventsFallthrough => false,
               variations => [],version => 0}}),
    [] = ldclient_storage_redis:get(another1, features, <<"flag1">>),
    [{<<"flag1">>, #{debugEventsUntilDate := null,deleted := false,
               fallthrough := #{bucketBy := #{binary := <<"key">>,
                   components := [<<"key">>],
                   valid := true},variations := []},
               key := <<"flag1">>,offVariation := 0,on := true,
               prerequisites := [],rules := [],salt := <<>>,targets := [],
               trackEvents := false,trackEventsFallthrough := false,
               variations := [],version := 0}}] = ldclient_storage_redis:get(default, features, <<"flag1">>),
    [] = ldclient_storage_redis:get(another1, features, <<"flag1">>),
    ok = ldclient_storage_redis:upsert(another1, features, #{<<"flag1">> => #{debugEventsUntilDate => null,deleted => false,
               fallthrough => #{bucketBy => key,variations => []},
               key => <<"flag1">>,offVariation => 0,on => false,
               prerequisites => [],rules => [],salt => <<>>,targets => [],
               trackEvents => false,trackEventsFallthrough => false,
               variations => [],version => 1}}),
    [{<<"flag1">>, #{debugEventsUntilDate := null,deleted := false,
               fallthrough := #{bucketBy := #{binary := <<"key">>,
                   components := [<<"key">>],
                   valid := true},variations := []},
               key := <<"flag1">>,offVariation := 0,on := false,
               prerequisites := [],rules := [],salt := <<>>,targets := [],
               trackEvents := false,trackEventsFallthrough := false,
               variations := [],version := 1}}] = ldclient_storage_redis:get(another1, features, <<"flag1">>),
    [{<<"flag1">>, #{debugEventsUntilDate := null,deleted := false,
               fallthrough := #{bucketBy := #{binary := <<"key">>,
                   components := [<<"key">>],
                   valid := true},variations := []},
               key := <<"flag1">>,offVariation := 0,on := true,
               prerequisites := [],rules := [],salt := <<>>,targets := [],
               trackEvents := false,trackEventsFallthrough := false,
               variations := [],version := 0}}] = ldclient_storage_redis:get(default, features, <<"flag1">>).

server_get_cache_timeout_upsert(_) ->
    [] = ldclient_storage_redis:get(timeout, features, <<"flag1">>),
    ok = ldclient_storage_redis:upsert(timeout, features, #{<<"flag1">> => #{debugEventsUntilDate => null,deleted => false,
               fallthrough => #{bucketBy => key,variations => []},
               key => <<"flag1">>,offVariation => 0,on => true,
               prerequisites => [],rules => [],salt => <<>>,targets => [],
               trackEvents => false,trackEventsFallthrough => false,
               variations => [],version => 0}}),
    [] = ldclient_storage_redis:get(another1, features, <<"flag1">>),
    [_, true] = ldclient_storage_cache_server:get(ldclient_storage_cache_server_timeout, features, <<"flag1">>),
    timer:sleep(3100),
    [[{}], false] = ldclient_storage_cache_server:get(ldclient_storage_cache_server_timeout, features, <<"flag1">>),
    [] = ldclient_storage_redis:get(another1, features, <<"flag1">>).

server_upsert_clean(_) ->
    ok = ldclient_storage_redis:upsert(default, features, #{<<"flag1">> => #{debugEventsUntilDate => null,deleted => false,
               fallthrough => #{bucketBy => key,variations => []},
               key => <<"flag1">>,offVariation => 0,on => true,
               prerequisites => [],rules => [],salt => <<>>,targets => [],
               trackEvents => false,trackEventsFallthrough => false,
               variations => [],version => 0}}),
    [{<<"flag1">>, #{debugEventsUntilDate := null,deleted := false,
               fallthrough := #{bucketBy := #{binary := <<"key">>,
                   components := [<<"key">>],
                   valid := true},variations := []},
               key := <<"flag1">>,offVariation := 0,on := true,
               prerequisites := [],rules := [],salt := <<>>,targets := [],
               trackEvents := false,trackEventsFallthrough := false,
               variations := [],version := 0}}] = ldclient_storage_redis:get(default, features, <<"flag1">>),
    ok = ldclient_storage_redis:upsert_clean(default, features, #{<<"flag2">> => #{debugEventsUntilDate => null,deleted => false,
               fallthrough => #{bucketBy => key,variations => []},
               key => <<"flag2">>,offVariation => 0,on => false,
               prerequisites => [],rules => [],salt => <<>>,targets => [],
               trackEvents => false,trackEventsFallthrough => false,
               variations => [],version => 1}}),
    [{<<"flag2">>, #{debugEventsUntilDate := null,deleted := false,
               fallthrough := #{bucketBy := #{binary := <<"key">>,
                   components := [<<"key">>],
                   valid := true},variations := []},
               key := <<"flag2">>,offVariation := 0,on := false,
               prerequisites := [],rules := [],salt := <<>>,targets := [],
               trackEvents := false,trackEventsFallthrough := false,
               variations := [],version := 1}}] = ldclient_storage_redis:all(default, features).

server_all(_) ->
    [] = ldclient_storage_redis:all(default, features),
    [] = ldclient_storage_redis:all(another1, features),
    ok = ldclient_storage_redis:upsert(default, features, #{<<"flag1">> => #{debugEventsUntilDate => null,deleted => false,
               fallthrough => #{bucketBy => key,variations => []},
               key => <<"flag1">>,offVariation => 0,on => true,
               prerequisites => [],rules => [],salt => <<>>,targets => [],
               trackEvents => false,trackEventsFallthrough => false,
               variations => [],version => 0}}),
    ok = ldclient_storage_redis:upsert(default, features, #{<<"flag2">> => #{debugEventsUntilDate => null,deleted => false,
               fallthrough => #{bucketBy => key,variations => []},
               key => <<"flag2">>,offVariation => 0,on => true,
               prerequisites => [],rules => [],salt => <<>>,targets => [],
               trackEvents => false,trackEventsFallthrough => false,
               variations => [],version => 0}}),
    ok = ldclient_storage_redis:upsert(another1, features, #{<<"flag1">> => #{debugEventsUntilDate => null,deleted => false,
               fallthrough => #{bucketBy => key,variations => []},
               key => <<"flag1">>,offVariation => 0,on => false,
               prerequisites => [],rules => [],salt => <<>>,targets => [],
               trackEvents => false,trackEventsFallthrough => false,
               variations => [],version => 1}}),
    ok = ldclient_storage_redis:upsert(another1, features, #{<<"flag5">> => #{debugEventsUntilDate => null,deleted => false,
               fallthrough => #{bucketBy => key,variations => []},
               key => <<"flag5">>,offVariation => 0,on => true,
               prerequisites => [],rules => [],salt => <<"launchdarkly">>,targets => [],
               trackEvents => false,trackEventsFallthrough => false,
               variations => [],version => 0}}),
    [
        {<<"flag1">>, #{debugEventsUntilDate := null,deleted := false,
               fallthrough := #{bucketBy := #{binary := <<"key">>,
                   components := [<<"key">>],
                   valid := true},variations := []},
               key := <<"flag1">>,offVariation := 0,on := true,
               prerequisites := [],rules := [],salt := <<>>,targets := [],
               trackEvents := false,trackEventsFallthrough := false,
               variations := [],version := 0}},
        {<<"flag2">>, #{debugEventsUntilDate := null,deleted := false,
               fallthrough := #{bucketBy := #{binary := <<"key">>,
                   components := [<<"key">>],
                   valid := true},variations := []},
               key := <<"flag2">>,offVariation := 0,on := true,
               prerequisites := [],rules := [],salt := <<>>,targets := [],
               trackEvents := false,trackEventsFallthrough := false,
               variations := [],version := 0}}
    ] = lists:sort(ldclient_storage_redis:all(default, features)),
    [
        {<<"flag1">>, #{debugEventsUntilDate := null,deleted := false,
               fallthrough := #{bucketBy := #{binary := <<"key">>,
                   components := [<<"key">>],
                   valid := true},variations := []},
               key := <<"flag1">>,offVariation := 0,on := false,
               prerequisites := [],rules := [],salt := <<>>,targets := [],
               trackEvents := false,trackEventsFallthrough := false,
               variations := [],version := 1}},
        {<<"flag5">>, #{debugEventsUntilDate := null,deleted := false,
               fallthrough := #{bucketBy := #{binary := <<"key">>,
                   components := [<<"key">>],
                   valid := true},variations := []},
               key := <<"flag5">>,offVariation := 0,on := true,
               prerequisites := [],rules := [],salt := <<"launchdarkly">>,targets := [],
               trackEvents := false,trackEventsFallthrough := false,
               variations := [],version := 0}}
    ] = lists:sort(ldclient_storage_redis:all(another1, features)).

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
    ok = ldclient_update_stream_server:process_items(put, Event, ldclient_storage_redis, default),
    [
        {<<"flag-key-1">>, #{debugEventsUntilDate := null,deleted := false,
               fallthrough := #{bucketBy := #{binary := <<"key">>,
                   components := [<<"key">>],
                   valid := true},variations := []},
               key := <<"flag-key-1">>,offVariation := 0,on := true,
               prerequisites := [],rules := [],salt := <<>>,targets := [],
               trackEvents := false,trackEventsFallthrough := false,
               variations := [],version := 0}},
        {<<"flag-key-2">>, #{debugEventsUntilDate := null,deleted := false,
               fallthrough := #{bucketBy := #{binary := <<"key">>,
                   components := [<<"key">>],
                   valid := true},variations := []},
               key := <<"flag-key-2">>,offVariation := 0,on := true,
               prerequisites := [],rules := [],salt := <<>>,targets := [],
               trackEvents := false,trackEventsFallthrough := false,
               variations := [],version := 0}},
        {<<"flag-key-3">>, #{debugEventsUntilDate := null,deleted := false,
               fallthrough := #{bucketBy := #{binary := <<"key">>,
                   components := [<<"key">>],
                   valid := true},variations := []},
               key := <<"flag-key-3">>,offVariation := 0,on := true,
               prerequisites := [],rules := [],salt := <<>>,targets := [],
               trackEvents := false,trackEventsFallthrough := false,
               variations := [],version := 0}}
    ] = lists:sort(ldclient_storage_redis:all(default, features)),
    [
        {<<"segment-key-1">>, #{deleted := false,excluded := [],included := [],key := <<"segment-value-1">>,rules := [],
              salt := <<>>,version := 0}},
        {<<"segment-key-2">>, #{deleted := false,excluded := [],included := [],key := <<"segment-value-2">>,rules := [],
              salt := <<>>,version := 0}}
    ] = lists:sort(ldclient_storage_redis:all(default, segments)).

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
    ok = ldclient_update_stream_server:process_items(put, PutEvent, ldclient_storage_redis, default),
    PatchEvent = #{
        <<"path">> => <<"/flags/flag-key-2">>,
        <<"data">> => #{<<"debugEventsUntilDate">> => null,<<"deleted">> => false,
               <<"fallthrough">> => #{<<"bucketBy">> => key,<<"variations">> => []},
               <<"key">> => <<"flag-key-2">>,<<"offVariation">> => 0,<<"on">> => false,
               <<"prerequisites">> => [],<<"rules">> => [],<<"salt">> => <<>>,<<"targets">> => [],
               <<"trackEvents">> => false,<<"trackEventsFallthrough">> => false,
               <<"variations">> => [],<<"version">> => 1}
    },
    ok = ldclient_update_stream_server:process_items(patch, PatchEvent, ldclient_storage_redis, default),
    [
        {<<"flag-key-1">>, #{debugEventsUntilDate := null,deleted := false,
               fallthrough := #{bucketBy := #{binary := <<"key">>,
                   components := [<<"key">>],
                   valid := true},variations := []},
               key := <<"flag-key-1">>,offVariation := 0,on := true,
               prerequisites := [],rules := [],salt := <<>>,targets := [],
               trackEvents := false,trackEventsFallthrough := false,
               variations := [],version := 0}},
        {<<"flag-key-2">>, #{debugEventsUntilDate := null,deleted := false,
               fallthrough := #{bucketBy := #{binary := <<"key">>,
                   components := [<<"key">>],
                   valid := true},variations := []},
               key := <<"flag-key-2">>,offVariation := 0,on := false,
               prerequisites := [],rules := [],salt := <<>>,targets := [],
               trackEvents := false,trackEventsFallthrough := false,
               variations := [],version := 1}}
    ] = lists:sort(ldclient_storage_redis:all(default, features)).
