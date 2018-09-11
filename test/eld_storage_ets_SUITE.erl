-module(eld_storage_ets_SUITE).

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
        server_list,
        server_process_events_put,
        server_process_events_patch
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(eld),
    eld:start_storage(eld_storage_ets),
    Config.

end_per_suite(_) ->
    ok = application:stop(eld).

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok = eld_storage_ets:empty(flags),
    ok.

%%====================================================================
%% Helpers
%%====================================================================

%%====================================================================
%% Tests
%%====================================================================

server_init(_) ->
    % Verify flags and segments buckets are pre-created
    {error, already_exists, _} = eld_storage_ets:create(flags),
    {error, already_exists, _} = eld_storage_ets:create(segments).

server_bucket_exists(_) ->
    {error, bucket_not_found, _} = eld_storage_ets:get(eld_testing, <<"testing">>),
    ok = eld_storage_ets:create(eld_testing),
    [] = eld_storage_ets:get(eld_testing, <<"testing">>).

server_get_put(_) ->
    [] = eld_storage_ets:get(flags, <<"flag1">>),
    ok = eld_storage_ets:put(flags, #{<<"flag1">> => [{<<"value1">>, 0.5}]}),
    [{<<"flag1">>, [{<<"value1">>, 0.5}]}] = eld_storage_ets:get(flags, <<"flag1">>).

server_list(_) ->
    [] = eld_storage_ets:list(flags),
    ok = eld_storage_ets:put(flags, #{<<"flag1">> => [{<<"value1">>, 0.5}]}),
    ok = eld_storage_ets:put(flags, #{<<"flag2">> => [{<<"value2">>, 0.7}]}),
    [
        {<<"flag1">>, [{<<"value1">>, 0.5}]},
        {<<"flag2">>, [{<<"value2">>, 0.7}]}
    ] = lists:sort(eld_storage_ets:list(flags)).

server_process_events_put(_) ->
    Event = #{
        <<"path">> => <<"/">>,
        <<"data">> => #{
            <<"flags">> => #{
                <<"flag-key-1">> => <<"flag-value-1">>,
                <<"flag-key-2">> => <<"flag-value-2">>,
                <<"flag-key-3">> => <<"flag-value-3">>
            },
            <<"segments">> => #{
                <<"segment-key-1">> => <<"segment-value-1">>,
                <<"segment-key-2">> => <<"segment-value-2">>
            }
        }
    },
    ok = eld_stream_server:process_items(put, Event, eld_storage_ets),
    [
        {<<"flag-key-1">>, <<"flag-value-1">>},
        {<<"flag-key-2">>, <<"flag-value-2">>},
        {<<"flag-key-3">>, <<"flag-value-3">>}
    ] = lists:sort(eld_storage_ets:list(flags)),
    [
        {<<"segment-key-1">>, <<"segment-value-1">>},
        {<<"segment-key-2">>, <<"segment-value-2">>}
    ] = lists:sort(eld_storage_ets:list(segments)).

server_process_events_patch(_) ->
    PutEvent = #{
        <<"path">> => <<"/">>,
        <<"data">> => #{
            <<"flags">> => #{
                <<"flag-key-1">> => <<"flag-value-1">>,
                <<"flag-key-2">> => <<"flag-value-2">>
            },
            <<"segments">> => #{}
        }
    },
    ok = eld_stream_server:process_items(put, PutEvent, eld_storage_ets),
    PatchEvent = #{
        <<"path">> => <<"/flags/flag-key-2">>,
        <<"data">> => <<"flag-value-2-mod">>
    },
    ok = eld_stream_server:process_items(patch, PatchEvent, eld_storage_ets),
    [
        {<<"flag-key-1">>, <<"flag-value-1">>},
        {<<"flag-key-2">>, <<"flag-value-2-mod">>}
    ] = lists:sort(eld_storage_ets:list(flags)).
