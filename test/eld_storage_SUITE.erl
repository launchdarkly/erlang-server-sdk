-module(eld_storage_SUITE).

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
    server_process_events/1
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
        server_process_events
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(eld),
    Config.

end_per_suite(_) ->
    ok = application:stop(eld).

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok = eld_storage_server:empty(flags),
    ok.

%%====================================================================
%% Helpers
%%====================================================================

%%====================================================================
%% Tests
%%====================================================================

server_init(_) ->
    % Verify flags and segments buckets are pre-created
    {error, already_exists, _} = eld_storage_server:create(flags),
    {error, already_exists, _} = eld_storage_server:create(segments).

server_bucket_exists(_) ->
    {error, bucket_not_found, _} = eld_storage_server:get(eld_testing, <<"testing">>),
    ok = eld_storage_server:create(eld_testing),
    [] = eld_storage_server:get(eld_testing, <<"testing">>).

server_get_put(_) ->
    [] = eld_storage_server:get(flags, <<"flag1">>),
    ok = eld_storage_server:put(flags, <<"flag1">>, [{<<"value1">>, 0.5}]),
    [{<<"flag1">>, [{<<"value1">>, 0.5}]}] = eld_storage_server:get(flags, <<"flag1">>).

server_list(_) ->
    [] = eld_storage_server:list(flags),
    ok = eld_storage_server:put(flags, <<"flag1">>, [{<<"value1">>, 0.5}]),
    ok = eld_storage_server:put(flags, <<"flag2">>, [{<<"value2">>, 0.7}]),
    [
        {<<"flag1">>, [{<<"value1">>, 0.5}]},
        {<<"flag2">>, [{<<"value2">>, 0.7}]}
    ] = lists:sort(eld_storage_server:list(flags)).

server_process_events(_) ->
    % TODO
    ok.
