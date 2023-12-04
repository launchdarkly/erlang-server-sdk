-module(ldclient_start_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

%%====================================================================
%% ct functions
%%====================================================================
all() ->
    [
        test_already_running_client
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ldclient),
    Config.

end_per_suite(_) ->
    ok = application:stop(ldclient).

init_per_testcase(_, Config) ->
    Options = #{
        send_events => false,
        feature_store => ldclient_storage_map,
        datasource => testdata
    },
    ldclient:start_instance("", Options),
    Config.

end_per_testcase(_, _Config) ->
    ldclient:stop_all_instances().

%%====================================================================
%% Tests
%%====================================================================

test_already_running_client(_) ->
    {error, already_started, _} = ldclient:start_instance("", #{
        send_events => false,
        feature_store => ldclient_storage_map,
        datasource => testdata
    }).
