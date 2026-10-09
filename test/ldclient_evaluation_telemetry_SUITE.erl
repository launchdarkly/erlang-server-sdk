-module(ldclient_evaluation_telemetry_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    variation_emits_stop_event/1,
    variation_detail_emits_stop_event/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        variation_emits_stop_event,
        variation_detail_emits_stop_event
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
    ok = ldclient:start_instance("", Options),
    Config.

end_per_testcase(_, _Config) ->
    ldclient:stop_all_instances().

%%====================================================================
%% Tests
%%====================================================================

variation_emits_stop_event(_) ->
    {ok, Flag} = ldclient_testdata:flag(<<"flag1">>),
    _ = ldclient_testdata:update(ldclient_flagbuilder:on(true, Flag)),
    HandlerId = attach(),
    try
        _ = ldclient:variation(<<"flag1">>, ldclient_user:new(<<"user">>), false),
        assert_stop_event(<<"flag1">>)
    after
        telemetry:detach(HandlerId)
    end.

variation_detail_emits_stop_event(_) ->
    {ok, Flag} = ldclient_testdata:flag(<<"flag2">>),
    _ = ldclient_testdata:update(ldclient_flagbuilder:on(true, Flag)),
    HandlerId = attach(),
    try
        _ = ldclient:variation_detail(<<"flag2">>, ldclient_user:new(<<"user">>), false),
        assert_stop_event(<<"flag2">>)
    after
        telemetry:detach(HandlerId)
    end.

%%====================================================================
%% Helpers
%%====================================================================

attach() ->
    Self = self(),
    HandlerId = {?MODULE, self()},
    ok = telemetry:attach(
        HandlerId,
        [ldclient, evaluation, stop],
        fun(_Event, Measurements, Metadata, _Config) ->
            Self ! {evaluation, Measurements, Metadata}
        end,
        undefined
    ),
    HandlerId.

assert_stop_event(FlagKey) ->
    receive
        {evaluation, Measurements, Metadata} ->
            true = is_integer(maps:get(duration, Measurements)),
            #{tag := default, flag_key := FlagKey} = Metadata,
            true = maps:is_key(variation, Metadata)
    after 1000 ->
        ct:fail("Expected an [ldclient, evaluation, stop] telemetry event")
    end.
