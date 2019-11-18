-module(eld_poll_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    response_error_unauthorized/1,
    response_error_not_found/1,
    response_error_internal/1,
    response_flags_segments/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        response_error_unauthorized,
        response_error_not_found,
        response_error_internal,
        response_flags_segments
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(eld),
    Config.

end_per_suite(_) ->
    ok = application:stop(eld).

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%%====================================================================
%% Helpers
%%====================================================================

instance_options() ->
    #{
        stream => false,
        polling_update_requestor => eld_update_requestor_test
    }.

%%====================================================================
%% Tests
%%====================================================================

response_error_unauthorized(_) ->
    ok = eld:start_instance("sdk-key-unauthorized", instance_options()),
    [] = eld_storage_ets:list(default, flags),
    ok = eld:stop_instance().

response_error_not_found(_) ->
    ok = eld:start_instance("sdk-key-not-found", instance_options()),
    [] = eld_storage_ets:list(default, flags),
    ok = eld:stop_instance().

response_error_internal(_) ->
    ok = eld:start_instance("sdk-key-internal-error", instance_options()),
    [] = eld_storage_ets:list(default, flags),
    ok = eld:stop_instance().

response_flags_segments(_) ->
    ok = eld:start_instance("sdk-key-flags-segments", instance_options()),
    {FlagKey, _, FlagMap} = eld_update_requestor_test:get_simple_flag(),
    {SegmentKey, _, SegmentMap} = eld_update_requestor_test:get_simple_segment(),
    [{FlagKey, FlagMap}] = eld_storage_ets:list(default, flags),
    [{SegmentKey, SegmentMap}] = eld_storage_ets:list(default, segments),
    ok = eld:stop_instance().
