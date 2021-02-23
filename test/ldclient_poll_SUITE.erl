-module(ldclient_poll_SUITE).

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
    response_error_network/1,
    response_flags_segments/1,
    successful_response_override_existing_flags/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        response_error_unauthorized,
        response_error_not_found,
        response_error_internal,
        response_error_network,
        response_flags_segments,
        successful_response_override_existing_flags
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ldclient),
    Config.

end_per_suite(_) ->
    ok = application:stop(ldclient).

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
        polling_update_requestor => ldclient_update_requestor_test
    }.

set_storage_simple() ->
    {FlagKey, _, FlagMap} = ldclient_update_requestor_test:get_simple_flag(),
    {SegmentKey, _, SegmentMap} = ldclient_update_requestor_test:get_simple_segment(),
    ldclient_storage_ets:upsert_clean(default, features, #{FlagKey => FlagMap}),
    ldclient_storage_ets:upsert_clean(default, segments, #{SegmentKey => SegmentMap}).

trigger_poll() ->
    [{_,Pid,_,_}] = supervisor:which_children(ldclient_instance_stream_default),
    Pid ! {poll},
    timer:sleep(500).

check_storage_simple() ->
    {FlagKey, _, FlagMap} = ldclient_update_requestor_test:get_simple_flag(),
    {SegmentKey, _, SegmentMap} = ldclient_update_requestor_test:get_simple_segment(),
    [{FlagKey, FlagMap}] = ldclient_storage_ets:all(default, features),
    [{SegmentKey, SegmentMap}] = ldclient_storage_ets:all(default, segments).

check_storage_simple_parsed() ->
    {FlagKey, _, FlagMap} = ldclient_update_requestor_test:get_simple_flag(),
    {SegmentKey, _, SegmentMap} = ldclient_update_requestor_test:get_simple_segment(),
    ParsedFlagMap = ldclient_flag:new(FlagMap),
    ParsedSegmentMap = ldclient_segment:new(SegmentMap),
    [{FlagKey, ParsedFlagMap}] = ldclient_storage_ets:all(default, features),
    [{SegmentKey, ParsedSegmentMap}] = ldclient_storage_ets:all(default, segments).

check_storage_empty() ->
    [] = ldclient_storage_ets:all(default, features),
    [] = ldclient_storage_ets:all(default, segments).

%%====================================================================
%% Tests
%%====================================================================

response_error_unauthorized(_) ->
    ok = ldclient:start_instance("sdk-key-unauthorized", instance_options()),
    timer:sleep(500),
    set_storage_simple(),
    trigger_poll(),
    check_storage_simple(),
    ok = ldclient:stop_instance().

response_error_not_found(_) ->
    ok = ldclient:start_instance("sdk-key-not-found", instance_options()),
    timer:sleep(500),
    set_storage_simple(),
    trigger_poll(),
    check_storage_simple(),
    ok = ldclient:stop_instance().

response_error_internal(_) ->
    ok = ldclient:start_instance("sdk-key-internal-error", instance_options()),
    timer:sleep(500),
    set_storage_simple(),
    trigger_poll(),
    check_storage_simple(),
    ok = ldclient:stop_instance().

response_error_network(_) ->
    ok = ldclient:start_instance("sdk-key-network-error", instance_options()),
    timer:sleep(500),
    set_storage_simple(),
    trigger_poll(),
    check_storage_simple(),
    ok = ldclient:stop_instance().

response_flags_segments(_) ->
    ok = ldclient:start_instance("sdk-key-flags-segments", instance_options()),
    timer:sleep(500),
    check_storage_simple_parsed(),
    ok = ldclient:stop_instance().

successful_response_override_existing_flags(_) ->
    ok = ldclient:start_instance("sdk-key-empty-payload", instance_options()),
    timer:sleep(500),
    set_storage_simple(),
    trigger_poll(),
    check_storage_empty(),
    ok = ldclient:stop_instance().
