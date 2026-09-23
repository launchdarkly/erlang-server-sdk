-module(ldclient_eval_store_initialized_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    mark_warning_logged_is_true_only_for_first_call/1,
    delete_warning_states_keeps_initialized_states/1,
    variation_logs_store_data_warning_once/1,
    all_flags_state_logs_store_data_warning_once/1,
    restarted_instance_logs_store_data_warning_again/1
]).

%% error_logger handler callbacks
-export([init/1]).
-export([handle_event/2]).
-export([handle_call/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(TAG, store_initialized_test).
-define(VARIATION_WARNING,
    "Variation called before LaunchDarkly client initialization completed - using last known values from feature store. This message is logged once.").
-define(ALL_FLAGS_WARNING,
    "Called allFlagsState before client initialization; using last known values from data store. This message is logged once.").

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        mark_warning_logged_is_true_only_for_first_call,
        delete_warning_states_keeps_initialized_states,
        variation_logs_store_data_warning_once,
        all_flags_state_logs_store_data_warning_once,
        restarted_instance_logs_store_data_warning_again
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ldclient),
    Config.

end_per_suite(_) ->
    ok = application:stop(ldclient).

init_per_testcase(_, Config) ->
    error_logger:tty(false),
    error_logger:add_report_handler(?MODULE, self()),
    start_store_initialized_instance(),
    Config.

end_per_testcase(_, _Config) ->
    stop_store_initialized_instance(),
    error_logger:delete_report_handler(?MODULE),
    error_logger:tty(true),
    ok.

%%====================================================================
%% Tests
%%====================================================================

mark_warning_logged_is_true_only_for_first_call(_) ->
    true = ldclient_update_processor_state:mark_warning_logged(?TAG, some_warning),
    false = ldclient_update_processor_state:mark_warning_logged(?TAG, some_warning),
    true = ldclient_update_processor_state:mark_warning_logged(?TAG, other_warning),
    true = ldclient_update_processor_state:mark_warning_logged(other_tag, some_warning),
    true = ldclient_update_processor_state:delete_warning_states(other_tag).

delete_warning_states_keeps_initialized_states(_) ->
    true = ldclient_update_processor_state:create_initialized_state(unit_test_tag, false),
    true = ldclient_update_processor_state:mark_warning_logged(unit_test_tag, some_warning),
    true = ldclient_update_processor_state:delete_warning_states(unit_test_tag),
    true = ldclient_update_processor_state:mark_warning_logged(unit_test_tag, some_warning),
    false = ldclient_update_processor_state:get_initialized_state(unit_test_tag),
    true = ldclient_update_processor_state:delete_warning_states(unit_test_tag),
    true = ldclient_update_processor_state:delete_initialized_state(unit_test_tag).

variation_logs_store_data_warning_once(_) ->
    Context = ldclient_context:new(<<"user-key">>),
    <<"default">> = ldclient:variation(<<"flag-a">>, Context, <<"default">>, ?TAG),
    <<"default">> = ldclient:variation(<<"flag-b">>, Context, <<"default">>, ?TAG),
    2 = meck:num_calls(ldclient_storage_redis, get, '_'),
    1 = count_warnings(?VARIATION_WARNING).

all_flags_state_logs_store_data_warning_once(_) ->
    Context = ldclient_context:new(<<"user-key">>),
    Options = #{with_reasons => false, client_side_only => false},
    #{flag_values := #{}} = ldclient:all_flags_state(Context, ?TAG),
    #{<<"$valid">> := true} = ldclient:all_flags_state(Context, Options, ?TAG),
    #{flag_values := #{}} = ldclient:all_flags_state(Context, ?TAG),
    3 = meck:num_calls(ldclient_storage_redis, all, '_'),
    1 = count_warnings(?ALL_FLAGS_WARNING).

restarted_instance_logs_store_data_warning_again(_) ->
    Context = ldclient_context:new(<<"user-key">>),
    <<"default">> = ldclient:variation(<<"flag-a">>, Context, <<"default">>, ?TAG),
    1 = count_warnings(?VARIATION_WARNING),
    stop_store_initialized_instance(),
    start_store_initialized_instance(),
    <<"default">> = ldclient:variation(<<"flag-a">>, Context, <<"default">>, ?TAG),
    1 = count_warnings(?VARIATION_WARNING).

%%====================================================================
%% Helpers
%%====================================================================

%% Starts an instance, then makes the evaluation code see a client that is not initialized
%% with an initialized redis store. No redis server is needed.
start_store_initialized_instance() ->
    Options = #{
        stream => false,
        polling_update_requestor => ldclient_update_requestor_test
    },
    ok = ldclient:start_instance("", ?TAG, Options),
    meck:new(ldclient_instance, [passthrough]),
    meck:expect(ldclient_instance, update_processor_initialized, fun
        (?TAG) -> false;
        (Tag) -> meck:passthrough([Tag])
    end),
    meck:expect(ldclient_instance, feature_store_initialized, fun
        (?TAG) -> true;
        (Tag) -> meck:passthrough([Tag])
    end),
    meck:new(ldclient_config, [passthrough]),
    meck:expect(ldclient_config, get_value, fun
        (?TAG, feature_store) -> ldclient_storage_redis;
        (Tag, Key) -> meck:passthrough([Tag, Key])
    end),
    meck:new(ldclient_storage_redis, []),
    meck:expect(ldclient_storage_redis, get, fun(_Tag, _Bucket, _Key) -> [] end),
    meck:expect(ldclient_storage_redis, all, fun(_Tag, _Bucket) -> [] end),
    ok.

stop_store_initialized_instance() ->
    meck:unload(ldclient_storage_redis),
    meck:unload(ldclient_config),
    meck:unload(ldclient_instance),
    ok = ldclient:stop_instance(?TAG).

%% Returns how many times the handler captured a warning with the given format.
count_warnings(Format) ->
    length([F || F <- collect_warnings([]), F =:= Format]).

collect_warnings(Acc) ->
    receive
        {warning_log, Format, _Data} -> collect_warnings([Format | Acc])
    after 500 ->
        lists:reverse(Acc)
    end.

%%====================================================================
%% error_logger handler callbacks
%%====================================================================

init(Parent) ->
    {ok, Parent}.

handle_event({warning_msg, _GL, {_Pid, Format, Data}}, Parent) ->
    Parent ! {warning_log, Format, Data},
    {ok, Parent};
handle_event(_Event, Parent) ->
    {ok, Parent}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
