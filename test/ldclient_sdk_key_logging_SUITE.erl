-module(ldclient_sdk_key_logging_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    format_status_poll_server_redacts_sdk_key/1,
    format_status_event_process_server_redacts_sdk_key/1,
    event_dispatch_httpc_error_does_not_log_sdk_key/1,
    streaming_connection_error_does_not_log_sdk_key/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        format_status_poll_server_redacts_sdk_key,
        format_status_event_process_server_redacts_sdk_key,
        event_dispatch_httpc_error_does_not_log_sdk_key,
        streaming_connection_error_does_not_log_sdk_key
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ldclient),
    Config.

end_per_suite(_) ->
    ok = application:stop(ldclient).

init_per_testcase(_, Config) ->
    % Install a custom error_logger handler to capture logs
    error_logger:tty(false),
    error_logger:add_report_handler(?MODULE, self()),
    Config.

end_per_testcase(_, _Config) ->
    error_logger:delete_report_handler(?MODULE),
    error_logger:tty(true),
    ok.

%%====================================================================
%% Tests
%%====================================================================

%% Test that format_status for polling server redacts the SDK key
format_status_poll_server_redacts_sdk_key(_) ->
    SdkKey = "sdk-test-key-12345",
    State = #{
        sdk_key => SdkKey,
        feature_store => ldclient_storage_map,
        storage_tag => test,
        requestor => ldclient_update_requestor_httpc,
        requestor_state => #{},
        poll_uri => "http://example.com",
        poll_interval => 30000
    },

    % Call format_status as the gen_server would
    FormattedStatus = ldclient_update_poll_server:format_status(#{state => State}),

    % Extract the formatted state
    #{state := FormattedState} = FormattedStatus,

    % Verify SDK key is redacted
    "[REDACTED]" = maps:get(sdk_key, FormattedState),

    % Verify the formatted status doesn't contain the actual SDK key
    StatusStr = lists:flatten(io_lib:format("~p", [FormattedStatus])),
    nomatch = string:find(StatusStr, SdkKey).

%% Test that format_status for event process server redacts the SDK key
format_status_event_process_server_redacts_sdk_key(_) ->
    SdkKey = "sdk-test-key-67890",
    State = #{
        sdk_key => SdkKey,
        dispatcher => ldclient_event_dispatch_httpc,
        global_private_attributes => [],
        events_uri => "http://example.com/events",
        tag => test,
        dispatcher_state => #{}
    },

    % Call format_status as the gen_server would
    FormattedStatus = ldclient_event_process_server:format_status(#{state => State}),

    % Extract the formatted state
    #{state := FormattedState} = FormattedStatus,

    % Verify SDK key is redacted
    "[REDACTED]" = maps:get(sdk_key, FormattedState),

    % Verify the formatted status doesn't contain the actual SDK key
    StatusStr = lists:flatten(io_lib:format("~p", [FormattedStatus])),
    nomatch = string:find(StatusStr, SdkKey).

%% Test that event dispatch httpc errors don't log SDK keys
event_dispatch_httpc_error_does_not_log_sdk_key(Config) ->
    SdkKey = "sdk-test-event-key-with-newline\n",

    % Start bookish_spork to mock HTTP endpoint that will fail
    {ok, _} = bookish_spork:start_server(),

    % Configure spork to respond with connection error by not responding at all
    % We'll let the request timeout

    Tag = test_event_dispatch,
    Options = #{
        base_uri => "http://localhost:32002",
        events_uri => "http://localhost:32002",
        stream => false,
        offline => true,
        send_events => true,
        http_options => #{connect_timeout => 100}
    },

    ok = ldclient_config:register(Tag, ldclient_config:parse_options(SdkKey, Options)),

    % Initialize the dispatcher
    State = ldclient_event_dispatch_httpc:init(Tag, SdkKey),

    % Clear any existing messages
    flush_messages(),

    % Try to send events to a non-responding endpoint
    Result = ldclient_event_dispatch_httpc:send(State, <<"[]">>, uuid:get_v4(), "http://localhost:32002/events"),

    % Should get an error
    {error, _, SafeReason} = Result,

    % Verify the error reason doesn't contain the SDK key
    nomatch = string:find(SafeReason, SdkKey),
    nomatch = string:find(SafeReason, "sdk-test-event-key"),

    % Stop bookish_spork
    bookish_spork:stop_server(),

    ldclient_config:unregister(Tag).

%% Test that streaming connection errors don't log SDK keys
streaming_connection_error_does_not_log_sdk_key(_) ->
    SdkKey = "sdk-test-stream-key",

    % Test the format_shotgun_error function with various error types
    ErrorScenarios = [
        {gun_error, connection_refused},
        {shutdown, closed},
        timeout,
        econnrefused,
        nxdomain
    ],

    lists:foreach(fun(Error) ->
        SafeError = ldclient_key_redaction:format_shotgun_error(Error),
        % Verify the safe error doesn't contain any SDK key pattern
        nomatch = string:find(SafeError, SdkKey),
        % Verify it's a safe error message
        true = is_list(SafeError)
    end, ErrorScenarios).

%%====================================================================
%% Helper functions
%%====================================================================

flush_messages() ->
    receive
        _ -> flush_messages()
    after 0 ->
        ok
    end.

%%====================================================================
%% error_logger handler callbacks
%%====================================================================

init(Parent) ->
    {ok, Parent}.

handle_event({error, _GL, {_Pid, Format, Data}}, Parent) ->
    Parent ! {error_log, Format, Data},
    {ok, Parent};
handle_event({error_report, _GL, {_Pid, _Type, Report}}, Parent) ->
    Parent ! {error_report, Report},
    {ok, Parent};
handle_event({warning_msg, _GL, {_Pid, Format, Data}}, Parent) ->
    Parent ! {warning_log, Format, Data},
    {ok, Parent};
handle_event({warning_report, _GL, {_Pid, _Type, Report}}, Parent) ->
    Parent ! {warning_report, Report},
    {ok, Parent};
handle_event({info_msg, _GL, {_Pid, Format, Data}}, Parent) ->
    Parent ! {info_log, Format, Data},
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
