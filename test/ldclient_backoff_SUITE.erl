-module(ldclient_backoff_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    delay_starts_at_initial/1,
    delay_doubles_consecutive_failures/1,
    backoff_respects_max/1,
    jitters_backoff_value/1,
    resets_after_60_second_connection/1,
    does_not_reset_after_less_than_60_connection/1,
    creates_timer_when_fired/1,
    handles_max_exponent_correctly/1,
    handles_initial_greater_than_max/1,
    handles_initial_equal_to_max/1,
    handles_bad_initial_retry/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        delay_starts_at_initial,
        delay_doubles_consecutive_failures,
        backoff_respects_max,
        jitters_backoff_value,
        resets_after_60_second_connection,
        does_not_reset_after_less_than_60_connection,
        creates_timer_when_fired,
        handles_max_exponent_correctly,
        handles_initial_greater_than_max,
        handles_initial_equal_to_max,
        handles_bad_initial_retry
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

backoff_client(InitialDelay, Ratio) ->
    meck:new(rand, [unstick]),
    meck:expect(rand, uniform, fun() -> Ratio end),
    ldclient_backoff:init(InitialDelay, 30000, 0, listen).

backoff_client(InitialDelay) ->
    backoff_client(InitialDelay, 0).

backoff_client() ->
    backoff_client(1000, 0).

jitter_client(Ratio) ->
    backoff_client(1000, Ratio).

%%====================================================================
%% Tests
%%====================================================================

%% Tests which do not use the PID set it to 0.

delay_starts_at_initial(_) ->
    %% Rand of 0 is equal to no jitter.
    Backoff = backoff_client(),
    UpdatedBackoff = ldclient_backoff:fail(Backoff),
    #{current := 1000} = UpdatedBackoff.

delay_doubles_consecutive_failures(_) ->
    Backoff = backoff_client(),
    %% This is the full cycle for the defaults of initial 1000 and max 30000.
    FirstUpdate = ldclient_backoff:fail(Backoff),
    #{current := 1000} = FirstUpdate,
    SecondUpdate = ldclient_backoff:fail(FirstUpdate),
    #{current := 2000} = SecondUpdate,
    ThirdUpdate = ldclient_backoff:fail(SecondUpdate),
    #{current := 4000} = ThirdUpdate,
    FourthUpdate = ldclient_backoff:fail(ThirdUpdate),
    #{current := 8000} = FourthUpdate,
    FifthUpdate = ldclient_backoff:fail(FourthUpdate),
    #{current := 16000} = FifthUpdate,
    SixthUpdate = ldclient_backoff:fail(FifthUpdate),
    #{current := 30000} = SixthUpdate.
    

backoff_respects_max(_) ->
    Backoff =
        ldclient_backoff:fail(
            ldclient_backoff:fail(backoff_client(20000))),
    #{current := 30000} = Backoff.

jitters_backoff_value(_) ->
    %% Set the rand function to return 0.5 which will result in 1000 - (0.5 * 0.5 * 1000).
    Backoff =
        ldclient_backoff:fail(jitter_client(0.5)),
    #{current := 750} = Backoff.

resets_after_60_second_connection(_) ->
    meck:new(ldclient_time, [passthrough]),
    meck:expect(ldclient_time, time_seconds, fun() -> 0 end),
    Backoff = ldclient_backoff:fail(ldclient_backoff:fail(backoff_client())),
    %% After 2 failures we are at 2 seconds.
    #{current := 2000} = Backoff,
    BackoffAfterSuccess = ldclient_backoff:succeed(Backoff),
    #{current := 2000, active_since := 0} = BackoffAfterSuccess,
    meck:expect(ldclient_time, time_seconds, fun() -> 61 end),
    BackoffAfterFail = ldclient_backoff:fail(BackoffAfterSuccess),
    #{current := 1000} = BackoffAfterFail.

does_not_reset_after_less_than_60_connection(_) ->
    meck:new(ldclient_time, [passthrough]),
    meck:expect(ldclient_time, time_seconds, fun() -> 0 end),
    Backoff = ldclient_backoff:fail(ldclient_backoff:fail(backoff_client())),
    %% After 2 failures we are at 2 seconds.
    #{current := 2000} = Backoff,
    BackoffAfterSuccess = ldclient_backoff:succeed(Backoff),
    #{current := 2000, active_since := 0} = BackoffAfterSuccess,
    meck:expect(ldclient_time, time_seconds, fun() -> 50 end),
    BackoffAfterFail = ldclient_backoff:fail(BackoffAfterSuccess),
    %% The connection was less than 60 seconds, so we continue to
    %% increase the delay.
    #{current := 4000} = BackoffAfterFail.

creates_timer_when_fired(_) ->
    meck:new(ldclient_time,  [passthrough]),
    meck:expect(ldclient_time, start_timer, fun(1000, 0, listen) -> make_ref() end),
    Backoff = ldclient_backoff:fail(backoff_client()),
    ldclient_backoff:fire(Backoff),
    true = meck:validate(ldclient_time).

handles_max_exponent_correctly(_) ->
    Backoff = backoff_client(),
    %% 2^4 should be the step before we hit the max
    16000 = ldclient_backoff:delay(5, Backoff),
    %% 2^5 and higher should just use the max.
    30000 = ldclient_backoff:delay(6, Backoff),
    %% trunc(1000 * :math.pow(2, 1015)) would have an arithmetic error
    30000 = ldclient_backoff:delay(1016, Backoff).

handles_initial_equal_to_max(_) ->
    Backoff = backoff_client(30000),
    30000 = ldclient_backoff:delay(1, Backoff),
    30000 = ldclient_backoff:delay(2, Backoff).

handles_initial_greater_than_max(_) ->
    Backoff = backoff_client(60000),
    30000 = ldclient_backoff:delay(1, Backoff),
    30000 = ldclient_backoff:delay(2, Backoff).

handles_bad_initial_retry(_) -> 
    Backoff = backoff_client(0),
    1 = ldclient_backoff:delay(1, Backoff),
    2 = ldclient_backoff:delay(2, Backoff),
    4 = ldclient_backoff:delay(3, Backoff),
    16384 = ldclient_backoff:delay(15, Backoff),
    30000 = ldclient_backoff:delay(16, Backoff),
    %% Second backoff we do not use backoff_client as meck is already setup.
    Backoff2 = ldclient_backoff:init(-100, 30000, 0, listen),
    1 = ldclient_backoff:delay(1, Backoff2),
    2 = ldclient_backoff:delay(2, Backoff2),
    4 = ldclient_backoff:delay(3, Backoff2),
    16384 = ldclient_backoff:delay(15, Backoff2),
    30000 = ldclient_backoff:delay(16, Backoff2).
