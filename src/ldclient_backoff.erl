%%-------------------------------------------------------------------
%% @doc
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_backoff).

%% API
-export([init/4, init/5, fail/1, succeed/1, fire/1]).

%% Types

-type backoff() :: #{
    initial => non_neg_integer(),
    current => non_neg_integer(),
    max => non_neg_integer(),
    attempt => non_neg_integer(),
    active_since => integer() | undefined,
    destination => pid(),
    value => term(),
    max_exp => float(),
    uniform => fun(() -> float())
}.

-define(JITTER_RATIO, 0.5).

%% Reset interval in seconds.
-define(RESET_INTERVAL, 60).

-export_type([backoff/0]).

%% Expose non-exported methods for tests.
-ifdef(TEST).
-compile(export_all).
-endif.

-spec init(Initial :: non_neg_integer(), Max :: non_neg_integer(), Destination :: pid(), Value :: term()) -> backoff().
init(Initial, Max, Destination, Value) ->
    init(Initial, Max, Destination, Value, fun() -> rand:uniform() end).

%% This version of the function exists for testing and allows injecting the random number source.
-spec init(Initial :: non_neg_integer(), Max :: non_neg_integer(), Destination :: pid(), Value :: term(), Uniform :: fun(() -> float())) -> backoff().
init(Initial, Max, Destination, Value, Uniform) ->
    SafeInitial = lists:max([Initial, 1]),
    #{
        initial => SafeInitial, %% Do not allow initial delay to be 0 or negative.
        current => SafeInitial,
        max => Max,
        attempt => 0,
        active_since => undefined,
        destination => Destination,
        value => Value,
        %% The exponent at which the backoff delay will exceed the maximum.
        %% Beyond this limit the backoff can be set to the max.
        max_exp => math:ceil(math:log2(Max/SafeInitial)),
        %% For reasonable values this should ensure we never overflow.
        %% Note that while integers can be arbitrarily large the math library uses C functions
        %% that are implemented with floats.
        %% Allow for alternate random number source.
        uniform => Uniform
    }.

%% @doc Get an updated backoff with updated delay. Does not start a timer automatically.
%% Use fire/1 to start a timer with the updated backoff.
%% @end
-spec fail(Backoff :: backoff()) -> backoff().
fail(#{active_since := undefined} = Backoff) ->
    %% ActiveSince is undefined, so we have not had a successful connection since the last attempt.
    update_backoff(Backoff, undefined);
fail(#{active_since := ActiveSince} = Backoff) ->
    ActiveDuration = ldclient_time:time_seconds() - ActiveSince,
    update_backoff(Backoff, ActiveDuration).

%% @doc Get an updated backoff with information about the successful connection attempt.
%% Does not clear any timers. Use the timer handle from fire/1 to cancel pending timers.
%% @end
-spec succeed(Backoff :: backoff()) -> backoff().
succeed(Backoff) ->
    Backoff#{active_since => ldclient_time:time_seconds()}.

%% @doc Start a timer with the current delay.
%% @end
-spec fire(backoff()) -> Timer :: reference().
fire(#{current := Current, destination := Destination, value := Value}) ->
    ldclient_time:start_timer(Current, Destination, Value).

%%===================================================================
%% Internal functions
%%===================================================================

-spec update_backoff(Backoff :: backoff(), ActiveSince :: integer() | undefined) -> backoff().
update_backoff(#{attempt := Attempt} = Backoff, undefined = _ActiveDuration) ->
    %% There has not been a successful connection.
    NewAttempt = Attempt + 1,
    Backoff#{current => delay(NewAttempt, Backoff), attempt => NewAttempt};
update_backoff(Backoff, ActiveDuration) when ActiveDuration > ?RESET_INTERVAL ->
    %% The last successful connection was active for more than 1 minute.
    NewAttempt = 1,
    Backoff#{current => delay(NewAttempt, Backoff), attempt => NewAttempt, active_since => undefined};
update_backoff(#{attempt := Attempt} = Backoff, _ActiveDuration) ->
    %% The last successful connection was less than 1 minute,
    NewAttempt = Attempt + 1,
    %% Resetting ActiveSince here, otherwise after being disconnected for a minute it would
    %% reset the the AttemptCount.
    Backoff#{current => delay(NewAttempt, Backoff), attempt => NewAttempt, active_since => undefined}.

-spec delay(Attempt :: non_neg_integer(), Backoff :: backoff()) -> non_neg_integer().
delay(Attempt, #{initial := Initial, max := Max, max_exp := MaxExp, uniform := Uniform} = _Backoff)
  when Attempt - 1 < MaxExp ->
    jitter(min(backoff(Initial, Attempt), Max), Uniform);
delay(_Attempt, #{max := Max, uniform := Uniform} = _Backoff) ->
    jitter(Max, Uniform).

-spec backoff(Initial :: non_neg_integer(), Attempt :: non_neg_integer()) -> non_neg_integer().
backoff(Initial, Attempt) ->
    trunc(Initial * (math:pow(2, Attempt - 1))).

-spec jitter(Value :: non_neg_integer(), Uniform :: fun(() -> float())) -> non_neg_integer().
jitter(Value, Uniform) ->
    trunc(Value - (Uniform() * ?JITTER_RATIO * Value)).
