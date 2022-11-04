%%-------------------------------------------------------------------
%% @doc Local module for erlang module time methods.
%% This allows them to be easily mocked for testing.
%% @private
%% @end
%%-------------------------------------------------------------------
-module(ldclient_time).

%% API
-export([
    time_seconds/0,
    start_timer/3
]).

-spec time_seconds() -> integer().
time_seconds() ->
    erlang:monotonic_time(second).

-spec start_timer(Time :: non_neg_integer(), Dest :: pid() | atom(),  Msg :: term())  -> Timer :: reference().
start_timer(Time,  Dest, Msg) ->
    erlang:start_timer(Time, Dest, Msg).
