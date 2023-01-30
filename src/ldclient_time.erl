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
    start_timer/3,
    datetime_to_timestamp/1
]).

%% The difference between the start of the Gregorian calendar and 1/1/1970 (Unix Epoch).
-define(GREGORIAN_UNIX_OFFSET_SECONDS, 62167219200).

-spec time_seconds() -> integer().
time_seconds() ->
    erlang:monotonic_time(second).

-spec start_timer(Time :: non_neg_integer(), Dest :: pid() | atom(),  Msg :: term())  -> Timer :: reference().
start_timer(Time,  Dest, Msg) ->
    erlang:start_timer(Time, Dest, Msg).

%% There isn't a built-in method to convert from a calendar:datetime() to a unix timestamp.
%% This is useful to allow comparison between a parsed date and erlang:system_time.
-spec datetime_to_timestamp(DateTime :: calendar:datetime()) -> UtcMilliseconds :: integer().
datetime_to_timestamp(DateTime) ->
    (calendar:datetime_to_gregorian_seconds(DateTime) - ?GREGORIAN_UNIX_OFFSET_SECONDS) * 1000.
