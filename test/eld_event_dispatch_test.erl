%%-------------------------------------------------------------------
%% @doc Event dispatcher for testing
%%
%% @end
%%-------------------------------------------------------------------

-module(eld_event_dispatch_test).

-behaviour(eld_event_dispatch).

%% Behavior callbacks
-export([send/3]).

%%===================================================================
%% Behavior callbacks
%%===================================================================

%% @doc Send events to test event server process
%%
%% @end
-spec send(OutputEvents :: list(), Uri :: string(), SdkKey :: string()) -> ok.
send(OutputEvents, _Uri, _SdkKey) ->
    eld_test_events ! OutputEvents,
    ok.
