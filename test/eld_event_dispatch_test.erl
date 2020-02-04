%%-------------------------------------------------------------------
%% @doc Event dispatcher for testing
%%
%% @end
%%-------------------------------------------------------------------

-module(eld_event_dispatch_test).

-behaviour(eld_event_dispatch).

%% Behavior callbacks
-export([send/4]).

%%===================================================================
%% Behavior callbacks
%%===================================================================

%% @doc Send events to test event server process
%%
%% @end
-spec send(OutputEvents :: list(), PayloadId :: uuid:uuid(), Uri :: string(), SdkKey :: string())
    -> ok | {error, temporary, string()}.
send(OutputEvents, PayloadId, _Uri, SdkKey) ->
    Result = case SdkKey of
        "sdk-key-events-fail" ->
            {error, temporary, "Testing event send failure."};
         _ ->
            ok
    end,
    eld_test_events ! {OutputEvents, PayloadId},
    Result.
