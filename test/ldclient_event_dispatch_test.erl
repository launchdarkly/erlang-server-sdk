%%-------------------------------------------------------------------
%% @doc Event dispatcher for testing
%%
%% @end
%%-------------------------------------------------------------------

-module(ldclient_event_dispatch_test).

-behaviour(ldclient_event_dispatch).

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
    ldclient_test_events ! {OutputEvents, PayloadId},
    Result.
