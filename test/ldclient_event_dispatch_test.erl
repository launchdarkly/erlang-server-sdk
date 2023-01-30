%%-------------------------------------------------------------------
%% @doc Event dispatcher for testing
%%
%% @end
%%-------------------------------------------------------------------

-module(ldclient_event_dispatch_test).

-behaviour(ldclient_event_dispatch).

%% Behavior callbacks
-export([init/2, send/4]).

%%===================================================================
%% Behavior callbacks
%%===================================================================

-spec init(Tag :: atom(), SdkKey :: string()) -> any().
init(_Tag, SdkKey) ->
    #{sdk_key => SdkKey}.

%% @doc Send events to test event server process
%%
%% @end
-spec send(State :: any(), OutputEvents :: list(), PayloadId :: uuid:uuid(), Uri :: string())
    -> {error, temporary, string()}.
send(State, OutputEvents, PayloadId, _Uri) ->
    #{sdk_key := SdkKey} = State,
    Result = case SdkKey of
        "sdk-key-events-fail" ->
            {error, temporary, "Testing event send failure."};
         _ ->
             {ok, 0}
    end,
    ldclient_test_events ! {OutputEvents, PayloadId},
    Result.
