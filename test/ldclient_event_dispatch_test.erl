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
    -> ldclient_event_dispatch:send_result().
send(State, OutputEvents, PayloadId, _Uri) ->
    #{sdk_key := SdkKey} = State,
    Result = case SdkKey of
        "sdk-key-events-fail" ->
            {error, temporary, "Testing event send failure."};
        "sdk-key-events-503" ->
            {error, temporary, "Testing event send failure with a status code.", 503};
        "sdk-key-events-503-then-network" ->
            %% The attempt and its retry run in the same worker process.
            case erlang:put(ldclient_test_attempted, true) of
                undefined -> {error, temporary, "Testing a 503 on the first attempt.", 503};
                true -> {error, temporary, "Testing a network error on the retry."}
            end;
        "sdk-key-events-bad-status" ->
            {error, temporary, "Testing a dispatcher that reports a non-integer status.", <<"503">>};
         _ ->
             {ok, 0}
    end,
    ldclient_test_events ! {OutputEvents, PayloadId},
    Result.
