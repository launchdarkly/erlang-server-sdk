%%-------------------------------------------------------------------
%% @doc Event dispatcher that always fails permanently, for tests.
%% @end
%%-------------------------------------------------------------------

-module(ldclient_event_dispatch_permanent).

-behaviour(ldclient_event_dispatch).

%% Behavior callbacks
-export([init/2, send/4]).

%%===================================================================
%% Behavior callbacks
%%===================================================================

-spec init(Tag :: atom(), SdkKey :: string()) -> any().
init(_Tag, SdkKey) ->
    #{sdk_key => SdkKey}.

-spec send(State :: any(), OutputEvents :: list(), PayloadId :: uuid:uuid(), Uri :: string())
    -> {error, permanent, string()}.
send(_State, OutputEvents, PayloadId, _Uri) ->
    ldclient_test_events ! {OutputEvents, PayloadId},
    {error, permanent, "Testing permanent event send failure."}.
