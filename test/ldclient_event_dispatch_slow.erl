%%-------------------------------------------------------------------
%% @doc Event dispatcher that delays before succeeding, for tests.
%% @end
%%-------------------------------------------------------------------

-module(ldclient_event_dispatch_slow).

-behaviour(ldclient_event_dispatch).

%% Behavior callbacks
-export([init/2, send/4]).

-define(DELAY_MS, 200).

%%===================================================================
%% Behavior callbacks
%%===================================================================

-spec init(Tag :: atom(), SdkKey :: string()) -> any().
init(_Tag, SdkKey) ->
    #{sdk_key => SdkKey}.

-spec send(State :: any(), OutputEvents :: list(), PayloadId :: uuid:uuid(), Uri :: string())
    -> {ok, integer()}.
send(_State, OutputEvents, PayloadId, _Uri) ->
    timer:sleep(?DELAY_MS),
    ldclient_test_events ! {OutputEvents, PayloadId},
    {ok, 0}.
