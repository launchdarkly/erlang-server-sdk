%%-------------------------------------------------------------------
%% @doc Event dispatcher that forwards the payload and then crashes the calling
%% worker on the first send for an instance (as if the worker died after the
%% request was delivered), and succeeds afterwards, for tests.
%% @end
%%-------------------------------------------------------------------

-module(ldclient_event_dispatch_crash_once).

-behaviour(ldclient_event_dispatch).

%% Behavior callbacks
-export([init/2, send/4]).

%%===================================================================
%% Behavior callbacks
%%===================================================================

-spec init(Tag :: atom(), SdkKey :: string()) -> any().
init(Tag, SdkKey) ->
    #{tag => Tag, sdk_key => SdkKey}.

-spec send(State :: any(), OutputEvents :: list(), PayloadId :: uuid:uuid(), Uri :: string())
    -> {ok, integer()}.
send(#{tag := Tag}, OutputEvents, PayloadId, _Uri) ->
    Key = {?MODULE, Tag},
    case persistent_term:get(Key, first) of
        first ->
            persistent_term:put(Key, crashed),
            ldclient_test_events ! {OutputEvents, PayloadId},
            error(simulated_worker_crash);
        crashed ->
            ldclient_test_events ! {OutputEvents, PayloadId},
            {ok, 0}
    end.
