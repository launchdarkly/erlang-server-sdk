%%-------------------------------------------------------------------
%% @doc `ldclient_event_dispatch' module
%% @private
%% This is a behavior that event dispatchers must implement. It is used to send
%% event batches to LaunchDarkly.
%% @end
%%-------------------------------------------------------------------

-module(ldclient_event_dispatch).

%% `send' must dispatch the batch of events. It takes the list of events, the
%% destination URI and SDK key. It must return success or temporary or
%% permanent failure.
-callback send(State:: any(), OutputEvents :: binary(), PayloadId :: uuid:uuid(), Uri :: string()) ->
    {ok, integer()} | {error, temporary, string()} | {error, permanent, string()}.

%% `init' should return an initial value for the `State' argument to `send'
-callback init(Tag :: atom(), SdkKey :: string()) -> any().
