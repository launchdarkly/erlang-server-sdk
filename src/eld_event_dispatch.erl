%%-------------------------------------------------------------------
%% @doc `eld_event_dispatch' module
%%
%% This is a behavior that event dispatchers must implement. It is used to send
%% event batches to LaunchDarkly.
%% @end
%%-------------------------------------------------------------------

-module(eld_event_dispatch).

%% `send' must dispatch the batch of events. It takes the list of events, the
%% destination URI and SDK key. It must return success or temporary or
%% permanent failure.
-callback send(OutputEvents :: binary(), Uri :: string(), SdkKey :: string()) ->
    ok | {error, temporary, string()} | {error, permanent, string()}.
