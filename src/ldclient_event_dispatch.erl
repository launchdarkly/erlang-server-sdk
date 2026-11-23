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
%% The result of one delivery attempt: the server time from the response on
%% success, or the failure class and a description. An HTTP error response is
%% reported with its status code, which the pipeline carries in its telemetry
%% (`[ldclient, events, flush]' and `[ldclient, events, send_error]').
-type send_result() ::
    {ok, ServerTime :: integer()}
    | {error, temporary | permanent, Reason :: string()}
    | {error, temporary | permanent, Reason :: string(), StatusCode :: pos_integer()}.

-export_type([send_result/0]).

-callback send(State:: any(), OutputEvents :: binary(), PayloadId :: uuid:uuid(), Uri :: string()) ->
    send_result().

%% `init' should return an initial value for the `State' argument to `send'
-callback init(Tag :: atom(), SdkKey :: string()) -> any().

%% `stop' releases anything `init' set up per instance (for example an httpc
%% profile). It is called when the instance's event pipeline stops, including
%% after a failed start, and must tolerate being called more than once.
-callback stop(Tag :: atom()) -> ok.

-optional_callbacks([stop/1]).
