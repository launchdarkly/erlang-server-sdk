%%-------------------------------------------------------------------
%% @doc A dispatcher whose init raises, so that no reporter worker can start,
%% and whose stop reports to the test process.
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_event_dispatch_init_fail).

-behaviour(ldclient_event_dispatch).

-export([init/2, send/4, stop/1]).

-spec init(Tag :: atom(), SdkKey :: string()) -> any().
init(_Tag, _SdkKey) ->
    error(dispatcher_init_failed).

-spec send(State :: any(), OutputEvents :: binary(), PayloadId :: uuid:uuid(), Uri :: string()) ->
    ldclient_event_dispatch:send_result().
send(_State, _OutputEvents, _PayloadId, _Uri) ->
    {error, permanent, "never started"}.

-spec stop(Tag :: atom()) -> ok.
stop(Tag) ->
    ldclient_test_events ! {dispatcher_stopped, Tag},
    ok.
