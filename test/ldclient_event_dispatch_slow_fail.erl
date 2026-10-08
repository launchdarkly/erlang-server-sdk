%%-------------------------------------------------------------------
%% @doc Test event dispatcher that is slow and always fails temporarily, so
%% every batch occupies its worker for a while and then leaves a scheduled
%% retry behind. Payloads are forwarded to the `ldclient_test_events' collector.
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_event_dispatch_slow_fail).

-behaviour(ldclient_event_dispatch).

-export([init/2, send/4]).

-define(DELAY_MS, 200).

-spec init(Tag :: atom(), SdkKey :: string()) -> any().
init(_Tag, SdkKey) ->
    #{sdk_key => SdkKey}.

-spec send(State :: any(), OutputEvents :: list(), PayloadId :: uuid:uuid(), Uri :: string()) ->
    {error, temporary, string()}.
send(_State, OutputEvents, PayloadId, _Uri) ->
    timer:sleep(?DELAY_MS),
    ldclient_test_events ! {OutputEvents, PayloadId},
    {error, temporary, "Testing a slow, temporarily failing endpoint."}.
