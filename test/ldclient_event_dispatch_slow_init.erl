%%-------------------------------------------------------------------
%% @doc Test event dispatcher whose initialisation takes a while, so that the
%% event server's `init/1' (which starts the workers) is observably long.
%% Sending behaves like `ldclient_event_dispatch_test'.
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_event_dispatch_slow_init).

-behaviour(ldclient_event_dispatch).

-export([init/2, send/4]).

-define(INIT_DELAY_MS, 300).

-spec init(Tag :: atom(), SdkKey :: string()) -> any().
init(Tag, SdkKey) ->
    timer:sleep(?INIT_DELAY_MS),
    ldclient_event_dispatch_test:init(Tag, SdkKey).

-spec send(State :: any(), OutputEvents :: list(), PayloadId :: uuid:uuid(), Uri :: string()) ->
    {ok, integer()} | {error, temporary, string()}.
send(State, OutputEvents, PayloadId, Uri) ->
    ldclient_event_dispatch_test:send(State, OutputEvents, PayloadId, Uri).
