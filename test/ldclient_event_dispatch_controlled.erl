%%-------------------------------------------------------------------
%% @doc A dispatcher the tests steer per tag through persistent_term: whether
%% and how slowly init fails, how long send takes and what it returns. stop
%% reports to the test collector.
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_event_dispatch_controlled).

-behaviour(ldclient_event_dispatch).

-export([init/2, send/4, stop/1]).
-export([set/3, reset/1]).

-define(KEYS, [init_fail, init_delay_ms, send_delay_ms, send_result]).

%% init_fail (false), init_delay_ms (0), send_delay_ms (0), send_result ({ok, 0}).
-spec set(Tag :: atom(), Key :: atom(), Value :: term()) -> ok.
set(Tag, Key, Value) when is_atom(Tag) ->
    true = lists:member(Key, ?KEYS),
    persistent_term:put({?MODULE, Tag, Key}, Value).

-spec reset(Tag :: atom()) -> ok.
reset(Tag) when is_atom(Tag) ->
    lists:foreach(fun(Key) -> persistent_term:erase({?MODULE, Tag, Key}) end, ?KEYS).

-spec init(Tag :: atom(), SdkKey :: string()) -> any().
init(Tag, _SdkKey) ->
    timer:sleep(get(Tag, init_delay_ms, 0)),
    case get(Tag, init_fail, false) of
        true -> error(dispatcher_init_failed);
        false -> #{tag => Tag}
    end.

-spec send(State :: any(), OutputEvents :: binary(), PayloadId :: uuid:uuid(), Uri :: string()) ->
    ldclient_event_dispatch:send_result().
send(#{tag := Tag}, OutputEvents, PayloadId, _Uri) ->
    timer:sleep(get(Tag, send_delay_ms, 0)),
    ldclient_test_events ! {OutputEvents, PayloadId},
    get(Tag, send_result, {ok, 0}).

-spec stop(Tag :: atom()) -> ok.
stop(Tag) ->
    _ = (catch ldclient_test_events ! {dispatcher_stopped, Tag}),
    ok.

get(Tag, Key, Default) ->
    persistent_term:get({?MODULE, Tag, Key}, Default).
