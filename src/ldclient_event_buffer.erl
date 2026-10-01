%%-------------------------------------------------------------------
%% @doc Bounded FIFO buffer for pending events
%%
%% Backed by an ETS `ordered_set' so that writes and reads are decoupled from
%% the owning process heap. Entries are keyed by a node-wide monotonic integer,
%% which preserves insertion (FIFO) order.
%% @private
%% @end
%%-------------------------------------------------------------------
-module(ldclient_event_buffer).

%% API
-export([new/0, insert/2, pop_batch/2, delete/1]).

-export_type([buffer/0]).

-opaque buffer() :: ets:tid().

%%===================================================================
%% API
%%===================================================================

-spec new() -> buffer().
new() ->
    ets:new(?MODULE, [ordered_set, protected, {write_concurrency, true}, {read_concurrency, true}]).

-spec insert(buffer(), term()) -> ok.
insert(Buffer, Event) ->
    Key = erlang:unique_integer([monotonic, positive]),
    true = ets:insert(Buffer, {Key, Event}),
    ok.

%% @doc Remove and return up to `Max' of the oldest buffered entries.
%% @end
-spec pop_batch(buffer(), non_neg_integer()) -> [term()].
pop_batch(Buffer, Max) ->
    pop_batch(Buffer, Max, []).

-spec delete(buffer()) -> ok.
delete(Buffer) ->
    ets:delete(Buffer).

%%===================================================================
%% Internal functions
%%===================================================================

-spec pop_batch(buffer(), non_neg_integer(), [term()]) -> [term()].
pop_batch(_Buffer, 0, Acc) ->
    lists:reverse(Acc);
pop_batch(Buffer, Max, Acc) ->
    case ets:first(Buffer) of
        '$end_of_table' ->
            lists:reverse(Acc);
        Key ->
            case ets:take(Buffer, Key) of
                [{Key, Event}] ->
                    pop_batch(Buffer, Max - 1, [Event|Acc]);
                [] ->
                    %% Another consumer took the entry first; try the next one.
                    pop_batch(Buffer, Max, Acc)
            end
    end.
