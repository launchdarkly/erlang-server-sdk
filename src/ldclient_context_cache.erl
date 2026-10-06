%%-------------------------------------------------------------------
%% @doc Seen-context cache
%%
%% Tracks the canonical keys of contexts we have already sent an `index' event
%% for, so repeated evaluations do not emit duplicate index events.
%%
%% This is backed by two generations of ETS sets rather than a process. The
%% check-and-insert is a single atomic `ets:insert_new/2', so it never needs a
%% message round-trip into another process, which was a serialization point on
%% the event intake path.
%%
%% When the current generation reaches the configured capacity, the previous
%% generation is dropped and the current one is demoted, so memory stays bounded
%% at roughly twice `context_keys_capacity' while still remembering contexts
%% from the last generation.
%% @private
%% @end
%%-------------------------------------------------------------------
-module(ldclient_context_cache).

%% API
-export([new/1, notice_context/2, maybe_rotate/2, delete/1]).

-define(CACHE_KEY, ldclient_context_cache).

%%===================================================================
%% API
%%===================================================================

%% @doc Create the two cache generations for a tag and publish them so the
%% intake process can reach them without a message round-trip.
%% @end
-spec new(Tag :: atom()) -> ok.
new(Tag) ->
    persistent_term:put({?CACHE_KEY, Tag}, {new_table(), new_table()}),
    ok.

%% @doc Add the context to the set of contexts we've noticed, returning true if
%% it was already known to us (and therefore no index event is needed).
%% @end
-spec notice_context(Tag :: atom(), Context :: ldclient_context:context()) -> boolean().
notice_context(Tag, Context) ->
    case cache(Tag) of
        undefined ->
            %% No cache (server not started); treat as new.
            false;
        {Current, Previous} ->
            do_notice_context({Current, Previous}, Context)
    end.

%% @doc Demote the current generation when it reaches capacity, dropping the
%% oldest generation.
%% @end
-spec maybe_rotate(Tag :: atom(), Capacity :: pos_integer()) -> ok.
maybe_rotate(Tag, Capacity) ->
    case cache(Tag) of
        undefined ->
            ok;
        {Current, Previous} ->
            case ets:info(Current, size) >= Capacity of
                true ->
                    _ = ets:delete(Previous),
                    persistent_term:put({?CACHE_KEY, Tag}, {new_table(), Current});
                false ->
                    ok
            end
    end.

-spec delete(Tag :: atom()) -> ok.
delete(Tag) ->
    case cache(Tag) of
        undefined ->
            ok;
        {Current, Previous} ->
            _ = ets:delete(Current),
            _ = ets:delete(Previous),
            _ = persistent_term:erase({?CACHE_KEY, Tag}),
            ok
    end.

%%===================================================================
%% Internal functions
%%===================================================================

-spec cache(Tag :: atom()) -> {ets:tid(), ets:tid()} | undefined.
cache(Tag) ->
    persistent_term:get({?CACHE_KEY, Tag}, undefined).

-spec do_notice_context({ets:tid(), ets:tid()}, ldclient_context:context()) -> boolean().
do_notice_context({Current, Previous}, Context) ->
    case ldclient_context:get_canonical_key(Context) of
        <<>> ->
            %% Do not add to the cache. Returning true also means we should not
            %% send an index for this invalid context.
            true;
        Key ->
            case ets:member(Previous, Key) of
                true ->
                    %% Seen in the previous generation; promote it.
                    _ = ets:insert(Current, {Key, true}),
                    true;
                false ->
                    %% insert_new returns true when the key was absent, i.e. the
                    %% context is new to us.
                    not ets:insert_new(Current, {Key, true})
            end
    end.

-spec new_table() -> ets:tid().
new_table() ->
    ets:new(?MODULE, [set, public, {write_concurrency, true}, {read_concurrency, true}]).
