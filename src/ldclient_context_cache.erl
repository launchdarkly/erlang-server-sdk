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
%% Rotation is enforced on insert: as soon as the current generation reaches
%% `context_keys_capacity' the previous generation is dropped and the current
%% one is demoted, so memory stays bounded at roughly twice the configured
%% capacity. The cache is owned by the event server, which is the only caller,
%% so the rotation is a local operation with no coordination.
%% @private
%% @end
%%-------------------------------------------------------------------
-module(ldclient_context_cache).

%% API
-export([new/0, notice_context/3, size/1, delete/1]).

-export_type([cache/0]).

-opaque cache() :: {Current :: ets:tid(), Previous :: ets:tid()}.

%%===================================================================
%% API
%%===================================================================

-spec new() -> cache().
new() ->
    {new_table(), new_table()}.

%% @doc Add the context to the set of contexts we've noticed. Returns whether it
%% was already known to us (and therefore no index event is needed) together
%% with the (possibly rotated) cache.
%% @end
-spec notice_context(cache(), ldclient_context:context(), pos_integer()) ->
    {boolean(), cache()}.
notice_context(Cache = {Current, Previous}, Context, Capacity) ->
    Seen = case ldclient_context:get_canonical_key(Context) of
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
    end,
    {Seen, maybe_rotate(Cache, Capacity)}.

-spec delete(cache()) -> ok.
delete({Current, Previous}) ->
    _ = ets:delete(Current),
    _ = ets:delete(Previous),
    ok.

%% @doc Total number of cached contexts across both generations.
%% @end
-spec size(cache()) -> non_neg_integer().
size({Current, Previous}) ->
    ets:info(Current, size) + ets:info(Previous, size).

%%===================================================================
%% Internal functions
%%===================================================================

%% @doc Demote the current generation when it reaches capacity, dropping the
%% oldest generation.
%% @end
-spec maybe_rotate(cache(), pos_integer()) -> cache().
maybe_rotate({Current, Previous} = Cache, Capacity) ->
    case ets:info(Current, size) >= Capacity of
        true ->
            _ = ets:delete(Previous),
            {new_table(), Current};
        false ->
            Cache
    end.

-spec new_table() -> ets:tid().
new_table() ->
    %% Only the owning event server reads and writes these tables, so keep
    %% them protected and skip the concurrency options (which only add cost to
    %% a single-writer member/insert pattern).
    ets:new(?MODULE, [set, protected]).
