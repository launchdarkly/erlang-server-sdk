%%-------------------------------------------------------------------
%% @doc User LRU cache
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_context_cache).

%% API
-export([get_local_reg_name/1]).
-export([notice_context/2]).

%%===================================================================
%% API
%%===================================================================

-spec get_local_reg_name(Tag :: atom()) -> atom().
get_local_reg_name(Tag) ->
    list_to_atom("ldclient_context_cache_" ++ atom_to_list(Tag)).

%% @doc Add to the set of the users we've noticed, and return true if the user
%% was already known to us.
%%
%% @end
-spec notice_context(Tag :: atom(), Context :: ldclient_context:context()) -> boolean().
notice_context(Tag, Context) ->
    CanonicalKey = ldclient_context:get_canonical_key(Context),
    notice_context_canonical_key(Tag, CanonicalKey).

-spec notice_context_canonical_key(Tag :: atom(), CanonicalKey :: binary()) -> boolean().
notice_context_canonical_key(_Tag, <<>>) ->
    %% Do not add to the cache. Returning true also means we should not send an index for this invalid user.
    true;
notice_context_canonical_key(Tag, CanonicalKey) ->
    CacheServer = get_local_reg_name(Tag),
    {Exists, _} = lru:contains_or_add(whereis(CacheServer), CanonicalKey, CanonicalKey),
    Exists.
