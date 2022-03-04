%%-------------------------------------------------------------------
%% @doc User LRU cache
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_user_cache).

%% API
-export([get_local_reg_name/1]).
-export([notice_user/2]).

%%===================================================================
%% API
%%===================================================================

-spec get_local_reg_name(Tag :: atom()) -> atom().
get_local_reg_name(Tag) ->
    list_to_atom("ldclient_user_cache_" ++ atom_to_list(Tag)).

%% @doc Add to the set of the users we've noticed, and return true if the user
%% was already known to us.
%%
%% @end
-spec notice_user(Tag :: atom(), User :: ldclient_user:user()) -> boolean().
notice_user(_Tag, #{key := <<>>}) ->
    % Empty user key always returns true so it's not indexed
    true;
notice_user(_Tag, #{key := null}) ->
    % Null user key always returns true so it's not indexed
    true;
notice_user(Tag, #{key := UserKey}) ->
    CacheServer = get_local_reg_name(Tag),
    {Exists, _} = lru:contains_or_add(whereis(CacheServer), UserKey, UserKey),
    Exists;
notice_user(_Tag, _User) ->
    % No user key always returns true so it's not indexed
    true.
