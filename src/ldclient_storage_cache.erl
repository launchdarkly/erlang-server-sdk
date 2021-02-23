%%-------------------------------------------------------------------
%% @doc `ldclient_storage_cache' module
%% @private
%% Provides implementation of storage cache using Erlang map.
%% @end
%%-------------------------------------------------------------------

-module(ldclient_storage_cache).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

%% Behavior callbacks
-export([init/3]).
-export([create/4]).
-export([empty/4]).
-export([get/5]).
-export([upsert/5]).
-export([upsert_clean/5]).
-export([delete/5]).
-export([terminate/1]).

%%===================================================================
%% Behavior callbacks
%%===================================================================

-spec init(SupRef :: atom(), Tag :: atom(), Options :: list()) ->
    ok.
init(SupRef, Tag, _) ->
    SupRegName = get_local_reg_name(supervisor, Tag),
    WorkerRegName = get_local_reg_name(worker, Tag),
    StorageCacheSup = ?CHILD(ldclient_storage_cache_sup, ldclient_storage_cache_sup, [SupRegName, WorkerRegName, Tag], supervisor),
    {ok, _} = supervisor:start_child(SupRef, StorageCacheSup),
    ok.

-spec create(Tag :: atom(), Bucket :: atom(), ServerRef :: atom(), StorageBackend :: atom()) ->
    ok |
    {error, already_exists, string()}.
create(Tag, Bucket, ServerRef, StorageBackend) ->
    LocalServerRef = get_local_reg_name(worker, Tag),
    _ = ldclient_storage_cache_server:create(LocalServerRef, Bucket),
    StorageBackend:create(ServerRef, Bucket).

-spec empty(Tag :: atom(), Bucket :: atom(), ServerRef :: atom(), StorageBackend :: atom()) ->
    ok |
    {error, bucket_not_found, string()}.
empty(Tag, Bucket, ServerRef, StorageBackend) ->
    LocalServerRef = get_local_reg_name(worker, Tag),
    ok = ldclient_storage_cache_server:empty(LocalServerRef, Bucket),
    StorageBackend:empty(ServerRef, Bucket).

-spec get(Tag :: atom(), Bucket :: atom(), Key :: binary(), ServerRef :: atom(), StorageBackend :: atom()) ->
    [{Key :: binary(), Value :: any()}] |
    {error, bucket_not_found, string()}.
get(Tag, Bucket, Key, ServerRef, StorageBackend) ->
    LocalServerRef = get_local_reg_name(worker, Tag),
    case ldclient_storage_cache_server:get(LocalServerRef, Bucket, Key) of
        {Value, Hit} -> case Hit of
                            false -> StorageBackend:get(ServerRef, Bucket, Key);
                            true -> Value
                        end;
        {error, bucket_not_found, Error} -> {error, bucket_not_found, Error}
    end.

-spec upsert(Tag :: atom(), Bucket :: atom(), Items :: #{Key :: binary() => Value :: any()}, ServerRef :: atom(), StorageBackend :: atom()) ->
    ok |
    {error, bucket_not_found, string()}.
upsert(Tag, Bucket, Items, ServerRef, StorageBackend) ->
    LocalServerRef = get_local_reg_name(worker, Tag),
    ok = ldclient_storage_cache_server:upsert(LocalServerRef, Bucket, Items),
    StorageBackend:upsert(ServerRef, Bucket, Items).

-spec upsert_clean(Tag :: atom(), Bucket :: atom(), Items :: #{Key :: binary() => Value :: any()}, ServerRef :: atom(), StorageBackend :: atom()) ->
    ok |
    {error, bucket_not_found, string()}.
upsert_clean(Tag, Bucket, Items, ServerRef, StorageBackend) ->
    LocalServerRef = get_local_reg_name(worker, Tag),
    ok = ldclient_storage_cache_server:upsert_clean(LocalServerRef, Bucket, Items),
    StorageBackend:upsert_clean(ServerRef, Bucket, Items).

-spec delete(Tag :: atom(), Bucket :: atom(), Key :: binary(), ServerRef :: atom(), StorageBackend :: atom()) ->
    ok |
    {error, bucket_not_found, string()}.
delete(Tag, Bucket, Key, ServerRef, StorageBackend) ->
    LocalServerRef = get_local_reg_name(worker, Tag),
    ok =ldclient_storage_cache_server:delete(LocalServerRef, Bucket, Key),
    StorageBackend:delete(ServerRef, Bucket, Key).

-spec terminate(Tag :: atom()) -> ok.
terminate(_Tag) -> ok.

%%===================================================================
%% Internal functions
%%===================================================================

-spec get_local_reg_name(atom(), Tag :: atom()) -> atom().
get_local_reg_name(supervisor, Tag) ->
    list_to_atom("ldclient_storage_cache_sup_" ++ atom_to_list(Tag));
get_local_reg_name(worker, Tag) ->
    list_to_atom("ldclient_storage_cache_server_" ++ atom_to_list(Tag)).