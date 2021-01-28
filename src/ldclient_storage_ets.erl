%%-------------------------------------------------------------------
%% @doc `ldclient_storage_ets' module
%% @private
%% Provides implementation of ETS storage backend behavior.
%% @end
%%-------------------------------------------------------------------

-module(ldclient_storage_ets).

-behaviour(ldclient_storage_engine).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

%% Behavior callbacks
-export([init/3]).
-export([create/2]).
-export([empty/2]).
-export([get/3]).
-export([all/2]).
-export([upsert/3]).
-export([upsert_clean/3]).
-export([delete/3]).
-export([terminate/1]).

%%===================================================================
%% Behavior callbacks
%%===================================================================

-spec init(SupRef :: atom(), Tag :: atom(), Options :: list()) ->
    ok.
init(SupRef, Tag, _) ->
    SupRegName = get_local_reg_name(supervisor, Tag),
    WorkerRegName = get_local_reg_name(worker, Tag),
    StorageSup = ?CHILD(ldclient_storage_ets_sup, ldclient_storage_ets_sup, [SupRegName, WorkerRegName, Tag], supervisor),
    {ok, _} = supervisor:start_child(SupRef, StorageSup),
    % Pre-create features and segments buckets
    ok = create(Tag, features),
    ok = create(Tag, segments),
    Reload = ldclient_update_processor_state:get_storage_initialized_state(Tag),
    case Reload of
        reload -> ok = ldclient_updater:stop(list_to_atom("ldclient_instance_stream_" ++ atom_to_list(Tag)));
        _ -> ok
    end.

-spec create(Tag :: atom(), Bucket :: atom()) ->
    ok |
    {error, already_exists, string()}.
create(Tag, Bucket) ->
    ServerRef = get_local_reg_name(worker, Tag),
    ldclient_storage_ets_server:create(ServerRef, Bucket).

-spec empty(Tag :: atom(), Bucket :: atom()) ->
    ok |
    {error, bucket_not_found, string()}.
empty(Tag, Bucket) ->
    ServerRef = get_local_reg_name(worker, Tag),
    ldclient_storage_ets_server:empty(ServerRef, Bucket).

-spec get(Tag :: atom(), Bucket :: atom(), Key :: binary()) ->
    [{Key :: binary(), Value :: any()}] |
    {error, bucket_not_found, string()}.
get(Tag, Bucket, Key) ->
    ServerRef = get_local_reg_name(worker, Tag),
    ldclient_storage_ets_server:get(ServerRef, Bucket, Key).

-spec all(Tag :: atom(), Bucket :: atom()) ->
    [{Key :: binary(), Value :: any()}] |
    {error, bucket_not_found, string()}.
all(Tag, Bucket) ->
    ServerRef = get_local_reg_name(worker, Tag),
    ldclient_storage_ets_server:all(ServerRef, Bucket).

-spec upsert(Tag :: atom(), Bucket :: atom(), Items :: #{Key :: binary() => Value :: any()}) ->
    ok |
    {error, bucket_not_found, string()}.
upsert(Tag, Bucket, Items) ->
    ServerRef = get_local_reg_name(worker, Tag),
    ldclient_storage_ets_server:upsert(ServerRef, Bucket, Items).

-spec upsert_clean(Tag :: atom(), Bucket :: atom(), Items :: #{Key :: binary() => Value :: any()}) ->
    ok |
    {error, bucket_not_found, string()}.
upsert_clean(Tag, Bucket, Items) ->
    ServerRef = get_local_reg_name(worker, Tag),
    ldclient_storage_ets_server:upsert_clean(ServerRef, Bucket, Items).

-spec delete(Tag :: atom(), Bucket :: atom(), Key :: binary()) ->
    ok |
    {error, bucket_not_found, string()}.
delete(Tag, Bucket, Key) ->
    ServerRef = get_local_reg_name(worker, Tag),
    ldclient_storage_ets_server:delete(ServerRef, Bucket, Key).

-spec terminate(Tag :: atom()) -> ok.
terminate(_Tag) -> ok.

%%===================================================================
%% Internal functions
%%===================================================================

-spec get_local_reg_name(atom(), Tag :: atom()) -> atom().
get_local_reg_name(supervisor, Tag) ->
    list_to_atom("ldclient_storage_ets_sup_" ++ atom_to_list(Tag));
get_local_reg_name(worker, Tag) ->
    list_to_atom("ldclient_storage_ets_server_" ++ atom_to_list(Tag)).
