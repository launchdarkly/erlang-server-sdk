%%-------------------------------------------------------------------
%% @doc `ldclient_storage_ets_server' module
%% @private
%% This is a simple in-memory implementation of an storage server using ETS.
%% @end
%%-------------------------------------------------------------------

-module(ldclient_storage_ets_server).

-behaviour(gen_server).

%% Supervision
-export([start_link/2, init/1]).

%% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% API
-export([create/2]).
-export([empty/2]).
-export([get/3]).
-export([all/2]).
-export([upsert/3]).
-export([upsert_clean/3]).
-export([delete/3]).

%% Types
-type state() :: #{
    tids => map(),
    tag => atom()
}.

%%===================================================================
%% Supervision
%%===================================================================

start_link(WorkerRegName, Tag) ->
    error_logger:info_msg("Starting ets server with name ~p", [WorkerRegName]),
    gen_server:start_link({local, WorkerRegName}, ?MODULE, [Tag], []).

init([Tag]) ->
    true = ldclient_update_processor_state:set_storage_initialized_state(Tag, true),
    {ok, #{tids => #{}, tag => Tag}}.

%%===================================================================
%% API
%%===================================================================

%% @doc Create a bucket
%%
%% @end
-spec create(ServerRef :: atom(), Bucket :: atom()) ->
    ok |
    {error, already_exists, string()}.
create(ServerRef, Bucket) when is_atom(Bucket) ->
    gen_server:call(ServerRef, {create, Bucket}).

%% @doc Empty a bucket
%%
%% @end
-spec empty(ServerRef :: atom(), Bucket :: atom()) ->
    ok |
    {error, bucket_not_found, string()}.
empty(ServerRef, Bucket) when is_atom(Bucket) ->
    gen_server:call(ServerRef, {empty, Bucket}).

%% @doc Get an item from the bucket by its key
%%
%% @end
-spec get(ServerRef :: atom(), Bucket :: atom(), Key :: binary()) ->
    [{Key :: binary(), Value :: any()}] |
    {error, bucket_not_found, string()}.
get(ServerRef, Bucket, Key) when is_atom(Bucket), is_binary(Key) ->
    gen_server:call(ServerRef, {get, Bucket, Key}).

%% @doc List all items in a bucket
%%
%% @end
-spec all(ServerRef :: atom(), Bucket :: atom()) ->
    [{Key :: binary(), Value :: any()}] |
    {error, bucket_not_found, string()}.
all(ServerRef, Bucket) when is_atom(Bucket) ->
    gen_server:call(ServerRef, {all, Bucket}).

%% @doc Upsert item key value pairs in an existing bucket
%%
%% @end
-spec upsert(ServerRef :: atom(), Bucket :: atom(), Items :: #{Key :: binary() => Value :: any()}) ->
    ok |
    {error, bucket_not_found, string()}.
upsert(ServerRef, Bucket, Items) when is_atom(Bucket), is_map(Items) ->
    ok = gen_server:call(ServerRef, {upsert, Bucket, Items}).

%% @doc Perform an atomic empty and upsert
%%
%% @end
-spec upsert_clean(ServerRef :: atom(), Bucket :: atom(), Items :: #{Key :: binary() => Value :: any()}) ->
    ok |
    {error, bucket_not_found, string()}.
upsert_clean(ServerRef, Bucket, Items) when is_atom(Bucket), is_map(Items) ->
    ok = gen_server:call(ServerRef, {upsert_clean, Bucket, Items}).

%% @doc Delete an item from the bucket by its key
%%
%% @end
-spec delete(ServerRef :: atom(), Bucket :: atom(), Key :: binary()) ->
    ok |
    {error, bucket_not_found, string()}.
delete(ServerRef, Bucket, Key) ->
    ok = gen_server:call(ServerRef, {delete, Bucket, Key}).

%%===================================================================
%% Behavior callbacks
%%===================================================================

-type from() :: {pid(), term()}.
-spec handle_call(term(), from(), state()) -> {reply, term(), state()}.
handle_call({create, Bucket}, _From, #{tids := Tids} = State) ->
    {Reply, NewTids} = create_bucket(bucket_exists(Bucket, Tids), Bucket, Tids),
    NewState = State#{tids := NewTids},
    {reply, Reply, NewState};
handle_call({empty, Bucket}, _From, #{tids := Tids} = State) ->
    {reply, empty_bucket(bucket_exists(Bucket, Tids), Bucket, Tids), State};
handle_call({get, Bucket, Key}, _From, #{tids := Tids} = State) ->
    {reply, lookup_key(bucket_exists(Bucket, Tids), Key, Bucket, Tids), State};
handle_call({all, Bucket}, _From, #{tids := Tids} = State) ->
    {reply, all_items(bucket_exists(Bucket, Tids), Bucket, Tids), State};
handle_call({upsert, Bucket, Item}, _From, #{tids := Tids} = State) ->
    {reply, upsert_items(bucket_exists(Bucket, Tids), Item, Bucket, Tids), State};
handle_call({upsert_clean, Bucket, Item}, _From, #{tids := Tids} = State) ->
    {reply, upsert_clean_items(bucket_exists(Bucket, Tids), Item, Bucket, Tids), State};
handle_call({delete, Bucket, Key}, _From, #{tids := Tids} = State) ->
    {reply, delete_key(bucket_exists(Bucket, Tids), Key, Bucket, Tids), State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, #{tag := Tag} = _State) -> 
    true = ldclient_update_processor_state:set_storage_initialized_state(Tag, reload),
    ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%===================================================================
%% Internal functions
%%===================================================================

%% @doc Check whether bucket already exists
%% @private
%%
%% @end
-spec bucket_exists(Bucket :: atom(), Tids :: map()) -> boolean().
bucket_exists(Bucket, Tids) when is_atom(Bucket) ->
    maps:is_key(Bucket, Tids).

%% @doc Create a bucket
%% @private
%%
%% If bucket with the same name already exists an error will be returned.
%% @end
-spec create_bucket(BucketExists :: boolean(), Bucket :: atom(), Tids :: map()) ->
    {ok|{error, already_exists, string()}, NewTids :: map()}.
create_bucket(true, Bucket, Tids) ->
    {{error, already_exists, "ETS table " ++ atom_to_list(Bucket) ++ " already exists."}, Tids};
create_bucket(false, Bucket, Tids) ->
    Tid = ets:new(Bucket, [set]),
    {ok, Tids#{Bucket => Tid}}.

%% @doc Empty a bucket
%% @private
%%
%% If no such bucket exists, error will be returned.
%% @end
-spec empty_bucket(BucketExists :: boolean(), Bucket :: atom(), Tids :: map()) ->
    ok |
    {error, bucket_not_found, string()}.
empty_bucket(false, Bucket, _Tids) ->
    {error, bucket_not_found, "ETS table " ++ atom_to_list(Bucket) ++ " does not exist."};
empty_bucket(true, Bucket, Tids) ->
    Tid = maps:get(Bucket, Tids),
    true = ets:delete_all_objects(Tid),
    ok.

%% @doc List all items in a bucket
%% @private
%%
%% Returns all items stored in the bucket.
%% @end
-spec all_items(BucketExists :: boolean(), Bucket :: atom(), Tids :: map()) ->
    [tuple()] |
    {error, bucket_not_found, string()}.
all_items(false, Bucket, _Tids) ->
    {error, bucket_not_found, "ETS table " ++ atom_to_list(Bucket) ++ " does not exist."};
all_items(true, Bucket, Tids) ->
    Tid = maps:get(Bucket, Tids),
    ets:tab2list(Tid).

%% @doc Look up an item by key
%% @private
%%
%% Search for an item by its key.
%% @end
-spec lookup_key(BucketExists :: boolean(), Key :: binary(), Bucket :: atom(), Tids :: map()) ->
    [tuple()] |
    {error, bucket_not_found, string()}.
lookup_key(false, _Key, Bucket, _Tids) ->
    {error, bucket_not_found, "ETS table " ++ atom_to_list(Bucket) ++ " does not exist."};
lookup_key(true, Key, Bucket, Tids) ->
    Tid = maps:get(Bucket, Tids),
    ets:lookup(Tid, Key).

%% @doc Upsert key value pairs in bucket
%% @private
%%
%% @end
-spec upsert_items(BucketExists :: boolean(), Items :: #{Key :: binary() => Value :: any()}, Bucket :: atom(), Tids :: map()) ->
    ok |
    {error, bucket_not_found, string()}.
upsert_items(false, _Items, Bucket, _Tids) ->
    {error, bucket_not_found, "ETS table " ++ atom_to_list(Bucket) ++ " does not exist."};
upsert_items(true, Items, Bucket, Tids) ->
    Tid = maps:get(Bucket, Tids),
    true = ets:insert(Tid, maps:to_list(Items)),
    ok.

%% @doc Empty bucket and upsert key value pairs
%% @private
%%
%% @end
-spec upsert_clean_items(BucketExists :: boolean(), Items :: #{Key :: binary() => Value :: any()}, Bucket :: atom(), Tids :: map()) ->
    ok |
    {error, bucket_not_found, string()}.
upsert_clean_items(false, _Items, Bucket, _Tids) ->
    {error, bucket_not_found, "ETS table " ++ atom_to_list(Bucket) ++ " does not exist."};
upsert_clean_items(true, Items, Bucket, Tids) ->
    Tid = maps:get(Bucket, Tids),
    true = ets:delete_all_objects(Tid),
    true = ets:insert(Tid, maps:to_list(Items)),
    ok.

%% @doc Delete an item by key
%% @private
%%
%% Delete an item by its key.
%% @end
-spec delete_key(BucketExists :: boolean(), Key :: binary(), Bucket :: atom(), Tids :: map()) ->
    ok |
    {error, bucket_not_found, string()}.
delete_key(false, _Key, Bucket, _Tids) ->
    {error, bucket_not_found, "ETS table " ++ atom_to_list(Bucket) ++ " does not exist."};
delete_key(true, Key, Bucket, Tids) ->
    Tid = maps:get(Bucket, Tids),
    true = ets:delete(Tid, Key),
    ok.
