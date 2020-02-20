%%-------------------------------------------------------------------
%% @doc `ldclient_storage_ets_server' module
%%
%% This is a simple in-memory implementation of an storage server using ETS.
%% @end
%%-------------------------------------------------------------------

-module(ldclient_storage_ets_server).

-behaviour(gen_server).

%% Supervision
-export([start_link/1, init/1]).

%% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% API
-export([create/2]).
-export([empty/2]).
-export([get/3]).
-export([list/2]).
-export([put/3]).
-export([put_clean/3]).

%% Types
-type state() :: #{
    tids => map()
}.

%%===================================================================
%% Supervision
%%===================================================================

start_link(WorkerRegName) ->
    error_logger:info_msg("Starting ets server with name ~p", [WorkerRegName]),
    gen_server:start_link({local, WorkerRegName}, ?MODULE, [], []).

init([]) ->
    {ok, #{tids => #{}}}.

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
-spec list(ServerRef :: atom(), Bucket :: atom()) ->
    [{Key :: binary(), Value :: any()}] |
    {error, bucket_not_found, string()}.
list(ServerRef, Bucket) when is_atom(Bucket) ->
    gen_server:call(ServerRef, {list, Bucket}).

%% @doc Put item key value pairs in an existing bucket
%%
%% @end
-spec put(ServerRef :: atom(), Bucket :: atom(), Items :: #{Key :: binary() => Value :: any()}) ->
    ok |
    {error, bucket_not_found, string()}.
put(ServerRef, Bucket, Items) when is_atom(Bucket), is_map(Items) ->
    ok = gen_server:call(ServerRef, {put, Bucket, Items}).

%% @doc Perform an atomic empty and put
%%
%% @end
-spec put_clean(ServerRef :: atom(), Bucket :: atom(), Items :: #{Key :: binary() => Value :: any()}) ->
    ok |
    {error, bucket_not_found, string()}.
put_clean(ServerRef, Bucket, Items) when is_atom(Bucket), is_map(Items) ->
    ok = gen_server:call(ServerRef, {put_clean, Bucket, Items}).

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
handle_call({list, Bucket}, _From, #{tids := Tids} = State) ->
    {reply, list_items(bucket_exists(Bucket, Tids), Bucket, Tids), State};
handle_call({put, Bucket, Item}, _From, #{tids := Tids} = State) ->
    {reply, put_items(bucket_exists(Bucket, Tids), Item, Bucket, Tids), State};
handle_call({put_clean, Bucket, Item}, _From, #{tids := Tids} = State) ->
    {reply, put_clean_items(bucket_exists(Bucket, Tids), Item, Bucket, Tids), State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

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
-spec list_items(BucketExists :: boolean(), Bucket :: atom(), Tids :: map()) ->
    [tuple()] |
    {error, bucket_not_found, string()}.
list_items(false, Bucket, _Tids) ->
    {error, bucket_not_found, "ETS table " ++ atom_to_list(Bucket) ++ " does not exist."};
list_items(true, Bucket, Tids) ->
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

%% @doc Put key value pairs in bucket
%% @private
%%
%% @end
-spec put_items(BucketExists :: boolean(), Items :: #{Key :: binary() => Value :: any()}, Bucket :: atom(), Tids :: map()) ->
    ok |
    {error, bucket_not_found, string()}.
put_items(false, _Items, Bucket, _Tids) ->
    {error, bucket_not_found, "ETS table " ++ atom_to_list(Bucket) ++ " does not exist."};
put_items(true, Items, Bucket, Tids) ->
    Tid = maps:get(Bucket, Tids),
    true = ets:insert(Tid, maps:to_list(Items)),
    ok.

%% @doc Empty bucket and put key value pairs
%% @private
%%
%% @end
-spec put_clean_items(BucketExists :: boolean(), Items :: #{Key :: binary() => Value :: any()}, Bucket :: atom(), Tids :: map()) ->
    ok |
    {error, bucket_not_found, string()}.
put_clean_items(false, _Items, Bucket, _Tids) ->
    {error, bucket_not_found, "ETS table " ++ atom_to_list(Bucket) ++ " does not exist."};
put_clean_items(true, Items, Bucket, Tids) ->
    Tid = maps:get(Bucket, Tids),
    true = ets:delete_all_objects(Tid),
    true = ets:insert(Tid, maps:to_list(Items)),
    ok.
