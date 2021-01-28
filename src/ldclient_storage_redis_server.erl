%%-------------------------------------------------------------------
%% @doc `ldclient_storage_redis_server' module
%%
%% This is a implementation of an storage server using Redis.
%% @end
%%-------------------------------------------------------------------

-module(ldclient_storage_redis_server).

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
    client => eredis:client(),
    prefix => string(),
    buckets => list()
}.

%%===================================================================
%% Supervision
%%===================================================================

start_link(WorkerRegName, Tag) ->
    error_logger:info_msg("Starting redis server with name ~p", [WorkerRegName]),
    gen_server:start_link({local, WorkerRegName}, ?MODULE, [Tag], []).

init([Tag]) ->
    Host = ldclient_config:get_value(Tag, redis_host),
    Port = ldclient_config:get_value(Tag, redis_port),
    Database = ldclient_config:get_value(Tag, redis_database),
    Password = ldclient_config:get_value(Tag, redis_password),
    Prefix = ldclient_config:get_value(Tag, redis_prefix),
    {ok, Client} = eredis:start_link(Host, Port, Database, Password),
    State = #{
        client => Client,
        prefix => Prefix,
        buckets => []
    },
    {ok, State}.

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
handle_call({create, Bucket}, _From, #{client := Client, prefix := Prefix, buckets := Buckets} = State) ->
    {Reply, NewBuckets} = create_bucket(bucket_exists(Bucket, Buckets), Bucket, Client, Prefix, Buckets),
    NewState = State#{buckets := NewBuckets},
    {reply, Reply, NewState};
handle_call({empty, Bucket}, _From, #{client := Client, prefix := Prefix, buckets := Buckets} = State) ->
    {reply, empty_bucket(bucket_exists(Bucket, Buckets), Bucket, Client, Prefix), State};
handle_call({get, Bucket, Key}, _From, #{client := Client, prefix := Prefix, buckets := Buckets} = State) ->
    {reply, lookup_key(bucket_exists(Bucket, Buckets), Key, Bucket, Client, Prefix), State};
handle_call({all, Bucket}, _From, #{client := Client, prefix := Prefix, buckets := Buckets} = State) ->
    {reply, all_items(bucket_exists(Bucket, Buckets), Bucket, Client, Prefix), State};
handle_call({upsert, Bucket, Item}, _From, #{client := Client, prefix := Prefix, buckets := Buckets} = State) ->
    {reply, upsert_items(bucket_exists(Bucket, Buckets), Item, Bucket, Client, Prefix), State};
handle_call({upsert_clean, Bucket, Item}, _From, #{client := Client, prefix := Prefix, buckets := Buckets} = State) ->
    {reply, upsert_clean_items(bucket_exists(Bucket, Buckets), Item, Bucket, Client, Prefix), State};
handle_call({delete, Bucket, Key}, _From, #{client := Client, prefix := Prefix, buckets := Buckets} = State) ->
    {reply, delete_key(bucket_exists(Bucket, Buckets), Key, Bucket, Client, Prefix), State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, #{client := Client, prefix := _Prefix} = _State) -> 
    eredis:stop(Client),
    ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%===================================================================
%% Internal functions
%%===================================================================

%% @doc Check whether bucket already exists
%% @private
%%
%% @end
-spec bucket_exists(Bucket :: atom(), Buckets :: list() ) -> boolean().
bucket_exists(Bucket, Buckets) when is_atom(Bucket) ->
    lists:member(Bucket, Buckets).

%% @doc Create a bucket
%% @private
%%
%% If bucket with the same name already exists an error will be returned.
%% @end
-spec create_bucket(BucketExists :: boolean(), Bucket :: atom(), Client :: eredis:client(), Prefix :: string(), Buckets :: list()) ->
    {ok | {error, already_exists, string()}, NewBuckets :: list()}.
create_bucket(true, Bucket, _Client, _Prefix, Buckets) ->
    {{error, already_exists, "Redis hash " ++ atom_to_list(Bucket) ++ " already exists."}, Buckets};
create_bucket(false, Bucket, Client, Prefix, Buckets) ->
    {ok, _} = eredis:q(Client, ["HSET", bucket_name(Prefix, Bucket), null, null]),
    {ok, [Bucket|Buckets]}.

%% @doc Empty a bucket
%% @private
%%
%% If no such bucket exists, error will be returned.
%% @end
-spec empty_bucket(BucketExists :: boolean(), Bucket :: atom(), Client :: eredis:client(), Prefix :: string()) ->
    ok |
    {error, bucket_not_found, string()}.
empty_bucket(false, Bucket, _Client, _Prefix) ->
    {error, bucket_not_found, "Redis hash " ++ atom_to_list(Bucket) ++ " does not exist."};
empty_bucket(true, Bucket, Client, Prefix) ->
    {ok, _} = eredis:q(Client, ["DEL", bucket_name(Prefix, Bucket)]),
    {ok, _} = create_bucket(false, Bucket, Client, Prefix, []),
    ok.

%% @doc List all items in a bucket
%% @private
%%
%% Returns all items stored in the bucket.
%% @end
-spec all_items(BucketExists :: boolean(), Bucket :: atom(), Client :: eredis:client(), Prefix :: string()) ->
    [tuple()] |
    {error, bucket_not_found, string()}.
all_items(false, Bucket, _Client, _Prefix) ->
    {error, bucket_not_found, "Redis hash " ++ atom_to_list(Bucket) ++ " does not exist."};
all_items(true, Bucket, Client, Prefix) ->
    {ok, Values} = eredis:q(Client, ["HGETALL", bucket_name(Prefix, Bucket)]),
    NullFilter = [<<"null">>],
    NewValues = lists:filter(fun (Elem) -> not lists:member(Elem, NullFilter) end, Values), %This removes the initial null key and value
    Decoded = pairs(NewValues),
    keys_to_atom(Decoded).
    %pairs(NewValues).

pairs([A,B|L]) ->
    [{A, jsx:decode(B, [return_maps])}|pairs(L)];
pairs([]) -> [].

keys_to_atom([{A,B}|L]) ->
    Map = 
        maps:fold(fun(K, V, Acc) ->
            Key = binary_to_atom(K, latin1),
            Value = if is_map(V) -> fallthrough_fold(V); true -> V end,
            maps:put(Key, Value, Acc)
            end, #{}, B),
    [{A, Map}|keys_to_atom(L)];
keys_to_atom([]) -> [].

fallthrough_fold(Map) ->
    maps:fold(fun(K, V, Acc) ->
        Key = binary_to_atom(K, latin1),
        Value = if Key == bucket_by -> binary_to_atom(V, latin1); true -> V end,
        maps:put(Key, Value, Acc)
        end, #{}, Map).

%% @doc Look up an item by key
%% @private
%%
%% Search for an item by its key.
%% @end
-spec lookup_key(BucketExists :: boolean(), Key :: binary(), Bucket :: atom(), Client :: eredis:client(), Prefix :: string()) ->
    [tuple()] |
    {error, bucket_not_found, string()}.
lookup_key(false, _Key, Bucket, _Client, _Prefix) ->
    {error, bucket_not_found, "Redis hash " ++ atom_to_list(Bucket) ++ " does not exist."};
lookup_key(true, Key, Bucket, Client, Prefix) ->
    {ok, Value} = eredis:q(Client, ["HGET", bucket_name(Prefix, Bucket), Key]),
    if
        (Value == undefined) -> [];
        true ->
            Parsed = jsx:decode(Value, [return_maps]),
            Atoms = lookup_keys_to_atom(Parsed),
            [{Key, Atoms}]
    end.

lookup_keys_to_atom(Map) ->
    maps:fold(fun(K, V, Acc) ->
            Key = binary_to_atom(K, latin1),
            Value = if is_map(V) -> fallthrough_fold(V); true -> V end,
            maps:put(Key, Value, Acc)
            end, #{}, Map).

%% @doc Upsert key value pairs in bucket
%% @private
%%
%% @end
-spec upsert_items(BucketExists :: boolean(), Items :: #{Key :: binary() => Value :: any()}, Bucket :: atom(), Client :: eredis:client(), Prefix :: string()) ->
    ok |
    {error, bucket_not_found, string()}.
upsert_items(false, _Items, Bucket, _Client, _Prefix) ->
    {error, bucket_not_found, "Redis hash " ++ atom_to_list(Bucket) ++ " does not exist."};
upsert_items(true, Items, Bucket, Client, Prefix) ->
    {ok, <<"OK">>} = eredis:q(Client, ["WATCH", bucket_name(Prefix, Bucket)]),
    ok = maps:fold(
            fun(K, V, ok) ->
                {ok, _} = eredis:q(Client, ["HSET", bucket_name(Prefix, Bucket), K, jsx:encode(V)]),
                ok
            end, ok, Items),
    {ok, <<"OK">>} = eredis:q(Client, ["UNWATCH"]),
    ok.

%% @doc Empty bucket and upsert key value pairs
%% @private
%%
%% @end
-spec upsert_clean_items(BucketExists :: boolean(), Items :: #{Key :: binary() => Value :: any()}, Bucket :: atom(), Client :: eredis:client(), Prefix :: string()) ->
    ok |
    {error, bucket_not_found, string()}.
upsert_clean_items(false, _Items, Bucket, _Client, _Prefix) ->
    {error, bucket_not_found, "Redis hash " ++ atom_to_list(Bucket) ++ " does not exist."};
upsert_clean_items(true, Items, Bucket, Client, Prefix) ->
    ok = empty_bucket(true, Bucket, Client, Prefix),
    ok = upsert_items(true, Items, Bucket, Client, Prefix),
    ok.

%% @doc Delete an item by key
%% @private
%%
%% Delete an item by its key.
%% @end
-spec delete_key(BucketExists :: boolean(), Key :: binary(), Bucket :: atom(), Client :: eredis:client(), Prefix :: string()) ->
    ok |
    {error, bucket_not_found, string()}.
delete_key(false, _Key, Bucket, _Client, _Prefix) ->
    {error, bucket_not_found, "Redis hash " ++ atom_to_list(Bucket) ++ " does not exist."};
delete_key(true, Key, Bucket, Client, Prefix) ->
    {ok, _} = eredis:q(Client, ["HDEL", bucket_name(Prefix, Bucket), Key]),
    ok.

bucket_name(Prefix, Bucket) ->
    lists:concat([Prefix, ":", Bucket]).
