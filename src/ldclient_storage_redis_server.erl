%%-------------------------------------------------------------------
%% @doc `ldclient_storage_redis_server' module
%% @private
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
-export([set_init/1]).
-export([get_init/1]).

-type client_pid() :: pid() |
                  atom() |
                  {atom(), atom()} |
                  {global, term()} |
                  {via, atom(), term()}.

%% Types
-type state() :: #{
client => client_pid(),
prefix => string(),
buckets => list(),
tag => atom()
}.

%%===================================================================
%% Supervision
%%===================================================================

start_link(WorkerRegName, Tag) ->
    error_logger:info_msg("Starting redis server with name ~p", [WorkerRegName]),
    gen_server:start_link({local, WorkerRegName}, ?MODULE, [Tag], []).

-spec set_tls_options(Options :: list(), TlsOptions :: list()) -> OutOptions :: list().
set_tls_options(Options, []) -> Options;
set_tls_options(Options, undefined) -> Options;
set_tls_options(Options, TlsOptions) -> lists:append(Options, [{tls, TlsOptions}]).

init([Tag]) ->
    Host = ldclient_config:get_value(Tag, redis_host),
    Port = ldclient_config:get_value(Tag, redis_port),
    Database = ldclient_config:get_value(Tag, redis_database),
    Username = ldclient_config:get_value(Tag, redis_username),
    Password = ldclient_config:get_value(Tag, redis_password),
    Prefix = ldclient_config:get_value(Tag, redis_prefix),
    TlsOpts = ldclient_config:get_value(Tag, redis_tls),
    BasicOpts = [
        {host, Host}, {port, Port}, {database, Database}, {username, Username}, {password, Password}
    ],
    EredisOpts = set_tls_options(BasicOpts, TlsOpts),
    {ok, Client} = eredis:start_link(EredisOpts),
    State = #{
        client => Client,
        prefix => Prefix,
        buckets => [],
        tag => Tag
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

set_init(ServerRef) ->
    ok = gen_server:call(ServerRef, {set_init}).

get_init(ServerRef) ->
    gen_server:call(ServerRef, {get_init}).

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
    {reply, delete_key(bucket_exists(Bucket, Buckets), Key, Bucket, Client, Prefix), State};
handle_call({set_init}, _From, #{client := Client, prefix := Prefix} = State) ->
    {reply, set_init(Client, Prefix), State};
handle_call({get_init}, _From, #{client := Client, prefix := Prefix} = State) ->
    {reply, get_init(Client, Prefix), State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, #{client := Client, tag := Tag} = _State) ->
    eredis:stop(Client),
    true = ldclient_update_processor_state:set_storage_initialized_state(Tag, false),
    ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%===================================================================
%% Internal functions
%%===================================================================

%% @doc Check whether bucket already exists
%% @private
%%
%% @end
-spec bucket_exists(Bucket :: atom(), Buckets :: list()) -> boolean().
bucket_exists(Bucket, Buckets) when is_atom(Bucket) ->
    lists:member(Bucket, Buckets).

%% @doc Create a bucket
%% @private
%%
%% If bucket with the same name already exists an error will be returned.
%% @end
-spec create_bucket(BucketExists :: boolean(), Bucket :: atom(), Client :: client_pid(), Prefix :: string(), Buckets :: list()) ->
    {ok | {error, already_exists, string()}, NewBuckets :: list()}.
create_bucket(true, Bucket, _Client, _Prefix, Buckets) ->
    {{error, already_exists, "Redis hash " ++ atom_to_list(Bucket) ++ " already exists."}, Buckets};
create_bucket(false, Bucket, Client, Prefix, Buckets) ->
    {ok, _} = eredis:q(Client, ["HSET", bucket_name(Prefix, Bucket), null, null]),
    {ok, [Bucket | Buckets]}.

%% @doc Empty a bucket
%% @private
%%
%% If no such bucket exists, error will be returned.
%% @end
-spec empty_bucket(BucketExists :: boolean(), Bucket :: atom(), Client :: client_pid(), Prefix :: string()) ->
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
-spec all_items(BucketExists :: boolean(), Bucket :: atom(), Client :: client_pid(), Prefix :: string()) ->
    [tuple()] |
    {error, bucket_not_found, string()}.
all_items(false, Bucket, _Client, _Prefix) ->
    {error, bucket_not_found, "Redis hash " ++ atom_to_list(Bucket) ++ " does not exist."};
all_items(true, Bucket, Client, Prefix) ->
    case eredis:q(Client, ["HGETALL", bucket_name(Prefix, Bucket)]) of
        {ok, Values} ->
            NullFilter = [<<"null">>],
            NewValues = lists:filter(fun(Elem) ->
                not lists:member(Elem, NullFilter) end, Values), %This removes the initial null key and value
            pairs(NewValues, Bucket);
        {error, Reason} ->
            error_logger:error_msg("Redis error listing all items in bucket ~p: ~s", [Bucket, format_error(Reason)]),
            []
    end.

pairs([A, B | L], Bucket) ->
    Decoded = jsx:decode(B, [return_maps]),
    if
        Bucket == features ->
            Parsed = ldclient_flag:new(Decoded),
            [{A, Parsed} | pairs(L, Bucket)];
        Bucket == segments ->
            Parsed = ldclient_segment:new(Decoded),
            [{A, Parsed} | pairs(L, Bucket)]
    end;
pairs([], _Bucket) -> [].

%% @doc Look up an item by key
%% @private
%%
%% Search for an item by its key.
%% @end
-spec lookup_key(BucketExists :: boolean(), Key :: binary(), Bucket :: atom(), Client :: client_pid(), Prefix :: string()) ->
    [tuple()] |
    {error, bucket_not_found, string()}.
lookup_key(false, _Key, Bucket, _Client, _Prefix) ->
    {error, bucket_not_found, "Redis hash " ++ atom_to_list(Bucket) ++ " does not exist."};
lookup_key(true, Key, Bucket, Client, Prefix) ->
    case eredis:q(Client, ["HGET", bucket_name(Prefix, Bucket), Key]) of
        {ok, Value} ->
            if
                (Value == undefined) -> [];
                true ->
                    Decoded = jsx:decode(Value, [return_maps]),
                    if
                        Bucket == features ->
                            Parsed = ldclient_flag:new(Decoded),
                            [{Key, Parsed}];
                        Bucket == segments ->
                            Parsed = ldclient_segment:new(Decoded),
                            [{Key, Parsed}]
                    end
            end;
        {error, Reason} ->
            error_logger:error_msg("Redis error looking up key ~p in bucket ~p: ~s", [Key, Bucket, format_error(Reason)]),
            []
    end.

%% @doc Upsert key value pairs in bucket
%% @private
%%
%% @end
-spec upsert_items(BucketExists :: boolean(), Items :: #{Key :: binary() => Value :: any()}, Bucket :: atom(), Client :: client_pid(), Prefix :: string()) ->
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
-spec upsert_clean_items(BucketExists :: boolean(), Items :: #{Key :: binary() => Value :: any()}, Bucket :: atom(), Client :: client_pid(), Prefix :: string()) ->
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
-spec delete_key(BucketExists :: boolean(), Key :: binary(), Bucket :: atom(), Client :: client_pid(), Prefix :: string()) ->
    ok |
    {error, bucket_not_found, string()}.
delete_key(false, _Key, Bucket, _Client, _Prefix) ->
    {error, bucket_not_found, "Redis hash " ++ atom_to_list(Bucket) ++ " does not exist."};
delete_key(true, Key, Bucket, Client, Prefix) ->
    {ok, _} = eredis:q(Client, ["HDEL", bucket_name(Prefix, Bucket), Key]),
    ok.

bucket_name(Prefix, Bucket) ->
    lists:concat([Prefix, ":", Bucket]).

%% @doc Format Redis error for logging
%% @private
%%
%% @end
-spec format_error(Reason :: no_connection | binary()) -> string().
format_error(no_connection) -> "no connection to Redis";
format_error(Reason) when is_binary(Reason) -> binary_to_list(Reason).

-spec set_init(Client :: client_pid(), Prefix :: string()) -> ok.
set_init(Client, Prefix) ->
    {ok, _} = eredis:q(Client, ["SET", lists:concat([Prefix, ":$inited"]), ""]),
    ok.

-spec get_init(Client :: client_pid(), Prefix :: string()) -> boolean().
get_init(Client, Prefix) ->
    case eredis:q(Client, ["GET", lists:concat([Prefix, ":$inited"])]) of
        {ok, Value} ->
            case Value of
                undefined -> false;
                _ -> true
            end;
        {error, Reason} ->
            error_logger:error_msg("Redis error getting init flag: ~s", [format_error(Reason)]),
            false
    end.
