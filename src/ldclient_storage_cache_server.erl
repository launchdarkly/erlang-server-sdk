%%-------------------------------------------------------------------
%% @doc `ldclient_storage_cache_server' module
%%
%% This is a simple in-memory implementation of an storage cache server using Erlang
%% map.
%% @end
%%-------------------------------------------------------------------

-module(ldclient_storage_cache_server).

-behaviour(gen_server).

%% Supervision
-export([start_link/2, init/1]).

%% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% API
-export([create/2]).
-export([empty/2]).
-export([get/3]).
-export([upsert/3]).
-export([upsert_clean/3]).
-export([delete/3]).

%% Types
-type state() :: #{
    data => map(),
    cache_ttl => integer()
}.

%%===================================================================
%% Supervision
%%===================================================================

start_link(WorkerRegName, Tag) ->
    error_logger:info_msg("Starting map server with name ~p", [WorkerRegName]),
    gen_server:start_link({local, WorkerRegName}, ?MODULE, [Tag], []).

init([Tag]) ->
    CacheTtl = ldclient_config:get_value(Tag, cache_ttl),
    {ok, #{data => #{}, cache_ttl => CacheTtl}}.

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
    {[{Key :: binary(), Value :: any()}], Hit :: boolean()} |
    {error, bucket_not_found, string()}.
get(ServerRef, Bucket, Key) when is_atom(Bucket), is_binary(Key) ->
    gen_server:call(ServerRef, {get, Bucket, Key}).

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
handle_call({create, Bucket}, _From, #{data := Data, cache_ttl := _CacheTtl} = State) ->
    case create_bucket(Bucket, Data) of
        {error, already_exists, Error} ->
            {reply, {error, already_exists, Error}, State};
        {ok, NewData} ->
            {reply, ok, maps:update(data, NewData, State)}
    end;
handle_call({empty, Bucket}, _From, #{data := Data, cache_ttl := _CacheTtl} = State) ->
    case empty_bucket(Bucket, Data) of
        {error, bucket_not_found, Error} ->
            {reply, {error, bucket_not_found, Error}, State};
        {ok, NewData} ->
            {reply, ok, maps:update(data, NewData, State)}
    end;
handle_call({get, Bucket, Key}, _From, #{data := Data, cache_ttl := CacheTtl} = State) ->
    {reply, lookup_key(Key, Bucket, Data, CacheTtl), State};
handle_call({upsert, Bucket, Items}, _From, #{data := Data, cache_ttl := _CacheTtl} = State) ->
    case upsert_items(Items, Bucket, Data) of
        {error, bucket_not_found, Error} ->
            {reply, {error, bucket_not_found, Error}, State};
        {ok, NewData} ->
            {reply, ok, maps:update(data, NewData, State)}
    end;
handle_call({upsert_clean, Bucket, Items}, _From, #{data := Data, cache_ttl := _CacheTtl} = State) ->
    case upsert_clean_items(Items, Bucket, Data) of
        {error, bucket_not_found, Error} ->
            {reply, {error, bucket_not_found, Error}, State};
        {ok, NewData} ->
            {reply, ok, maps:update(data, NewData, State)}
    end;
handle_call({delete, Bucket, Key}, _From, #{data := Data} = State) ->
    {reply, delete_key(Key, Bucket, Data), State}.


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
-spec bucket_exists(Bucket :: atom(), Data :: map()) -> boolean().
bucket_exists(Bucket, Data) when is_atom(Bucket) ->
    maps:is_key(Bucket, Data).

%% @doc Create a bucket
%% @private
%%
%% If bucket with the same name already exists an error will be returned.
%% @end
-spec create_bucket(Bucket :: atom(), Data :: map()) ->
    {ok, NewData :: map()} |
    {error, already_exists, string()}.
create_bucket(Bucket, Data) when is_atom(Bucket) ->
    case bucket_exists(Bucket, Data) of
        false ->
            {ok, maps:put(Bucket, #{}, Data)};
        _ ->
            {error, already_exists, "Map " ++ atom_to_list(Bucket) ++ " already exists."}
    end.

%% @doc Empty a bucket
%% @private
%%
%% If no such bucket exists, error will be returned.
%% @end
-spec empty_bucket(Bucket :: atom(), Data :: map()) ->
    {ok, NewData :: map()} |
    {error, bucket_not_found, string()}.
empty_bucket(Bucket, Data) when is_atom(Bucket) ->
    case bucket_exists(Bucket, Data) of
        false ->
            {error, bucket_not_found, "Map " ++ atom_to_list(Bucket) ++ " does not exist."};
        _ ->
            {ok, maps:update(Bucket, #{}, Data)}
    end.

%% @doc Look up an item by key
%% @private
%%
%% Search for an item by its key.
%% @end
-spec lookup_key(Key :: binary(), Bucket :: atom(), Data :: map(), CacheTtl :: integer()) ->
    {[tuple()], boolean()} |
    {error, bucket_not_found, string()}.
lookup_key(Key, Bucket, Data, CacheTtl) when is_atom(Bucket), is_binary(Key) ->
    case bucket_exists(Bucket, Data) of
        false ->
            {error, bucket_not_found, "Map " ++ atom_to_list(Bucket) ++ " does not exist."};
        _ ->
            BucketMap = maps:get(Bucket, Data),
            try maps:get(Key, BucketMap) of
                {Value, Timestamp} ->
                    TimeCheck = Timestamp + CacheTtl,
                    CurrentTime = os:system_time(seconds),
                    if 
                        CacheTtl == 0 -> % testing mode for underlying persistent storage layer
                            {[{}], false};
                        CacheTtl < 0 -> % infinite ttl
                            Atoms = parse_json_to_flag(Bucket, Key, Value),
                            {Atoms, true};
                        TimeCheck > CurrentTime ->
                            Atoms = parse_json_to_flag(Bucket, Key, Value),
                            {Atoms, true};
                        true ->
                            {[{}], false}
                    end
            catch
                error:{badkey, Key} ->
                    {[{}], false}
            end
    end.

parse_json_to_flag(Bucket, Key, Value) ->
    Decoded = jsx:decode(Value, [return_maps]),
    if 
        Bucket == features ->
            Parsed = ldclient_flag:new(Decoded),
            [{Key, Parsed}];
        Bucket == segments ->
            Parsed = ldclient_segment:new(Decoded),
            [{Key, Parsed}]
    end.

%% @doc Upsert key value pairs in bucket
%% @private
%%
%% @end
-spec upsert_items(Items :: #{Key :: binary() => Value :: any()}, Bucket :: atom(), Data :: map()) ->
    {ok, NewData :: map()} |
    {error, bucket_not_found, string()}.
upsert_items(Items, _, Data) when map_size(Items) == 0 -> {ok, Data};
upsert_items(Items, Bucket, Data) when is_map(Items), is_atom(Bucket) ->
    case bucket_exists(Bucket, Data) of
        false ->
            {error, bucket_not_found, "Map " ++ atom_to_list(Bucket) ++ " does not exist."};
        _ ->
            TimestampedItems = maps:map(
                fun(_, V) -> {jsx:encode(V), os:system_time(seconds)} end, Items),
            BucketMap = maps:get(Bucket, Data),
            NewBucketData = maps:merge(BucketMap, TimestampedItems),
            {ok, maps:update(Bucket, NewBucketData, Data)}
    end.

%% @doc Empty bucket and upsert key value pairs
%% @private
%%
%% @end
-spec upsert_clean_items(Items :: #{Key :: binary() => Value :: any()}, Bucket :: atom(), Data :: map()) ->
    {ok, NewData :: map()} |
    {error, bucket_not_found, string()}.
upsert_clean_items(Items, _, Data) when map_size(Items) == 0 -> {ok, Data};
upsert_clean_items(Items, Bucket, Data) when is_map(Items), is_atom(Bucket) ->
    case bucket_exists(Bucket, Data) of
        false ->
            {error, bucket_not_found, "Map " ++ atom_to_list(Bucket) ++ " does not exist."};
        _ ->
            TimestampedItems = maps:map(
                fun(_, V) -> {jsx:encode(V), os:system_time(seconds)} end, Items),
            {ok, maps:update(Bucket, TimestampedItems, Data)}
    end.

%% @doc Delete an item by key
%% @private
%%
%% Delete an item by its key.
%% @end
-spec delete_key(Key :: binary(), Bucket :: atom(), Data :: map()) ->
    ok |
    {error, bucket_not_found, string()}.
delete_key(Key, Bucket, Data) when is_atom(Bucket), is_binary(Key) ->
    case bucket_exists(Bucket, Data) of
        false ->
            {error, bucket_not_found, "Map " ++ atom_to_list(Bucket) ++ " does not exist."};
        _ ->
            BucketMap = maps:get(Bucket, Data),
            {ok, maps:remove(Key, BucketMap)}
    end.
