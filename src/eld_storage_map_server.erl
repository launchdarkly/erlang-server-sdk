%%-------------------------------------------------------------------
%% @doc `eld_storage_map_server' module
%%
%% This is a simple in-memory implementation of an storage server using Erlang
%% map.
%% @end
%%-------------------------------------------------------------------

-module(eld_storage_map_server).

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

%% Types
-type state() :: #{data => map()}.

%%===================================================================
%% Supervision
%%===================================================================

start_link(WorkerRegName) ->
    gen_server:start_link({local, WorkerRegName}, ?MODULE, [], []).

init([]) ->
    {ok, #{data => #{}}}.

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

%% @doc Put an item key value pair in an existing bucket
%%
%% @end
-spec put(ServerRef :: atom(), Bucket :: atom(), Items :: #{Key :: binary() => Value :: any()}) ->
    ok |
    {error, bucket_not_found, string()}.
put(ServerRef, Bucket, Items) when is_atom(Bucket), is_map(Items) ->
    ok = gen_server:call(ServerRef, {put, Bucket, Items}).

%%===================================================================
%% Behavior callbacks
%%===================================================================

-type from() :: {pid(), term()}.
-spec handle_call(term(), from(), state()) -> {reply, term(), state()}.
handle_call({create, Bucket}, _From, #{data := Data} = State) ->
    case create_bucket(Bucket, Data) of
        {error, already_exists, Error} ->
            {reply, {error, already_exists, Error}, State};
        {ok, NewData} ->
            {reply, ok, maps:update(data, NewData, State)}
    end;
handle_call({empty, Bucket}, _From, #{data := Data} = State) ->
    case empty_bucket(Bucket, Data) of
        {error, bucket_not_found, Error} ->
            {reply, {error, bucket_not_found, Error}, State};
        {ok, NewData} ->
            {reply, ok, maps:update(data, NewData, State)}
    end;
handle_call({get, Bucket, Key}, _From, #{data := Data} = State) ->
    {reply, lookup_key(Key, Bucket, Data), State};
handle_call({list, Bucket}, _From, #{data := Data} = State) ->
    {reply, list_items(Bucket, Data), State};
handle_call({put, Bucket, Items}, _From, #{data := Data} = State) ->
    case put_items(Items, Bucket, Data) of
        {error, bucket_not_found, Error} ->
            {reply, {error, bucket_not_found, Error}, State};
        {ok, NewData} ->
            {reply, ok, maps:update(data, NewData, State)}
    end.

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
            {error, already_exists, "ETS table " ++ atom_to_list(Bucket) ++ " already exists."}
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
            {error, bucket_not_found, "ETS table " ++ atom_to_list(Bucket) ++ " does not exist."};
        _ ->
            {ok, maps:update(Bucket, #{}, Data)}
    end.

%% @doc List all items in a bucket
%% @private
%%
%% Returns all items stored in the bucket.
%% @end
-spec list_items(Bucket :: atom(), Data :: map()) ->
    [tuple()] |
    {error, bucket_not_found, string()}.
list_items(Bucket, Data) when is_atom(Bucket) ->
    case bucket_exists(Bucket, Data) of
        false ->
            {error, bucket_not_found, "ETS table " ++ atom_to_list(Bucket) ++ " does not exist."};
        _ ->
            maps:to_list(maps:get(Bucket, Data))
    end.

%% @doc Look up an item by key
%% @private
%%
%% Search for an item by its key.
%% @end
-spec lookup_key(Key :: binary(), Bucket :: atom(), Data :: map()) ->
    [tuple()] |
    {error, bucket_not_found, string()}.
lookup_key(Key, Bucket, Data) when is_atom(Bucket), is_binary(Key) ->
    case bucket_exists(Bucket, Data) of
        false ->
            {error, bucket_not_found, "ETS table " ++ atom_to_list(Bucket) ++ " does not exist."};
        _ ->
            BucketMap = maps:get(Bucket, Data),
            try maps:get(Key, BucketMap) of
                Value -> [{Key, Value}]
            catch
                error:{badkey, Key} -> []
            end
    end.

%% @doc Put a key value pair in bucket
%% @private
%%
%% @end
-spec put_items(Items :: #{Key :: binary() => Value :: any()}, Bucket :: atom(), Data :: map()) ->
    {ok, NewData :: map()} |
    {error, bucket_not_found, string()}.
put_items(Items, Bucket, Data) when is_map(Items), is_atom(Bucket) ->
    case bucket_exists(Bucket, Data) of
        false ->
            {error, bucket_not_found, "ETS table " ++ atom_to_list(Bucket) ++ " does not exist."};
        _ ->
            BucketData = maps:get(Bucket, Data),
            NewBucketData = maps:merge(BucketData, Items),
            {ok, maps:update(Bucket, NewBucketData, Data)}
    end.
