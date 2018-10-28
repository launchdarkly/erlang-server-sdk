%%-------------------------------------------------------------------
%% @doc `eld_storage_map' module
%%
%% Provides implementation of storage backend using Erlang map.
%% @end
%%-------------------------------------------------------------------

-module(eld_storage_map).

-behaviour(eld_storage_engine).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

%% Behavior callbacks
-export([init/1]).
-export([create/1]).
-export([empty/1]).
-export([get/2]).
-export([list/1]).
-export([put/2]).
-export([terminate/0]).

%%===================================================================
%% Behavior callbacks
%%===================================================================

-spec init(Options :: list()) -> ok.
init(_) ->
    StorageSup = ?CHILD(eld_storage_map_sup, eld_storage_map_sup, [], supervisor),
    {ok, _} = supervisor:start_child(eld_sup, StorageSup),
    % Pre-create flags and segments buckets
    ok = create(flags),
    ok = create(segments).

-spec create(Bucket :: atom()) ->
    ok |
    {error, already_exists, string()}.
create(Bucket) ->
    eld_storage_map_server:create(Bucket).

-spec empty(Bucket :: atom()) ->
    ok |
    {error, bucket_not_found, string()}.
empty(Bucket) ->
    eld_storage_map_server:empty(Bucket).

-spec get(Bucket :: atom(), Key :: binary()) ->
    [{Key :: binary(), Value :: any()}] |
    {error, bucket_not_found, string()}.
get(Bucket, Key) ->
    eld_storage_map_server:get(Bucket, Key).

-spec list(Bucket :: atom()) ->
    [{Key :: binary(), Value :: any()}] |
    {error, bucket_not_found, string()}.
list(Bucket) ->
    eld_storage_map_server:list(Bucket).

-spec put(Bucket :: atom(), Items :: #{Key :: binary() => Value :: any()}) ->
    ok |
    {error, bucket_not_found, string()}.
put(Bucket, Items) ->
    eld_storage_map_server:put(Bucket, Items).

-spec terminate() -> ok.
terminate() ->
    supervisor:terminate_child(eld_storage_map_sup, eld_storage_map_server).
