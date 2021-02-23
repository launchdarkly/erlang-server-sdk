%%-------------------------------------------------------------------
%% @doc `ldclient_storage_cache_sup' module
%%
%% This is a supervisor for cache storage worker.
%% @end
%%-------------------------------------------------------------------

-module(ldclient_storage_cache_sup).

-behaviour(supervisor).

%% Supervision
-export([start_link/3, init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

%%===================================================================
%% Supervision
%%===================================================================

-spec start_link(SupRegName :: atom(), WorkerRegName :: atom(), Tag :: atom()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(SupRegName, WorkerRegName, Tag) when is_atom(SupRegName), is_atom(WorkerRegName), is_atom(Tag) ->
    supervisor:start_link({local, SupRegName}, ?MODULE, [WorkerRegName, Tag]).

-spec init(Args :: term()) ->
    {ok, {{supervisor:strategy(), non_neg_integer(), pos_integer()}, [supervisor:child_spec()]}}.
init([WorkerRegName, Tag]) ->
    {ok, {{one_for_one, 0, 1}, children(WorkerRegName, Tag)}}.

%%===================================================================
%% Internal functions
%%===================================================================

-spec children(WorkerRegName :: atom(), Tag :: atom()) -> [supervisor:child_spec()].
children(WorkerRegName, Tag) ->
    FeatureStorageServer = ?CHILD(ldclient_storage_cache_server, ldclient_storage_cache_server, [WorkerRegName, Tag], worker),
    [FeatureStorageServer].
