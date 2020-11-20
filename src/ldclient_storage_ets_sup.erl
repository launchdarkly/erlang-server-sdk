%%-------------------------------------------------------------------
%% @doc `ldclient_storage_ets_sup' module
%% @private
%% This is a supervisor for ETS storage worker.
%% @end
%%-------------------------------------------------------------------

-module(ldclient_storage_ets_sup).

-behaviour(supervisor).

%% Supervision
-export([start_link/2, init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

%%===================================================================
%% Supervision
%%===================================================================

-spec start_link(SupRegName :: atom(), WorkerRegName :: atom()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(SupRegName, WorkerRegName) when is_atom(SupRegName), is_atom(WorkerRegName) ->
    supervisor:start_link({local, SupRegName}, ?MODULE, [WorkerRegName]).

-spec init(Args :: term()) ->
    {ok, {{supervisor:strategy(), non_neg_integer(), pos_integer()}, [supervisor:child_spec()]}}.
init([WorkerRegName]) ->
    {ok, {{one_for_one, 0, 1}, children(WorkerRegName)}}.

%%===================================================================
%% Internal functions
%%===================================================================

-spec children(WorkerRegName :: atom()) -> [supervisor:child_spec()].
children(WorkerRegName) ->
    FlagStorageServer = ?CHILD(ldclient_storage_ets_server, ldclient_storage_ets_server, [WorkerRegName], worker),
    [FlagStorageServer].
