%%-------------------------------------------------------------------
%% @doc Update processor supervisor
%%
%% @end
%%-------------------------------------------------------------------

-module(eld_update_sup).

-behaviour(supervisor).

%% Supervision
-export([start_link/2, init/1]).

%%===================================================================
%% Supervision
%%===================================================================

-spec start_link(UpdateSupName :: atom(), UpdateWorkerModule :: atom()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(UpdateSupName, UpdateWorkerModule) ->
    supervisor:start_link({local, UpdateSupName}, ?MODULE, [UpdateWorkerModule]).

-spec init(Args :: term()) ->
    {ok, {{supervisor:strategy(), non_neg_integer(), pos_integer()}, [supervisor:child_spec()]}}.
init([UpdateWorkerModule]) ->
    MaxRestart = 10,
    MaxTime = 3600,
    ChildSpec = {
        UpdateWorkerModule,
        {UpdateWorkerModule, start_link, []},
        permanent,
        5000, % shutdown time
        worker,
        [UpdateWorkerModule]
    },
    {ok, {{simple_one_for_one, MaxRestart, MaxTime}, [ChildSpec]}}.
