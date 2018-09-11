%%%-------------------------------------------------------------------
%%% @doc `eld_storage_ets_sup' module
%%%
%%% This is a supervisor for ETS storage worker.
%%% @end
%%%-------------------------------------------------------------------

-module(eld_storage_ets_sup).

-behaviour(supervisor).

%% Supervision
-export([start_link/0, init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

%%%===================================================================
%%% Supervision
%%%===================================================================

-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init(Args :: term()) ->
    {ok, {{supervisor:strategy(), non_neg_integer(), pos_integer()}, [supervisor:child_spec()]}}.
init([]) ->
    {ok, {{one_for_one, 0, 1}, children()}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec children() -> [supervisor:child_spec()].
children() ->
    FlagStorageServer = ?CHILD(eld_storage_ets_server, eld_storage_ets_server, [], worker),
    [FlagStorageServer].
