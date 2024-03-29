%%-------------------------------------------------------------------
%% @doc Top level application supervisor
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_sup).

-behaviour(supervisor).

%% Supervision
-export([start_link/0, init/1]).

%%===================================================================
%% Supervision
%%===================================================================

-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init(Args :: term()) ->
    {ok, {{supervisor:strategy(), non_neg_integer(), pos_integer()}, [supervisor:child_spec()]}}.
init([]) ->
    MaxRestart = 10,
    MaxTime = 3600,
    {ok, {{one_for_one, MaxRestart, MaxTime}, []}}.
