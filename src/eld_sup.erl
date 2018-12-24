%%-------------------------------------------------------------------
%% @doc Top level application supervisor
%%
%% @end
%%-------------------------------------------------------------------

-module(eld_sup).

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
    ChildSpec = {
        eld_instance_sup,
        {eld_instance_sup, start_link, []},
        permanent,
        5000, % shutdown time
        supervisor,
        [eld_instance_sup]
    },
    {ok, {{simple_one_for_one, MaxRestart, MaxTime}, [ChildSpec]}}.
