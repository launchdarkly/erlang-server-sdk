%%%-------------------------------------------------------------------
%%% @doc Stream supervisor
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(eld_stream_sup).

-behaviour(supervisor).

%% Supervision
-export([start_link/0, init/1]).

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
    MaxRestart = 1,
    MaxTime = 3600,
    ChildSpec = {
        eld_stream_server,
        {eld_stream_server, start_link, []},
        permanent,
        5000, % shutdown time
        worker,
        [eld_stream_server]
    },
    {ok, {{simple_one_for_one, MaxRestart, MaxTime}, [ChildSpec]}}.
