%%-------------------------------------------------------------------
%% @doc Stream supervisor
%%
%% @end
%%-------------------------------------------------------------------

-module(eld_stream_sup).

-behaviour(supervisor).

%% Supervision
-export([start_link/1, init/1]).

%%===================================================================
%% Supervision
%%===================================================================

-spec start_link(StreamSupName :: atom()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(StreamSupName) ->
    supervisor:start_link({local, StreamSupName}, ?MODULE, []).

-spec init(Args :: term()) ->
    {ok, {{supervisor:strategy(), non_neg_integer(), pos_integer()}, [supervisor:child_spec()]}}.
init([]) ->
    MaxRestart = 10,
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
