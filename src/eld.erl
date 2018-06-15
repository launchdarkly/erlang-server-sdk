%%%-------------------------------------------------------------------
%%% @doc `eld' module
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(eld).

% API
-export([start_client/4]).
-export([stop_all_clients/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts client process and listens for server sent events
%%
%% @end
-spec start_client(Host :: string(), Port :: pos_integer(), Path :: string(), SdkKey :: string()) ->
    ok | {error, atom(), term()}.
start_client(Host, Port, Path, SdkKey) ->
    {ok, Pid} = supervisor:start_child(eld_stream_sup, [SdkKey]),
    ok = eld_stream_server:listen(Pid, Host, Port, Path).

%% @doc Terminates all current stream listeners
%%
%% @end
-spec stop_all_clients() -> ok.
stop_all_clients() ->
    Pids = [Pid || {_, Pid, worker, _} <- supervisor:which_children(eld_stream_sup)],
    ok = stop_all_clients(Pids).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Recursively terminates processes of given children Pids
%% @private
%%
%% @end
-spec stop_all_clients([pid()]) -> ok.
stop_all_clients([]) ->
    ok;
stop_all_clients([Pid|Rest]) ->
    ok = supervisor:terminate_child(eld_stream_sup, Pid),
    stop_all_clients(Rest).
