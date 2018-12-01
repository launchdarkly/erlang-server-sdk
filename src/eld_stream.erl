%%-------------------------------------------------------------------
%% @doc `eld_stream' module
%%
%% Used to start and stop client stream listener.
%% @end
%%-------------------------------------------------------------------

-module(eld_stream).

%% API
-export([start/2]).
-export([stop/1]).

%% Constants
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

%%===================================================================
%% API
%%===================================================================

%% @doc Start client stream listener
%%
%% @end
-spec start(StreamSupName :: atom(), Tag :: atom()) ->
    ok
    | {error, gun_open_failed, term()}
    | {error, gun_open_timeout, term()}
    | {error, get_req_failed, term()}.
start(StreamSupName, Tag) when is_atom(StreamSupName) ->
    {ok, Pid} = supervisor:start_child(StreamSupName, [Tag]),
    eld_stream_server:listen(Pid).

%% @doc Stop client stream listener
%%
%% @end
-spec stop(Tag :: atom()) -> ok.
stop(StreamSupName) when is_atom(StreamSupName) ->
    ok = terminate_all_children(StreamSupName).

%%===================================================================
%% Internal functions
%%===================================================================

%% @doc Terminates all children of a given supervisor
%% @private
%%
%% @end
-spec terminate_all_children(Sup :: atom()) -> ok.
terminate_all_children(Sup) ->
    Pids = [Pid || {_, Pid, worker, _} <- supervisor:which_children(Sup)],
    terminate_all_children(Sup, Pids).

%% @doc Recursively terminates processes of given children Pids
%% @private
%%
%% @end
-spec terminate_all_children(Sup :: atom(), [pid()]) -> ok.
terminate_all_children(_Sup, []) ->
    ok;
terminate_all_children(Sup, [Pid|Rest]) ->
    ok = supervisor:terminate_child(Sup, Pid),
    terminate_all_children(Sup, Rest).
