%%-------------------------------------------------------------------
%% @doc `eld_updater' module
%%
%% Used to start and stop client stream listener.
%% @end
%%-------------------------------------------------------------------

-module(eld_updater).

%% API
-export([start/3]).
-export([stop/1]).

%% Constants
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

%%===================================================================
%% API
%%===================================================================

%% @doc Start client stream listener
%%
%% @end
-spec start(UpdateSupName :: atom(), UpdateWorkerModule :: atom(), Tag :: atom()) ->
    ok
    | {error, gun_open_failed, term()}
    | {error, gun_open_timeout, term()}
    | {error, get_request_failed, term()}.
start(UpdateSupName, UpdateWorkerModule, Tag) when is_atom(UpdateSupName), is_atom(UpdateWorkerModule) ->
    {ok, Pid} = supervisor:start_child(UpdateSupName, [Tag]),
    UpdateWorkerModule:listen(Pid).

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
