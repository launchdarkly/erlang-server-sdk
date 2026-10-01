%%-------------------------------------------------------------------
%% @doc Supervisor for event reporter workers
%%
%% Uses a `simple_one_for_one' strategy so the event server can commission and
%% decommission workers at runtime.
%% @private
%% @end
%%-------------------------------------------------------------------
-module(ldclient_event_worker_sup).

-behaviour(supervisor).

%% Supervision
-export([start_link/2, init/1]).

%% API
-export([start_worker/1, stop_worker/2, stop_all/1, get_sup_name/1]).

%%===================================================================
%% Supervision
%%===================================================================

-spec start_link(SupName :: atom(), Tag :: atom()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(SupName, Tag) ->
    error_logger:info_msg("Starting event worker supervisor for ~p with name ~p", [Tag, SupName]),
    supervisor:start_link({local, SupName}, ?MODULE, [Tag]).

-spec init(Args :: term()) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([_Tag]) ->
    SupFlags = #{strategy => simple_one_for_one, intensity => 1, period => 5},
    Child = #{
        id => ldclient_event_process_server,
        start => {ldclient_event_process_server, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [ldclient_event_process_server]
    },
    {ok, {SupFlags, [Child]}}.

%%===================================================================
%% API
%%===================================================================

-spec start_worker(Tag :: atom()) -> {ok, pid()} | {error, term()}.
start_worker(Tag) ->
    supervisor:start_child(get_sup_name(Tag), [Tag]).

-spec stop_worker(Tag :: atom(), pid()) -> ok | {error, term()}.
stop_worker(Tag, Pid) ->
    supervisor:terminate_child(get_sup_name(Tag), Pid).

%% @doc Terminate all workers for a tag. Used when the event server restarts so
%% that stale workers do not linger.
%% @end
-spec stop_all(Tag :: atom()) -> ok.
stop_all(Tag) ->
    SupName = get_sup_name(Tag),
    case whereis(SupName) of
        undefined ->
            ok;
        _Pid ->
            Children = supervisor:which_children(SupName),
            lists:foreach(
                fun({_Id, ChildPid, _Type, _Modules}) when is_pid(ChildPid) ->
                        _ = supervisor:terminate_child(SupName, ChildPid);
                   (_) ->
                        ok
                end,
                Children
            ),
            ok
    end.

-spec get_sup_name(Tag :: atom()) -> atom().
get_sup_name(Tag) ->
    list_to_atom("ldclient_event_worker_sup_" ++ atom_to_list(Tag)).
