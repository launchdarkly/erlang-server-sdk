%%-------------------------------------------------------------------
%% @doc `eld_storage_map_sup' module
%%
%% This is a supervisor for map storage worker.
%% @end
%%-------------------------------------------------------------------

-module(eld_storage_map_sup).

-behaviour(supervisor).

%% Supervision
-export([start_link/1, init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

%%===================================================================
%% Supervision
%%===================================================================

-spec start_link(Tag :: atom()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Tag) when is_atom(Tag) ->
    supervisor:start_link({local, get_local_reg_name(Tag)}, ?MODULE, [Tag]).

-spec init(Args :: term()) ->
    {ok, {{supervisor:strategy(), non_neg_integer(), pos_integer()}, [supervisor:child_spec()]}}.
init([Tag]) ->
    {ok, {{one_for_one, 0, 1}, children(Tag)}}.

%%===================================================================
%% Internal functions
%%===================================================================

-spec get_local_reg_name(Tag :: atom()) -> atom().
get_local_reg_name(Tag) ->
    list_to_atom("eld_" ++ atom_to_list(?MODULE) ++ atom_to_list(Tag)).

-spec children(Tag :: atom()) -> [supervisor:child_spec()].
children(Tag) ->
    FlagStorageServer = ?CHILD(eld_storage_map_server, eld_storage_map_server, [Tag], worker),
    [FlagStorageServer].
