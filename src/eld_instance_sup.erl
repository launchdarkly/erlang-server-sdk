%%-------------------------------------------------------------------
%% @doc Instance supervisor
%%
%% @end
%%-------------------------------------------------------------------

-module(eld_instance_sup).

-behaviour(supervisor).

%% Supervision
-export([start_link/5, init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

%%===================================================================
%% Supervision
%%===================================================================

-spec start_link(
    SupName :: atom(),
    UpdateSupName :: atom(),
    UpdateWorkerModule :: atom(),
    EventSupName :: atom(),
    Tag :: atom()
) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(SupName, UpdateSupName, UpdateWorkerModule, EventSupName, Tag) ->
    error_logger:info_msg("Starting instance supervisor for ~p with name ~p", [Tag, SupName]),
    supervisor:start_link({local, SupName}, ?MODULE, [UpdateSupName, UpdateWorkerModule, EventSupName, Tag]).

-spec init(Args :: term()) ->
    {ok, {{supervisor:strategy(), non_neg_integer(), pos_integer()}, [supervisor:child_spec()]}}.
init([UpdateSupName, UpdateWorkerModule, EventSupName, Tag]) ->
    {ok, {{one_for_one, 1, 5}, children(UpdateSupName, UpdateWorkerModule, EventSupName, Tag)}}.

%%===================================================================
%% Internal functions
%%===================================================================

-spec children(
    UpdateSupName :: atom(),
    UpdateWorkerModule :: atom(),
    EventSupName :: atom(),
    Tag :: atom()
) -> [supervisor:child_spec()].
children(UpdateSupName, UpdateWorkerModule, EventSupName, Tag) ->
    UpdateSup = ?CHILD(eld_update_sup, eld_update_sup, [UpdateSupName, UpdateWorkerModule], supervisor),
    EventSup = ?CHILD(eld_event_sup, eld_event_sup, [EventSupName, Tag], supervisor),
    [UpdateSup, EventSup].
