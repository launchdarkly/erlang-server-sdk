%%-------------------------------------------------------------------
%% @doc Instance supervisor
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_instance_sup).

-behaviour(supervisor).

%% Supervision
-export([start_link/5, init/1, child_spec/1, child_spec/2]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

child_spec(Args) -> child_spec(?MODULE, Args).
child_spec(Id, Args) ->
    #{
        id => Id,
        start => {?MODULE, start_link, Args},
        restart => permanent,
        shutdown => 5000, % shutdown time
        type => supervisor,
        modules => [?MODULE]
    }.
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
    % Allow up to 10 restarts in 60 seconds before giving up.
    % This provides resilience for transient failures in child supervisors
    % (update processor, events, storage) while preventing infinite restart loops.
    {ok, {{one_for_one, 10, 60}, children(UpdateSupName, UpdateWorkerModule, EventSupName, Tag)}}.

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
    UpdateSup = ?CHILD(ldclient_update_sup, ldclient_update_sup, [UpdateSupName, UpdateWorkerModule], supervisor),
    EventSup = ?CHILD(ldclient_event_sup, ldclient_event_sup, [EventSupName, Tag], supervisor),
    [UpdateSup, EventSup].
