%%-------------------------------------------------------------------
%% @doc Event supervisor
%%
%% @end
%%-------------------------------------------------------------------

-module(ldclient_event_sup).

-behaviour(supervisor).

%% Supervision
-export([start_link/2, init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

%%===================================================================
%% Supervision
%%===================================================================

-spec start_link(SupName :: atom(), Tag :: atom()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(SupName, Tag) ->
    error_logger:info_msg("Starting event supervisor for ~p with name ~p", [Tag, SupName]),
    supervisor:start_link({local, SupName}, ?MODULE, [Tag]).

-spec init(Args :: term()) ->
    {ok, {{supervisor:strategy(), non_neg_integer(), pos_integer()}, [supervisor:child_spec()]}}.
init([Tag]) ->
    {ok, {{one_for_one, 1, 5}, children(Tag)}}.

%%===================================================================
%% Internal functions
%%===================================================================

-spec children(Tag :: atom()) -> [supervisor:child_spec()].
children(Tag) ->
    UserCacheName = ldclient_user_cache:get_local_reg_name(Tag),
    UserKeysCapacity = ldclient_settings:get_value(Tag, user_keys_capacity),
    UserCacheWorker = ?CHILD(lru, lru, [{local, UserCacheName}, UserKeysCapacity, []], worker),
    EventStorageWorker = ?CHILD(ldclient_event_server, ldclient_event_server, [Tag], worker),
    EventProcessWorker = ?CHILD(ldclient_event_process_server, ldclient_event_process_server, [Tag], worker),
    [UserCacheWorker, EventStorageWorker, EventProcessWorker].
