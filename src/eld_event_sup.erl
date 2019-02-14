%%-------------------------------------------------------------------
%% @doc Event supervisor
%%
%% @end
%%-------------------------------------------------------------------

-module(eld_event_sup).

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
    io:format("Starting event_sup with sup name: ~p~n", [SupName]),
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
    EventStorageWorker = ?CHILD(eld_event_server, eld_event_server, [Tag], worker),
    EventDispatchWorker = ?CHILD(eld_event_dispatch_server, eld_event_dispatch_server, [Tag], worker),
    [EventStorageWorker, EventDispatchWorker].
