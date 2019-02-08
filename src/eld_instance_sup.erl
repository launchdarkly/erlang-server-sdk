%%-------------------------------------------------------------------
%% @doc Instance supervisor
%%
%% @end
%%-------------------------------------------------------------------

-module(eld_instance_sup).

-behaviour(supervisor).

%% Supervision
-export([start_link/4, init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

%%===================================================================
%% Supervision
%%===================================================================

-spec start_link(
    SupName :: atom(),
    StreamSupName :: atom(),
    EventSupName :: atom(),
    Tag :: atom()
) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(SupName, StreamSupName, EventSupName, Tag) ->
    io:format("Starting instance_sup with sup name: ~p~n", [SupName]),
    supervisor:start_link({local, SupName}, ?MODULE, [StreamSupName, EventSupName, Tag]).

-spec init(Args :: term()) ->
    {ok, {{supervisor:strategy(), non_neg_integer(), pos_integer()}, [supervisor:child_spec()]}}.
init([StreamSupName, EventSupName, Tag]) ->
    {ok, {{one_for_one, 1, 5}, children(StreamSupName, EventSupName, Tag)}}.

%%===================================================================
%% Internal functions
%%===================================================================

-spec children(
    StreamSupName :: atom(),
    EventSupName :: atom(),
    Tag :: atom()
) -> [supervisor:child_spec()].
children(StreamSupName, EventSupName, Tag) ->
    StreamSup = ?CHILD(eld_stream_sup, eld_stream_sup, [StreamSupName], supervisor),
    EventSup = ?CHILD(eld_event_sup, eld_event_sup, [EventSupName, Tag], supervisor),
    [StreamSup, EventSup].
