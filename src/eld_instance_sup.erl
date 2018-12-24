%%-------------------------------------------------------------------
%% @doc Instance supervisor
%%
%% @end
%%-------------------------------------------------------------------

-module(eld_instance_sup).

-behaviour(supervisor).

%% Supervision
-export([start_link/2, init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

%%===================================================================
%% Supervision
%%===================================================================

-spec start_link(SupName :: atom(), StreamSupName :: atom()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(SupName, StreamSupName) ->
    io:format("Starting instance_sup with sup name: ~p~n", [SupName]),
    supervisor:start_link({local, SupName}, ?MODULE, [StreamSupName]).

-spec init(Args :: term()) ->
    {ok, {{supervisor:strategy(), non_neg_integer(), pos_integer()}, [supervisor:child_spec()]}}.
init([StreamSupName]) ->
    {ok, {{one_for_one, 1, 5}, children(StreamSupName)}}.

%%===================================================================
%% Internal functions
%%===================================================================

-spec children(StreamSupName :: atom()) -> [supervisor:child_spec()].
children(StreamSupName) ->
    StreamSup = ?CHILD(eld_stream_sup, eld_stream_sup, [StreamSupName], supervisor),
    [StreamSup].
