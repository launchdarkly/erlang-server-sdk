%%%-------------------------------------------------------------------
%%% @doc Top level application supervisor
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(eld_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args}, permanent, 5000, Type, [Module]}).

%%%===================================================================
%%% Supervision
%%%===================================================================

-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init(Args :: term()) ->
    {ok, {{supervisor:strategy(), non_neg_integer(), pos_integer()}, [supervisor:child_spec()]}}.
init([]) ->
    {ok, {{one_for_one, 1, 5}, children()}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec children() -> [supervisor:child_spec()].
children() ->
    StorageSup = ?CHILD(eld_storage_sup, eld_storage_sup, [], supervisor),
   [StorageSup].
