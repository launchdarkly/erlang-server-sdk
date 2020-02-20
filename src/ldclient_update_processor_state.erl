%%-------------------------------------------------------------------
%% @doc `ldclient_update_processor_state' module
%%
%% @end
%%-------------------------------------------------------------------

-module(ldclient_update_processor_state).

-include("ldclient_update_processor_state.hrl").

% API
-export([init/0]).
-export([set_initialized_state/2]).
-export([get_initialized_state/1]).

%%===================================================================
%% API
%%===================================================================

%% @doc Initialize update processor initialization ETS table
%%
%% Initializes an empty ETS table for initialization state of update processors.
%% @end
-spec init() -> ok.
init() ->
    _TID = ets:new(?UPDATE_PROCESSOR_INITIALIZATION_TABLE, [set, public, named_table, {read_concurrency, true}]),
    ok.

%% @doc Change the update processor status for a given Tag
%%
%% @end
-spec set_initialized_state(Tag :: atom(), Value :: boolean()) -> boolean().
set_initialized_state(Tag, Value) ->
    ets:insert(?UPDATE_PROCESSOR_INITIALIZATION_TABLE, {Tag, Value}).

%% @doc Return the update processor status for a given Tag
%%
%% @end
-spec get_initialized_state(Tag :: atom()) -> boolean().
get_initialized_state(Tag) ->
    [{_Tag, Initialized}] = ets:lookup(?UPDATE_PROCESSOR_INITIALIZATION_TABLE, Tag),
    Initialized.
