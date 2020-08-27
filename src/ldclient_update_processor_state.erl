%%-------------------------------------------------------------------
%% @doc `ldclient_update_processor_state' module
%%
%% @end
%%-------------------------------------------------------------------

-module(ldclient_update_processor_state).

% API
-export([init/0]).
-export([create_initialized_state/2]).
-export([delete_initialized_state/1]).
-export([set_initialized_state/2]).
-export([get_initialized_state/1]).

-define(ETS_TABLE, ldclient_update_processor_initialization).

%%===================================================================
%% API
%%===================================================================

%% @doc Initialize update processor initialization ETS table
%%
%% Initializes an empty ETS table for initialization state of update processors.
%% @end
-spec init() -> ok.
init() ->
    _TID = ets:new(?ETS_TABLE, [set, public, named_table, {read_concurrency, true}]),
    ok.

%% @doc Create a state for a new processor
%%
%% @end
-spec create_initialized_state(Tag :: atom(), Value :: boolean()) -> boolean().
create_initialized_state(Tag, Value) ->
    ets:insert_new(?ETS_TABLE, {Tag, Value}).

%% @doc Delete initialized state for a processor
%%
%% @end
-spec delete_initialized_state(Tag :: atom()) -> boolean().
delete_initialized_state(Tag) ->
    ets:delete(?ETS_TABLE, Tag).

%% @doc Change the update processor status for a given Tag
%%
%% @end
-spec set_initialized_state(Tag :: atom(), Value :: boolean()) -> boolean().
set_initialized_state(Tag, Value) ->
    ets:insert(?ETS_TABLE, {Tag, Value}).

%% @doc Return the update processor status for a given Tag
%%
%% @end
-spec get_initialized_state(Tag :: atom()) -> boolean().
get_initialized_state(Tag) ->
    [{_Tag, Initialized}] = ets:lookup(?ETS_TABLE, Tag),
    Initialized.
