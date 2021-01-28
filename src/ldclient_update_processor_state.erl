%%-------------------------------------------------------------------
%% @doc `ldclient_update_processor_state' module
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_update_processor_state).

% API
-export([init/0]).
-export([create_initialized_state/2]).
-export([delete_initialized_state/1]).
-export([set_initialized_state/2]).
-export([get_initialized_state/1]).
-export([create_storage_initialized_state/2]).
-export([delete_storage_initialized_state/1]).
-export([set_storage_initialized_state/2]).
-export([get_storage_initialized_state/1]).

-define(UPDATE_TABLE, ldclient_update_processor_initialization).

%%===================================================================
%% API
%%===================================================================

%% @doc Initialize update processor ETS table
%%
%% Initializes an empty ETS table for initialization state of update processors.
%% @end
-spec init() -> ok.
init() ->
    _UTID = ets:new(?UPDATE_TABLE, [set, public, named_table, {read_concurrency, true}]),
    ok.

%% @doc Create a state for a new processor
%%
%% @end
-spec create_initialized_state(Tag :: atom(), Value :: boolean()) -> boolean().
create_initialized_state(Tag, Value) ->
    ets:insert_new(?UPDATE_TABLE, {Tag, Value}).

%% @doc Delete initialized state for a processor
%%
%% @end
-spec delete_initialized_state(Tag :: atom()) -> boolean().
delete_initialized_state(Tag) ->
    ets:delete(?UPDATE_TABLE, Tag).

%% @doc Change the update processor status for a given Tag
%%
%% @end
-spec set_initialized_state(Tag :: atom(), Value :: boolean()) -> boolean().
set_initialized_state(Tag, Value) ->
    ets:insert(?UPDATE_TABLE, {Tag, Value}).

%% @doc Return the update processor status for a given Tag
%%
%% @end
-spec get_initialized_state(Tag :: atom()) -> boolean().
get_initialized_state(Tag) ->
    [{_Tag, Initialized}] = ets:lookup(?UPDATE_TABLE, Tag),
    Initialized.

%% @doc Create a state for a new storage server
%%
%% @end
-spec create_storage_initialized_state(Tag :: atom(), Value :: atom()) -> boolean().
create_storage_initialized_state(Tag, Value) ->
    ets:insert_new(?UPDATE_TABLE, {get_storage_name(Tag), Value}).

%% @doc Delete initialized state for a storage server
%%
%% @end
-spec delete_storage_initialized_state(Tag :: atom()) -> boolean().
delete_storage_initialized_state(Tag) ->
    ets:delete(?UPDATE_TABLE, get_storage_name(Tag)).

%% @doc Change the storage server status for a given Tag
%%
%% @end
-spec set_storage_initialized_state(Tag :: atom(), Value :: atom()) -> boolean().
set_storage_initialized_state(Tag, Value) ->
    ets:insert(?UPDATE_TABLE, {get_storage_name(Tag), Value}).

%% @doc Return the storage server status for a given Tag
%%
%% @end
-spec get_storage_initialized_state(Tag :: atom()) -> atom().
get_storage_initialized_state(Tag) ->
    [{_Tag, Initialized}] = ets:lookup(?UPDATE_TABLE, get_storage_name(Tag)),
    Initialized.

-spec get_storage_name(Tag :: atom()) -> atom().
get_storage_name(Tag) ->
	list_to_atom(atom_to_list(Tag) ++ atom_to_list(ldclient_config:get_value(Tag, feature_store))).