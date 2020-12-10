%%-------------------------------------------------------------------
%% @doc `ldclient_instance' module
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_instance).

%% API
-export([start/3]).
-export([stop/1]).
-export([stop_all/0]).
-export([update_processor_initialized/1]).

-type options() :: #{
    base_uri => string(),
    stream_uri => string(),
    storage_backend => atom(),
    events_capacity => pos_integer(),
    events_flush_interval => pos_integer(),
    events_dispatcher => atom(),
    user_keys_capacity => pos_integer(),
    inline_users_in_events => boolean(),
    private_attributes => ldclient_settings:private_attributes(),
    stream => boolean(),
    polling_interval => pos_integer(),
    polling_update_requestor => atom(),
    offline => boolean()
}.
%% Options for starting an SDK instance

-export_type([options/0]).

%%===================================================================
%% API
%%===================================================================

%% @doc Start a client instance
%%
%% @end
-spec start(Tag :: atom(), SdkKey :: string(), Options :: options()) ->
    ok | {error, already_started, term()}.
start(Tag, SdkKey, Options) ->
    % TODO check if Tag already exists and return already_started error
    % Parse options into settings
    Settings = ldclient_settings:parse_options(SdkKey, Options),
    ok = ldclient_settings:register(Tag, Settings),
    % Start instance supervisor
    SupName = get_ref_from_tag(instance, Tag),
    StartStream = maps:get(stream, Settings),
    UpdateSupName = get_ref_from_tag(instance_stream, Tag),
    UpdateWorkerModule = get_update_processor(StartStream, maps:get(offline, Settings)),
    EventsSupName = get_ref_from_tag(instance_events, Tag),
    {ok, _} = supervisor:start_child(ldclient_sup, [SupName, UpdateSupName, UpdateWorkerModule, EventsSupName, Tag]),
    % Start storage backend
    true = ldclient_update_processor_state:create_storage_initialized_state(Tag, false),
    StorageBackend = maps:get(storage_backend, Settings),
    ok = StorageBackend:init(SupName, Tag, []),
    % Start stream client
    true = ldclient_update_processor_state:create_initialized_state(Tag, false),
    ok = start_updater(UpdateSupName, UpdateWorkerModule, Tag).

%% @doc Stop a client instance
%%
%% @end
-spec stop(Tag :: atom()) -> ok.
stop(Tag) when is_atom(Tag) ->
    % TODO only stop stream instance if it's running
    % Terminate stream
    StreamSupName = get_ref_from_tag(instance_stream, Tag),
    ok = ldclient_updater:stop(StreamSupName),
    % Terminate storage
    StorageBackend = ldclient_settings:get_value(Tag, storage_backend),
    ok = StorageBackend:terminate(Tag),
    % Terminate instance supervisors
    SupName = get_ref_from_tag(instance, Tag),
    SupPid = erlang:whereis(SupName),
    ok = supervisor:terminate_child(ldclient_sup, SupPid),
    ldclient_update_processor_state:delete_initialized_state(Tag),
    ldclient_update_processor_state:delete_storage_initialized_state(Tag),
    ldclient_settings:unregister(Tag).

%% @doc Stop all client instances
%%
%% @end
-spec stop_all() -> ok.
stop_all() ->
    Tags = ldclient_settings:get_registered_tags(),
    lists:foreach(fun stop/1, Tags).

%% @doc Whether an instance's update processor has initialized
%%
%% @end
-spec update_processor_initialized(Tag :: atom()) -> boolean().
update_processor_initialized(Tag) ->
    ldclient_update_processor_state:get_initialized_state(Tag).

%%===================================================================
%% Internal functions
%%===================================================================

%% @doc Get a supervisor registration name for a given tag
%% @private
%%
%% @end
-spec get_ref_from_tag(atom(), Tag :: atom()) -> atom().
get_ref_from_tag(instance, Tag) when is_atom(Tag) ->
    list_to_atom("ldclient_instance_" ++ atom_to_list(Tag));
get_ref_from_tag(instance_stream, Tag) when is_atom(Tag) ->
    list_to_atom("ldclient_instance_stream_" ++ atom_to_list(Tag));
get_ref_from_tag(instance_events, Tag) when is_atom(Tag) ->
    list_to_atom("ldclient_instance_events_" ++ atom_to_list(Tag)).

%% @doc Initialize update processor client and start listening
%% @private
%%
%% @end
-spec start_updater(atom(), atom(), atom()) ->
    ok.
start_updater(UpdateSupName, UpdateWorkerModule, Tag) ->
    {ok, _Pid} = ldclient_updater:start(UpdateSupName, UpdateWorkerModule, Tag),
    ok.

%% @doc Get update processor module name depending on settings
%% @private
%%
%% @end
-spec get_update_processor(Stream :: boolean(), Offline :: boolean()) -> atom().
get_update_processor(_Stream, true) -> ldclient_update_null_server;
get_update_processor(true, _Offline) -> ldclient_update_stream_server;
get_update_processor(false, _Offline) -> ldclient_update_poll_server.
