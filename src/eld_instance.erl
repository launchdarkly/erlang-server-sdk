%%-------------------------------------------------------------------
%% @doc `eld_instance' module
%%
%% @end
%%-------------------------------------------------------------------

-module(eld_instance).

%% API
-export([start/3]).
-export([stop/1]).
-export([stop_all/0]).

-type options() :: #{
    base_uri => string(),
    stream_uri => string(),
    storage_backend => atom(),
    events_capacity => pos_integer(),
    events_flush_interval => pos_integer(),
    events_dispatcher => atom(),
    user_keys_capacity => pos_integer(),
    inline_users_in_events => boolean(),
    private_attributes => eld_settings:private_attributes(),
    stream => boolean(),
    polling_interval => pos_integer(),
    polling_update_requestor => atom()
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
    Settings = eld_settings:parse_options(SdkKey, Options),
    ok = eld_settings:register(Tag, Settings),
    % Start instance supervisor
    SupName = get_ref_from_tag(instance, Tag),
    StartStream = maps:get(stream, Settings),
    UpdateSupName = get_ref_from_tag(instance_stream, Tag),
    UpdateWorkerModule = get_update_processor(StartStream),
    EventsSupName = get_ref_from_tag(instance_events, Tag),
    {ok, _} = supervisor:start_child(eld_sup, [SupName, UpdateSupName, UpdateWorkerModule, EventsSupName, Tag]),
    % Start storage backend
    StorageBackend = maps:get(storage_backend, Settings),
    ok = StorageBackend:init(SupName, Tag, []),
    % Start stream client
    ok = start_updater(UpdateSupName, UpdateWorkerModule, Tag).

%% @doc Stop a client instance
%%
%% @end
-spec stop(Tag :: atom()) -> ok.
stop(Tag) when is_atom(Tag) ->
    % TODO only stop stream instance if it's running
    % Terminate stream
    StreamSupName = get_ref_from_tag(instance_stream, Tag),
    ok = eld_updater:stop(StreamSupName),
    % Terminate storage
    StorageBackend = eld_settings:get_value(Tag, storage_backend),
    ok = StorageBackend:terminate(Tag),
    % Terminate instance supervisors
    SupName = get_ref_from_tag(instance, Tag),
    SupPid = erlang:whereis(SupName),
    ok = supervisor:terminate_child(eld_sup, SupPid),
    eld_settings:unregister(Tag).

%% @doc Stop all client instances
%%
%% @end
-spec stop_all() -> ok.
stop_all() ->
    Tags = eld_settings:get_registered_tags(),
    lists:foreach(fun stop/1, Tags).

%%===================================================================
%% Internal functions
%%===================================================================

%% @doc Get a supervisor registration name for a given tag
%% @private
%%
%% @end
-spec get_ref_from_tag(atom(), Tag :: atom()) -> atom().
get_ref_from_tag(instance, Tag) when is_atom(Tag) ->
    list_to_atom("eld_instance_" ++ atom_to_list(Tag));
get_ref_from_tag(instance_stream, Tag) when is_atom(Tag) ->
    list_to_atom("eld_instance_stream_" ++ atom_to_list(Tag));
get_ref_from_tag(instance_events, Tag) when is_atom(Tag) ->
    list_to_atom("eld_instance_events_" ++ atom_to_list(Tag)).

%% @doc Initialize update processor client and start listening
%% @private
%%
%% @end
-spec start_updater(atom(), atom(), atom()) ->
    ok.
start_updater(UpdateSupName, UpdateWorkerModule, Tag) ->
    ok = eld_updater:start(UpdateSupName, UpdateWorkerModule, Tag).

%% @doc Get update processor module name depending on settings
%% @private
%%
%% @end
-spec get_update_processor(Stream :: boolean()) -> atom().
get_update_processor(true) -> eld_update_stream_server;
get_update_processor(false) -> eld_update_poll_server.
