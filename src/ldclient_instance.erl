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
-export([feature_store_initialized/1]).

-type options() :: #{
    base_uri => string(),
    stream_uri => string(),
    feature_store => atom(),
    events_uri => string(),
    events_capacity => pos_integer(),
    events_flush_interval => pos_integer(),
    events_dispatcher => atom(),
    context_keys_capacity => pos_integer(),
    private_attributes => ldclient_config:private_attributes(),
    stream => boolean(),
    polling_interval => pos_integer(),
    polling_update_requestor => atom(),
    offline => boolean(),
    redis_host => string(),
    redis_port => pos_integer(),
    redis_database => integer(),
    redis_username => string() | undefined,
    redis_password => string(),
    redis_prefix => string(),
    redis_tls => [ssl:tls_option()] | undefined,
    use_ldd => boolean(),
    cache_ttl => integer(),
    send_events => boolean(),
    file_datasource => boolean(),
    file_paths => [string() | binary()],
    file_auto_update => boolean(),
    file_poll_interval => pos_integer(),
    file_allow_duplicate_keys => boolean(),
    http_options => ldclient_config:http_options(),
    testdata_tag => atom(),
    datasource => atom()
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
    % Parse options into settings
    Settings = ldclient_config:parse_options(SdkKey, Options),
    ok = ldclient_config:register(Tag, Settings),
    % Start instance supervisor
    SupName = get_ref_from_tag(instance, Tag),
    UpdateSupName = get_ref_from_tag(instance_stream, Tag),
    UpdateWorkerModule = get_update_processor(Settings),
    EventsSupName = get_ref_from_tag(instance_events, Tag),
    case
        supervisor:start_child(
            ldclient_sup,
            ldclient_instance_sup:child_spec(SupName, [
                SupName, UpdateSupName, UpdateWorkerModule, EventsSupName, Tag
            ])
        )
    of
        {ok, _} ->
            % Start storage backend
            true = ldclient_update_processor_state:create_storage_initialized_state(Tag, false),
            FeatureStore = maps:get(feature_store, Settings),
            ok = FeatureStore:init(SupName, Tag, []),
            % Start stream client
            true = ldclient_update_processor_state:create_initialized_state(Tag, false),
            start_updater(UpdateSupName, UpdateWorkerModule, Tag);
        {error, {already_started, Pid}} ->
            {error, already_started, Pid}
    end.

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
    FeatureStore = ldclient_config:get_value(Tag, feature_store),
    ok = FeatureStore:terminate(Tag),
    % Terminate instance supervisors
    SupName = get_ref_from_tag(instance, Tag),
    ok = supervisor:terminate_child(ldclient_sup, SupName),
    ok = supervisor:delete_child(ldclient_sup, SupName),
    ldclient_update_processor_state:delete_initialized_state(Tag),
    ldclient_update_processor_state:delete_storage_initialized_state(Tag),
    ldclient_config:unregister(Tag).

%% @doc Stop all client instances
%%
%% @end
-spec stop_all() -> ok.
stop_all() ->
    Tags = ldclient_config:get_registered_tags(),
    lists:foreach(fun stop/1, Tags).

%% @doc Whether an instance's update processor has initialized
%%
%% @end
-spec update_processor_initialized(Tag :: atom()) -> boolean().
update_processor_initialized(Tag) ->
    ldclient_update_processor_state:get_initialized_state(Tag).

%% @doc Whether an instance's feature store has initialized
%%
%% @end
-spec feature_store_initialized(Tag :: atom()) -> boolean().
feature_store_initialized(Tag) ->
    ldclient_update_processor_state:get_storage_initialized_state(Tag).

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
-spec get_update_processor(
    Settings :: #{
        stream := boolean(),
        offline := boolean(),
        use_ldd := boolean(),
        file_datasource := boolean(),
        datasource := atom(),
        _ => _
    }
) -> atom().
get_update_processor(#{offline := true}) ->
    ldclient_update_null_server;
get_update_processor(#{use_ldd := true}) ->
    ldclient_update_null_server;
get_update_processor(#{datasource := DataSource}) when DataSource /= undefined ->
    list_to_atom("ldclient_update_" ++ atom_to_list(DataSource) ++ "_server");
get_update_processor(#{file_datasource := true}) ->
    ldclient_update_file_server;
get_update_processor(#{stream := false}) ->
    ldclient_update_poll_server;
get_update_processor(#{stream := true}) ->
    ldclient_update_stream_server.
