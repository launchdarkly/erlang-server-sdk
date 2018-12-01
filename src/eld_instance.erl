%%-------------------------------------------------------------------
%% @doc `eld_instance' module
%%
%% @end
%%-------------------------------------------------------------------

-module(eld_instance).

%% API
-export([start/3]).
-export([stop/1]).

-type options() :: #{
    base_uri => string(),
    stream_uri => string(),
    storage_backend => atom(),
    start_stream => boolean()
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
    Settings = eld_settings:parse_options(SdkKey, Options),
    ok = eld_settings:register(Tag, Settings),
    % Start instance supervisor
    SupName = get_supref_from_tag(instance, Tag),
    StreamSupName = get_supref_from_tag(instance_stream, Tag),
    {ok, _} = supervisor:start_child(eld_sup, [SupName, StreamSupName]),
    % Start storage backend
    StorageBackend = maps:get(storage_backend, Settings),
    ok = StorageBackend:init(SupName, Tag, []),
    % Start stream client
    StartStream = maps:get(start_stream, Options, true),
    ok = start_stream(StartStream, Tag, SupName).

%% @doc Stop a client instance
%%
%% @end
-spec stop(Tag :: atom()) -> ok.
stop(Tag) when is_atom(Tag) ->
    % TODO only stop stream instance if it's running
    % Terminate stream
    StreamSupName = get_supref_from_tag(instance_stream, Tag),
    ok = eld_stream:stop(StreamSupName),
    % Terminate storage
    StorageBackend = eld_settings:get_value(Tag, storage_backend),
    ok = StorageBackend:terminate(Tag),
    % Terminate instance supervisors
    SupName = get_supref_from_tag(instance, Tag),
    SupPid = erlang:whereis(SupName),
    ok = supervisor:terminate_child(eld_sup, SupPid),
    eld_settings:unregister(Tag).

%%===================================================================
%% Internal functions
%%===================================================================

%% @doc Get a supervisor registration name for a given tag
%% @private
%%
%% @end
get_supref_from_tag(instance, Tag) when is_atom(Tag) ->
    list_to_atom("eld_instance_" ++ atom_to_list(Tag));
get_supref_from_tag(instance_stream, Tag) when is_atom(Tag) ->
    list_to_atom("eld_instance_stream_" ++ atom_to_list(Tag)).

%% @doc Initialize streamer client and start listening
%% @private
%%
%% @end
-spec start_stream(StartStream :: boolean, Tag :: atom(), SupName :: atom()) ->
    ok.
start_stream(false, _, _) -> ok;
start_stream(true, Tag, _SupName) ->
    StreamSupName = get_supref_from_tag(instance_stream, Tag),
    %{ok, _Pid} = supervisor:start_child(eld_sup, [SupName, StreamSupName]),
    ok = eld_stream:start(StreamSupName, Tag).
