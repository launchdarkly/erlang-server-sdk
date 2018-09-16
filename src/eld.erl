%%%-------------------------------------------------------------------
%%% @doc `eld' module
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(eld).

%% API
-export([start/1]).
-export([start/2]).
-export([start_stream/0]).
-export([stop_all_streams/0]).
-export([start_storage/1]).
-export([stop/0]).

%% Constants
-define(DEFAULT_BASE_URI, "https://app.launchdarkly.com").
-define(DEFAULT_EVENTS_URI, "https://events.launchdarkly.com").
-define(DEFAULT_STREAM_URI, "https://events.launchdarkly.com").
-define(DEFAULT_STORAGE_BACKEND, eld_storage_ets).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts streamer client and storage servers with default values
%%
%% Default streamer host and storage service (ETS) will be used.
%% @end
-spec start(SdkKey :: string()) -> ok | {error, atom(), term()}.
start(SdkKey) ->
    start(SdkKey, ?DEFAULT_STORAGE_BACKEND, #{}).

%% @doc Starts streamer client and storage servers with some options
%%
%% Specify either custom storage backend or config options.
%% @end
-spec start(SdkKey :: string(), Options :: atom() | map()) -> ok | {error, atom(), term()}.
start(SdkKey, StorageBackend) when is_atom(StorageBackend) ->
    start(SdkKey, StorageBackend, #{});
start(SdkKey, Options) when is_map(Options) ->
    start(SdkKey, ?DEFAULT_STORAGE_BACKEND, Options).

%% @doc Starts streamer client and storage servers with custom values
%%
%% Stores options in application environment, starts storage backend, starts
%% streamer client. Valid options are `base_uri', `events_uri', and
%% `stream_uri'. If an option is not supplied, the default will be used.
%% @end
-spec start(SdkKey :: string(), StorageBackend :: atom(), Options :: map()) -> ok | {error, atom(), term()}.
start(SdkKey, StorageBackend, Options) ->
    Opts = parse_options(Options),
    ok = store_settings(SdkKey, StorageBackend, Opts),
    ok = start_storage(StorageBackend),
    start_stream().

%% @doc Starts streamer client process
%%
%% It will retrieve the SDK key and URI from application environment.
%% @end
-spec start_stream() -> ok | {error, atom(), term()}.
start_stream() ->
    {ok, Pid} = supervisor:start_child(eld_stream_sup, []),
    eld_stream_server:listen(Pid).

%% @doc Terminates all current stream listeners
%%
%% @end
-spec stop_all_streams() -> ok.
stop_all_streams() ->
    ok = terminate_all_children(eld_stream_sup).

%% @doc Starts storage server.
%%
%% `StorageBackend' must be a module name that implements `eld_storage_engine'
%% behavior.
%% @end
-spec start_storage(StorageBackend :: atom()) -> ok.
start_storage(StorageBackend) ->
    ok = StorageBackend:init([]).

-spec stop() -> ok.
stop() ->
    ok = stop_all_streams(),
    {ok, StorageBackend} = application:get_env(eld, storage_backend),
    StorageBackend:terminate().

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Parses given map of options
%% @private
%%
%% @end
-spec parse_options(Options :: map()) -> map().
parse_options(Options) ->
    BaseUri = maps:get(base_uri, Options, ?DEFAULT_BASE_URI),
    EventsUri = maps:get(events_uri, Options, ?DEFAULT_EVENTS_URI),
    StreamUri = maps:get(stream_uri, Options, ?DEFAULT_STREAM_URI),
    #{
        base_uri => BaseUri,
        events_uri => EventsUri,
        stream_uri => StreamUri
    }.

%% @doc Stores startup options in application environment
%%
%% This is useful for later retrieval by streamer initalization.
%% @end
-spec store_settings(SdkKey :: string(), StorageBackend :: atom(), Options :: map()) -> ok.
store_settings(SdkKey, StorageBackend, Options) ->
    ok = application:set_env(eld, sdk_key, SdkKey),
    ok = application:set_env(eld, storage_backend, StorageBackend),
    ok = application:set_env(eld, base_uri, maps:get(base_uri, Options)),
    ok = application:set_env(eld, events_uri, maps:get(events_uri, Options)),
    ok = application:set_env(eld, stream_uri, maps:get(stream_uri, Options)).

%% @doc Terminates all children of a given supervisor
%% @private
%%
%% @end
-spec terminate_all_children(Sup :: atom()) -> ok.
terminate_all_children(Sup) ->
    Pids = [Pid || {_, Pid, worker, _} <- supervisor:which_children(Sup)],
    terminate_all_children(Sup, Pids).

%% @doc Recursively terminates processes of given children Pids
%% @private
%%
%% @end
-spec terminate_all_children(Sup :: atom(), [pid()]) -> ok.
terminate_all_children(_Sup, []) ->
    ok;
terminate_all_children(Sup, [Pid|Rest]) ->
    ok = supervisor:terminate_child(Sup, Pid),
    terminate_all_children(Sup, Rest).
