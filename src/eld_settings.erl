%%-------------------------------------------------------------------
%% @doc `eld_settings' module
%%
%% Acts as a storage interface for SDK client instance settings.
%% @end
%%-------------------------------------------------------------------

-module(eld_settings).

%% API
-export([init/0]).
-export([parse_options/2]).
-export([get_value/2]).
-export([register/2]).
-export([unregister/1]).

%% Types
-type instance() :: #{
    sdk_key => string(),
    base_uri => string(),
    stream_uri => string(),
    storage_backend => atom()
}.
% Settings stored for each running SDK instance

%% Constants
-define(DEFAULT_BASE_URI, "https://app.launchdarkly.com").
-define(DEFAULT_EVENTS_URI, "https://events.launchdarkly.com").
-define(DEFAULT_STREAM_URI, "https://events.launchdarkly.com").
-define(DEFAULT_STORAGE_BACKEND, eld_storage_ets).

%%===================================================================
%% API
%%===================================================================

%% @doc Initialize settings environment map
%%
%% Initializes an empty map for instance settings in application environment.
%% @end
-spec init() -> ok.
init() ->
    application:set_env(eld, instances, #{}).

%% @doc Parses given map of options
%% @private
%%
%% @end
-spec parse_options(SdkKey :: string(), Options :: map()) -> instance().
parse_options(SdkKey, Options) when is_list(SdkKey), is_map(Options) ->
    BaseUri = maps:get(base_uri, Options, ?DEFAULT_BASE_URI),
    EventsUri = maps:get(events_uri, Options, ?DEFAULT_EVENTS_URI),
    StreamUri = maps:get(stream_uri, Options, ?DEFAULT_STREAM_URI),
    StorageBackend = maps:get(storage_backend, Options, ?DEFAULT_STORAGE_BACKEND),
    #{
        sdk_key => SdkKey,
        base_uri => BaseUri,
        events_uri => EventsUri,
        stream_uri => StreamUri,
        storage_backend => StorageBackend
    }.

%% @doc Gets application environment variable value
%%
%% This is a convenience function to retrieve application environment variables
%% in one place. `Tag' is the instance tag. `Key' is the key of the
%% configuration option.
%% @end
-spec get_value(Tag :: atom(), Key :: atom()) -> undefined | term().
get_value(Tag, Key) when is_atom(Tag), is_atom(Key) ->
    {ok, Instances} = application:get_env(eld, instances),
    InstanceSettings = maps:get(Tag, Instances),
    maps:get(Key, InstanceSettings).

%% @doc Register settings for a new client instance
%%
%% @end
-spec register(Tag :: atom(), Settings :: instance()) -> ok.
register(Tag, Settings) when is_atom(Tag), is_map(Settings) ->
    Instances = get_all(),
    NewInstances = Instances#{Tag => Settings},
    application:set_env(eld, instances, NewInstances).

%% @doc Unregister settings for a client instance
%%
%% @end
-spec unregister(Tag :: atom()) -> ok.
unregister(Tag) when is_atom(Tag) ->
    NewInstances = maps:remove(Tag, get_all()),
    application:set_env(eld, instances, NewInstances).

%%===================================================================
%% Internal functions
%%===================================================================

-spec get_all() -> #{Tag :: atom() => eld_settings:instance()}.
get_all() ->
    {ok, Instances} = application:get_env(eld, instances),
    Instances.
