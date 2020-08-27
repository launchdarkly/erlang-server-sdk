%%-------------------------------------------------------------------
%% @doc `ldclient_settings' module
%%
%% Acts as a storage interface for SDK client instance settings.
%% @end
%%-------------------------------------------------------------------

-module(ldclient_settings).

%% API
-export([init/0]).
-export([parse_options/2]).
-export([get_registered_tags/0]).
-export([get_value/2]).
-export([register/2]).
-export([unregister/1]).
-export([get_user_agent/0]).
-export([get_event_schema/0]).

%% Types
-type instance() :: #{
    sdk_key => string(),
    base_uri => string(),
    events_uri => string(),
    stream_uri => string(),
    storage_backend => atom(),
    events_capacity => pos_integer(),
    events_flush_interval => pos_integer(),
    events_dispatcher => atom(),
    user_keys_capacity => pos_integer(),
    inline_users_in_events => boolean(),
    private_attributes => private_attributes(),
    stream => boolean(),
    polling_interval => pos_integer(),
    polling_update_requestor => atom(),
    offline => boolean()
}.
% Settings stored for each running SDK instance

-type private_attributes() :: all | [ldclient_user:attribute()].

-export_type([private_attributes/0]).

%% Constants
-define(DEFAULT_BASE_URI, "https://app.launchdarkly.com").
-define(DEFAULT_EVENTS_URI, "https://events.launchdarkly.com").
-define(DEFAULT_STREAM_URI, "https://stream.launchdarkly.com").
-define(DEFAULT_STORAGE_BACKEND, ldclient_storage_ets).
-define(DEFAULT_EVENTS_CAPACITY, 10000).
-define(DEFAULT_EVENTS_FLUSH_INTERVAL, 30000).
-define(DEFAULT_EVENTS_DISPATCHER, ldclient_event_dispatch_httpc).
-define(DEFAULT_USER_KEYS_CAPACITY, 1000).
-define(DEFAULT_INLINE_USERS_IN_EVENTS, false).
-define(DEFAULT_PRIVATE_ATTRIBUTES, []).
-define(DEFAULT_STREAM, true).
-define(DEFAULT_POLLING_UPDATE_REQUESTOR, ldclient_update_requestor_httpc).
-define(MINIMUM_POLLING_INTERVAL, 30).
-define(USER_AGENT, "ErlangClient").
-define(VERSION, "1.0.0-beta4").
-define(EVENT_SCHEMA, "3").
-define(DEFAULT_OFFLINE, false).

%%===================================================================
%% API
%%===================================================================

%% @doc Initialize settings environment map
%%
%% Initializes an empty map for instance settings in application environment.
%% @end
-spec init() -> ok.
init() ->
    ok = ldclient_update_processor_state:init(),
    application:set_env(ldclient, instances, #{}).

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
    EventsCapacity = maps:get(events_capacity, Options, ?DEFAULT_EVENTS_CAPACITY),
    EventsFlushInterval = maps:get(events_flush_interval, Options, ?DEFAULT_EVENTS_FLUSH_INTERVAL),
    EventsDispatcher = maps:get(events_dispatcher, Options, ?DEFAULT_EVENTS_DISPATCHER),
    UserKeysCapacity = maps:get(user_keys_capacity, Options, ?DEFAULT_USER_KEYS_CAPACITY),
    InlineUsersInEvents = maps:get(inline_users_in_events, Options, ?DEFAULT_INLINE_USERS_IN_EVENTS),
    PrivateAttributes = maps:get(private_attributes, Options, ?DEFAULT_PRIVATE_ATTRIBUTES),
    Stream = maps:get(stream, Options, ?DEFAULT_STREAM),
    PollingUpdateRequestor = maps:get(polling_update_requestor, Options, ?DEFAULT_POLLING_UPDATE_REQUESTOR),
    OfflineMode = maps:get(offline, Options, ?DEFAULT_OFFLINE),
    PollingInterval = lists:max([
        ?MINIMUM_POLLING_INTERVAL,
        maps:get(polling_interval, Options, ?MINIMUM_POLLING_INTERVAL)
    ]),
    #{
        sdk_key => SdkKey,
        base_uri => BaseUri,
        events_uri => EventsUri,
        stream_uri => StreamUri,
        storage_backend => StorageBackend,
        events_capacity => EventsCapacity,
        events_flush_interval => EventsFlushInterval,
        events_dispatcher => EventsDispatcher,
        user_keys_capacity => UserKeysCapacity,
        inline_users_in_events => InlineUsersInEvents,
        private_attributes => PrivateAttributes,
        stream => Stream,
        polling_update_requestor => PollingUpdateRequestor,
        offline => OfflineMode,
        polling_interval => PollingInterval
    }.

%% @doc Get all registered tags
%%
%% @end
-spec get_registered_tags() -> [atom()].
get_registered_tags() ->
    maps:keys(get_all()).

%% @doc Gets application environment variable value
%%
%% This is a convenience function to retrieve application environment variables
%% in one place. `Tag' is the instance tag. `Key' is the key of the
%% configuration option.
%% @end
-spec get_value(Tag :: atom(), Key :: atom()) -> undefined | term().
get_value(Tag, Key) when is_atom(Tag), is_atom(Key) ->
    {ok, Instances} = application:get_env(ldclient, instances),
    InstanceSettings = maps:get(Tag, Instances),
    maps:get(Key, InstanceSettings).

%% @doc Register settings for a new client instance
%%
%% @end
-spec register(Tag :: atom(), Settings :: instance()) -> ok.
register(Tag, Settings) when is_atom(Tag), is_map(Settings) ->
    Instances = get_all(),
    NewInstances = Instances#{Tag => Settings},
    application:set_env(ldclient, instances, NewInstances).

%% @doc Unregister settings for a client instance
%%
%% @end
-spec unregister(Tag :: atom()) -> ok.
unregister(Tag) when is_atom(Tag) ->
    NewInstances = maps:remove(Tag, get_all()),
    application:set_env(ldclient, instances, NewInstances).

-spec get_user_agent() -> string().
get_user_agent() ->
    ?USER_AGENT ++ "/" ++ ?VERSION.

-spec get_event_schema() -> string().
get_event_schema() ->
    ?EVENT_SCHEMA.

%%===================================================================
%% Internal functions
%%===================================================================

-spec get_all() -> #{Tag :: atom() => instance()}.
get_all() ->
    {ok, Instances} = application:get_env(ldclient, instances),
    Instances.
