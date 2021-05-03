%%-------------------------------------------------------------------
%% @doc `ldclient' module
%%
%% Acts as an interface to most common SDK functions: starting and stopping
%% client instances, and evaluating feature flags for users.
%% @end
%%-------------------------------------------------------------------

-module(ldclient).

%% API
-export([start_instance/1]).
-export([start_instance/2]).
-export([start_instance/3]).
-export([stop_all_instances/0]).
-export([stop_instance/0]).
-export([stop_instance/1]).
-export([is_offline/1]).
-export([initialized/1]).
-export([variation/3]).
-export([variation/4]).
-export([variation_detail/3]).
-export([variation_detail/4]).
-export([all_flags_state/1]).
-export([all_flags_state/2]).
-export([identify/1]).
-export([identify/2]).
-export([track/3]).
-export([track/4]).
-export([track_metric/4]).
-export([track_metric/5]).
-export([alias/2]).
-export([alias/3]).

%% Constants
-define(DEFAULT_INSTANCE_NAME, default).

%%===================================================================
%% API
%%===================================================================

%% @doc Start client with default options
%%
%% The SDK key is required to connect. Default streamer URL, storage backend
%% and instance name `default' will be used.
%% @end
-spec start_instance(SdkKey :: string()) -> ok | {error, atom(), term()}.
start_instance(SdkKey) ->
    start_instance(SdkKey, ?DEFAULT_INSTANCE_NAME).

%% @doc Start client with custom name or options
%%
%% When `TagOrOptions' is an atom, the instance is started with that name. When
%% it's a map, it can be used to start with custom options.
%% @end
-spec start_instance(SdkKey :: string(), TagOrOptions :: atom() | map()) ->
    ok | {error, atom(), term()}.
start_instance(SdkKey, Tag) when is_list(SdkKey), is_atom(Tag) ->
    start_instance(SdkKey, Tag, #{});
start_instance(SdkKey, Options) when is_list(SdkKey), is_map(Options) ->
    start_instance(SdkKey, ?DEFAULT_INSTANCE_NAME, Options).

%% @doc Start client with custom name and options
%%
%% Specify both custom client name and options when starting the client.
%% @end
-spec start_instance(SdkKey :: string(), Tag :: atom(), Options :: map()) ->
    ok | {error, atom(), term()}.
start_instance(SdkKey, Tag, Options) when is_list(SdkKey), is_atom(Tag), is_map(Options) ->
    ldclient_instance:start(Tag, SdkKey, Options).

%% @doc Stop all client instances
%%
%% @end
-spec stop_all_instances() -> ok.
stop_all_instances() ->
    ldclient_instance:stop_all().

%% @doc Stop client instance
%%
%% Stops the default client instance.
%% @end
-spec stop_instance() -> ok | {error, not_found, term()}.
stop_instance() ->
    stop_instance(?DEFAULT_INSTANCE_NAME).

%% @doc Stop client with the custom name
%%
%% This is useful if a client instance was started with a custom name.
%% @end
-spec stop_instance(Tag :: atom()) -> ok | {error, not_found, term()}.
stop_instance(Tag) when is_atom(Tag) ->
    ldclient_instance:stop(Tag).

%% @doc Returns whether the LaunchDarkly client is in offline mode.
%%
%% In some situations, you might want to stop making remote calls to LaunchDarkly and 
%% fall back to default values for your feature flags, this tells you whether only default 
%% values will be returned.
%% @end
-spec is_offline(Tag :: atom()) -> boolean().
is_offline(Tag) ->
    ldclient_config:get_value(Tag, offline).

%% @doc Returns whether the LaunchDarkly client has initialized.
%%
%% If this value is true, it means the client has succeeded at some point in connecting to LaunchDarkly and
%% has received feature flag data. It could still have encountered a connection problem after that point, so
%% this does not guarantee that the flags are up to date. Alternatively, it could also mean that the client
%% is in offline mode.
%%
%% If this value is false, it means the client has not yet connected to LaunchDarkly, or has permanently
%% failed. In this state, feature flag evaluations will always return default values.
%% @end
-spec initialized(Tag :: atom()) -> boolean().
initialized(Tag) ->
    IsOffline = is_offline(Tag),
    UpdateProcessorInitialized = ldclient_instance:update_processor_initialized(Tag),
    case ldclient_config:get_value(Tag, feature_store) of
        ldclient_storage_redis -> FeatureStoreInitialized = ldclient_instance:feature_store_initialized(Tag),
                                  IsOffline or UpdateProcessorInitialized and FeatureStoreInitialized;
        _ -> IsOffline or UpdateProcessorInitialized
    end.

%% @doc Evaluate given flag key for given user
%%
%% Evaluates the flag and returns the resulting variation value. The default
%% value will be returned in case of any errors.
%% @end
-spec variation(FlagKey :: binary(), User :: ldclient_user:user(), DefaultValue :: ldclient_eval:result_value()) ->
    ldclient_eval:result_value().
variation(FlagKey, User, DefaultValue) when is_binary(FlagKey), is_map(User) ->
     variation(FlagKey, User, DefaultValue, ?DEFAULT_INSTANCE_NAME).

%% @doc Evaluate given flag key for given user and given client instance
%%
%% Evaluates the flag and returns the resulting variation value. The default
%% value will be returned in case of any errors.
%% @end
-spec variation(FlagKey :: binary(), User :: ldclient_user:user(), DefaultValue :: ldclient_eval:result_value(), Tag :: atom()) ->
    ldclient_eval:result_value().
variation(FlagKey, User, DefaultValue, Tag) when is_binary(FlagKey), is_map(User) ->
    % Get evaluation result detail
    {{_Index, Value, _Reason}, Events} = ldclient_eval:flag_key_for_user(Tag, FlagKey, User, DefaultValue),
    % Send events
    SendEventsFun = fun(Event) -> ldclient_event_server:add_event(Tag, Event, #{}) end,
    lists:foreach(SendEventsFun, Events),
    % Return evaluation result
    Value.

%% @doc Evaluate given flag key for given user
%%
%% Evaluates the flag and returns the result detail containing the variation
%% index, value, and reason why the specific result was chosen. The default
%% value will be returned in case of any errors.
%% @end
-spec variation_detail(FlagKey :: binary(), User :: ldclient_user:user(), DefaultValue :: ldclient_eval:result_value()) ->
    ldclient_eval:detail().
variation_detail(FlagKey, User, DefaultValue) when is_binary(FlagKey), is_map(User) ->
    variation_detail(FlagKey, User, DefaultValue, ?DEFAULT_INSTANCE_NAME).

%% @doc Evaluate given flag key for given user and given client instance
%%
%% Evaluates the flag and returns the result detail containing the variation
%% index, value, and reason why the specific result was chosen. The default
%% value will be returned in case of any errors.
%% @end
-spec variation_detail(FlagKey :: binary(), User :: ldclient_user:user(), DefaultValue :: ldclient_eval:result_value(), Tag :: atom()) ->
    ldclient_eval:detail().
variation_detail(FlagKey, User, DefaultValue, Tag) when is_binary(FlagKey), is_map(User) ->
    % Get evaluation result detail
    {Detail, Events} = ldclient_eval:flag_key_for_user(Tag, FlagKey, User, DefaultValue),
    % Send events
    SendEventsFun = fun(Event) -> ldclient_event_server:add_event(Tag, Event, #{include_reasons => true}) end,
    lists:foreach(SendEventsFun, Events),
    % Return evaluation detail
    Detail.

%% @doc Evaluate all flags for a given user and return their values
%%
%% Evaluates all existing flags, but does not create any events as a side
%% effect of the evaluation. It returns a map of flag keys to evaluated values.
%% @end
-spec all_flags_state(User :: ldclient_user:user()) -> ldclient_eval:feature_flags_state().
all_flags_state(User) ->
    all_flags_state(User, ?DEFAULT_INSTANCE_NAME).

%% @doc Evaluate all flags for a given user and given client instance
%%
%% Evaluates all existing flags, but does not create any events as a side
%% effect of the evaluation. It returns a map of flag keys to evaluated values.
%% @end
-spec all_flags_state(User :: ldclient_user:user(), Tag :: atom()) -> ldclient_eval:feature_flags_state().
all_flags_state(User, Tag) ->
    ldclient_eval:all_flags_eval(User, Tag).

%% @doc Identify reports details about a user
%%
%% This function uses the default client instance.
%% @end
-spec identify(User :: ldclient_user:user()) -> ok.
identify(User) ->
    identify(User, ?DEFAULT_INSTANCE_NAME).

%% @doc Identify reports details about a user
%%
%% This is useful to report user to a specific client instance.
%% @end
-spec identify(User :: ldclient_user:user(), Tag :: atom()) -> ok.
identify(User, Tag) when is_atom(Tag) ->
    Event = ldclient_event:new_identify(User),
    ldclient_event_server:add_event(Tag, Event, #{}).

%% @doc Track reports that a user has performed an event
%%
%% Custom data can be attached to the event.
%% @end
-spec track(Key :: binary(), User :: ldclient_user:user(), Data :: map()) -> ok.
track(Key, User, Data) when is_binary(Key), is_map(Data) ->
    track(Key, User, Data, ?DEFAULT_INSTANCE_NAME).

%% @doc Track reports that a user has performed an event
%%
%% This is useful for specifying a specific client instance.
%% @end
-spec track(Key :: binary(), User :: ldclient_user:user(), Data :: map(), Tag :: atom()) -> ok.
track(Key, User, Data, Tag) when is_atom(Tag), is_binary(Key), is_map(Data) ->
    Event = ldclient_event:new_custom(Key, User, Data),
    ldclient_event_server:add_event(Tag, Event, #{}).

%% @doc Reports that a user has performed an event, and associates it with a numeric value.
%%
%% This value is used by the LaunchDarkly experimentation feature in numeric custom metrics, and will also
%% be returned as part of the custom event for Data Export.
%%
%% Custom data can also be attached to the event.
%% @end
-spec track_metric(Key :: binary(), User :: ldclient_user:user(), Data :: map(), Metric :: number()) -> ok.
track_metric(Key, User, Data, Metric) ->
    track_metric(Key, User, Data, Metric, ?DEFAULT_INSTANCE_NAME).

%% @doc Reports that a user has performed an event, and associates it with a numeric value.
%%
%% This value is used by the LaunchDarkly experimentation feature in numeric custom metrics, and will also
%% be returned as part of the custom event for Data Export.
%%
%% Custom data can also be attached to the event.
%% @end
-spec track_metric(Key :: binary(), User :: ldclient_user:user(), Data :: map(), Metric :: number(), Tag :: atom()) -> ok.
track_metric(Key, User, Data, Metric, Tag) ->
    Event = ldclient_event:new_custom(Key, User, Data, Metric),
    ldclient_event_server:add_event(Tag, Event, #{}).

%% @doc Associates two users for analytics purposes.
%%
%% This can be helpful in the situation where a person is represented by multiple
%% LaunchDarkly users. This may happen, for example, when a person initially logs into
%% an application-- the person might be represented by an anonymous user prior to logging
%% in and a different user after logging in, as denoted by a different user key.
%% @end
-spec alias(User :: ldclient_user:user(), PreviousUser :: ldclient_user:user()) -> ok.
alias(User, PreviousUser) ->
    Event = ldclient_event:new_alias(User, PreviousUser),
    ldclient_event_server:add_event(?DEFAULT_INSTANCE_NAME, Event, #{}).

%% @doc Associates two users for analytics purposes.
%%
%% This can be helpful in the situation where a person is represented by multiple
%% LaunchDarkly users. This may happen, for example, when a person initially logs into
%% an application-- the person might be represented by an anonymous user prior to logging
%% in and a different user after logging in, as denoted by a different user key.
%% @end
-spec alias(User :: ldclient_user:user(), PreviousUser :: ldclient_user:user(), Tag :: atom()) -> ok.
alias(User, PreviousUser, Tag) ->
    Event = ldclient_event:new_alias(User, PreviousUser),
    ldclient_event_server:add_event(Tag, Event, #{}).
