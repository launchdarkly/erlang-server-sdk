%%-------------------------------------------------------------------
%% @doc `ldclient' module
%%
%% Acts as an interface to most common SDK functions: starting and stopping
%% client instances, and evaluating feature flags for contexts.
%%
%% Most use cases only need a single client instance for the lifetime of
%% their application. Consider using multiple instances only if you need to
%% simultaneously access more than one environment. Do not start an instance
%% every time you need to make a variation or other SDK call.
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
-export([all_flags_state/3]).
-export([identify/1]).
-export([identify/2]).
-export([track/3]).
-export([track/4]).
-export([track_metric/4]).
-export([track_metric/5]).

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
%%
%% == Analytics events ==
%%
%% Event ingestion is non-blocking: flag evaluations hand events to the SDK
%% without waiting on the analytics pipeline. Buffered events are dispatched by
%% a pool of reporter workers that can autoscale with the buffer depth. The
%% following options tune this pipeline (defaults shown in parentheses):
%%
%% <ul>
%%   <li>`events_capacity' (`10000') - maximum number of full-fidelity events
%%       held in the buffer before new events are dropped.</li>
%%   <li>`events_flush_interval' (`30000') - how often, in milliseconds,
%%       buffered events are handed to the reporter pool.</li>
%%   <li>`events_dispatcher' (`ldclient_event_dispatch_httpc') - module
%%       implementing the `ldclient_event_dispatch' behaviour.</li>
%%   <li>`events_shed_threshold' (`events_capacity') - buffer depth at which
%%       best-effort events (identify/custom) are shed (dropped) by the caller
%%       instead of being enqueued, bounding memory and the event server mailbox
%%       under overload. Feature request events are always processed so summary
%%       analytics continue to count every evaluation; only their full-fidelity
%%       payloads are subject to `events_capacity'.</li>
%%   <li>`events_min_workers' (`1') - minimum reporter worker pool size.</li>
%%   <li>`events_max_workers' (`1') - maximum reporter worker pool size. Set
%%       above `events_min_workers' to enable autoscaling.</li>
%%   <li>`events_batch_size' (`100') - maximum number of events a worker sends
%%       per request.</li>
%%   <li>`events_scale_up_threshold' (half of `events_capacity') - buffer depth
%%       at which the pool commissions another worker.</li>
%%   <li>`events_scale_down_threshold' (`0') - buffer depth at or below which
%%       the pool decommissions an idle worker.</li>
%%   <li>`events_scale_interval_ms' (`1000') - how often the pool samples the
%%       buffer depth.</li>
%%   <li>`events_scale_cooldown_ms' (`1000') - minimum time between scaling
%%       decisions, to avoid thrashing.</li>
%% </ul>
%%
%% == Telemetry ==
%%
%% The SDK emits `telemetry' events (beam-telemetry) from the analytics
%% pipeline:
%%
%% <ul>
%%   <li>`[ldclient, events, shed]' - emitted when an event is dropped due to
%%       load shedding. Measurement `count' (number of events shed, currently
%%       always `1'); metadata contains the instance `tag'. Suitable for a
%%       counter metric.</li>
%%   <li>`[ldclient, events, published]' - emitted when a batch is successfully
%%       delivered. Measurement `count' (number of events in the batch);
%%       metadata contains the instance `tag'. Suitable for a counter metric.</li>
%%   <li>`[ldclient, events, send_error]' - emitted when a batch fails to send.
%%       Measurement `count' (currently always `1'); metadata contains the
%%       instance `tag' and the failure `type' (`temporary' or `permanent').
%%       Suitable for a counter metric.</li>
%%   <li>`[ldclient, events, pool_size]' - emitted whenever the reporter pool
%%       size changes, and once at startup. Measurement `workers' is the
%%       absolute pool size; metadata contains the instance `tag' and the
%%       `direction' of the change (`initial', `up' or `down'). Suitable for a
%%       gauge metric.</li>
%% </ul>
%%
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
    IsOffline or UpdateProcessorInitialized.

%% @doc Evaluate given flag key for given context
%%
%% Evaluates the flag and returns the resulting variation value. The default
%% value will be returned in case of any errors.
%% @end
-spec variation(FlagKey :: binary(), Context :: ldclient_user:user() | ldclient_context:context(), DefaultValue :: ldclient_eval:result_value()) ->
    ldclient_eval:result_value().
variation(FlagKey, Context, DefaultValue) when is_binary(FlagKey), is_map(Context) ->
     variation(FlagKey, ensure_context(Context), DefaultValue, ?DEFAULT_INSTANCE_NAME).

%% @doc Evaluate given flag key for given context and given client instance
%%
%% Evaluates the flag and returns the resulting variation value. The default
%% value will be returned in case of any errors.
%% @end
-spec variation(FlagKey :: binary(), Context :: ldclient_user:user() | ldclient_context:context(), DefaultValue :: ldclient_eval:result_value(), Tag :: atom()) ->
    ldclient_eval:result_value().
variation(FlagKey, Context, DefaultValue, Tag) when is_binary(FlagKey), is_map(Context) ->
    % Get evaluation result detail
    {{_Index, Value, _Reason}, Events} = ldclient_eval:flag_key_for_context(Tag, FlagKey, ensure_context(Context), DefaultValue),
    % Send events
    SendEventsFun = fun(Event) -> ldclient_event_server:add_event(Tag, Event, #{}) end,
    lists:foreach(SendEventsFun, Events),
    % Return evaluation result
    Value.

%% @doc Evaluate given flag key for given context
%%
%% Evaluates the flag and returns the result detail containing the variation
%% index, value, and reason why the specific result was chosen. The default
%% value will be returned in case of any errors.
%% @end
-spec variation_detail(FlagKey :: binary(), Context :: ldclient_user:user() | ldclient_context:context(), DefaultValue :: ldclient_eval:result_value()) ->
    ldclient_eval:detail().
variation_detail(FlagKey, Context, DefaultValue) when is_binary(FlagKey), is_map(Context) ->
    variation_detail(FlagKey, ensure_context(Context), DefaultValue, ?DEFAULT_INSTANCE_NAME).

%% @doc Evaluate given flag key for given context and given client instance
%%
%% Evaluates the flag and returns the result detail containing the variation
%% index, value, and reason why the specific result was chosen. The default
%% value will be returned in case of any errors.
%% @end
-spec variation_detail(FlagKey :: binary(), Context :: ldclient_user:user() | ldclient_context:context(), DefaultValue :: ldclient_eval:result_value(), Tag :: atom()) ->
    ldclient_eval:detail().
variation_detail(FlagKey, Context, DefaultValue, Tag) when is_binary(FlagKey), is_map(Context) ->
    % Get evaluation result detail
    {Detail, Events} = ldclient_eval:flag_key_for_context(Tag, FlagKey, ensure_context(Context), DefaultValue),
    % Send events
    SendEventsFun = fun(Event) -> ldclient_event_server:add_event(Tag, Event, #{include_reasons => true}) end,
    lists:foreach(SendEventsFun, Events),
    % Return evaluation detail
    Detail.

%% @doc Evaluate all flags for a given context and return their values
%%
%% Evaluates all existing flags, but does not create any events as a side
%% effect of the evaluation. It returns a map of flag keys to evaluated values.
%% @end
-spec all_flags_state(Context :: ldclient_user:user() | ldclient_context:context()) -> ldclient_eval:feature_flags_state().
all_flags_state(Context) ->
    all_flags_state(Context, ?DEFAULT_INSTANCE_NAME).

%% @doc Evaluate all flags for a given context and given client instance
%%
%% Evaluates all existing flags, but does not create any events as a side
%% effect of the evaluation. It returns a map of flag keys to evaluated values.
%% @end
-spec all_flags_state(Context :: ldclient_user:user() | ldclient_context:context(), Tag :: atom()) -> ldclient_eval:feature_flags_state().
all_flags_state(Context, Tag) ->
    ldclient_eval:all_flags_eval(ensure_context(Context), Tag).

%% @doc Returns an object that encapsulates the state of all feature flags for a given context.
%%
%% This includes the flag values, and also metadata that can be used on the front end.
%% The most common use case for this method is to bootstrap a set of client-side feature flags from a
%% back-end service.
%%
%% If you are not using this to boostrap a client, then you likely want all_flags_state/1 or all_flags_state/2.
%% @end
-spec all_flags_state(Context :: ldclient_user:user() | ldclient_context:context(),
    Options :: ldclient_eval:all_flags_state_options(), Tag :: atom()) -> ldclient_eval:feature_flags_state().
all_flags_state(Context, Options, Tag) ->
    ldclient_eval:all_flags_state(ensure_context(Context), Options, Tag).

%% @doc Identify reports details about a context
%%
%% This function uses the default client instance.
%% @end
-spec identify(Context :: ldclient_user:user() | ldclient_context:context()) -> ok.
identify(Context) ->
    identify(ensure_context(Context), ?DEFAULT_INSTANCE_NAME).

%% @doc Identify reports details about a context
%%
%% This is useful to report context to a specific client instance.
%% @end
-spec identify(Context :: ldclient_user:user() | ldclient_context:context(), Tag :: atom()) -> ok.
identify(Context, Tag) when is_atom(Tag) ->
    ConvertedContext = ensure_context(Context),
    when_is_valid_context(ConvertedContext, false, fun() ->
        Event = ldclient_event:new_identify(ConvertedContext),
        ldclient_event_server:add_event(Tag, Event, #{})
    end).

%% @doc Track reports that a context has performed an event
%%
%% Custom data can be attached to the event.
%% @end
-spec track(Key :: binary(), Context :: ldclient_user:user() | ldclient_context:context(), Data :: ldclient_event:event_data()) -> ok.
track(Key, Context, Data) when is_binary(Key), is_map(Data) ->
    track(Key, ensure_context(Context), Data, ?DEFAULT_INSTANCE_NAME).

%% @doc Track reports that a context has performed an event
%%
%% This is useful for specifying a specific client instance.
%% @end
-spec track(Key :: binary(), Context :: ldclient_user:user() | ldclient_context:context(), Data :: ldclient_event:event_data(), Tag :: atom()) -> ok.
track(Key, Context, Data, Tag) when is_atom(Tag), is_binary(Key) ->
    ConvertedContext = ensure_context(Context),
    when_is_valid_context(ConvertedContext, false, fun() ->
        Event = ldclient_event:new_custom(Key, ensure_context(Context), Data),
        ldclient_event_server:add_event(Tag, Event, #{})
    end).


%% @doc Reports that a context has performed an event, and associates it with a numeric value.
%%
%% This value is used by the LaunchDarkly experimentation feature in numeric custom metrics, and will also
%% be returned as part of the custom event for Data Export.
%%
%% Custom data can also be attached to the event.
%% @end
-spec track_metric(Key :: binary(), Context :: ldclient_user:user() | ldclient_context:context(), ldclient_event:event_data(), Metric :: number()) -> ok.
track_metric(Key, Context, Data, Metric) ->
    track_metric(Key, ensure_context(Context), Data, Metric, ?DEFAULT_INSTANCE_NAME).

%% @doc Reports that a context has performed an event, and associates it with a numeric value.
%%
%% This value is used by the LaunchDarkly experimentation feature in numeric custom metrics, and will also
%% be returned as part of the custom event for Data Export.
%%
%% Custom data can also be attached to the event.
%% @end
-spec track_metric(Key :: binary(), Context :: ldclient_user:user() | ldclient_context:context(), ldclient_event:event_data(), Metric :: number(), Tag :: atom()) -> ok.
track_metric(Key, Context, Data, Metric, Tag) ->
    ConvertedContext = ensure_context(Context),
    when_is_valid_context(ConvertedContext, false, fun() ->
        Event = ldclient_event:new_custom(Key, ensure_context(Context), Data, Metric),
        ldclient_event_server:add_event(Tag, Event, #{})
    end).

%% @doc Take a context or a context and return a context.
%%
%% If the context cannot be converted, then
%% @end
-spec ensure_context(UserOrContext :: ldclient_user:user() | ldclient_context:context()) -> ldclient_context:context().
ensure_context(#{kind := _Kind} = ContextOrContext) -> ContextOrContext;
ensure_context(UserOrContext) ->
    case maps:is_key(key, UserOrContext) of
        %% We allow legacy users to have an empty key, but we log it.
        false -> error_logger:warning_msg("Context key is blank. Flag evaluation will proceed, but the context will not be stored in Launchdarkly");
        true -> ok
    end,
    ldclient_context:new_from_user(UserOrContext).

%% @doc High order function that calls a function when a context is valid, otherwise returns true.
%%
%% @end
-spec when_is_valid_context(Context :: ldclient_context:context(), AllowEmptyKey :: boolean(), Fun :: function()) -> ok.
when_is_valid_context(Context, AllowEmptyKey, Fun) ->
    case ldclient_context:is_valid(Context, AllowEmptyKey) of
        true -> Fun();
        false -> ok
    end.
