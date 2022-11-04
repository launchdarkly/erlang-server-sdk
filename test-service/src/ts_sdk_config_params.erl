%-------------------------------------------------------------------
%% @doc `ts_sdk_config_params' module
%%
%% Parsers and types for configuration parameters.
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ts_sdk_config_params).

-export([
    parse_config_params/1,
    to_ldclient_options/1
]).

-type sdk_config_params() :: #{
    credential => binary() | undefined,
    start_wait_time_ms => pos_integer() | undefined, %% TODO: Implement
    init_can_fail => boolean(), %% TODO: Implement
    streaming => sdk_config_streaming_params(),
    events => sdk_config_event_params()
}.

-type sdk_config_streaming_params() :: #{
    base_uri => binary() | undefined,
    initial_retry_delay_ms => pos_integer() | undefined %% Not supported
}.

-type sdk_config_event_params() :: #{
    base_uri => binary() | undefined,
    capacity => pos_integer() | undefined,
    enable_diagnostics => true | false | undefined, %% Not supported
    all_attributes_private => true | false | undefined,
    global_private_attributes => [any()] | undefined,
    flush_interval_ms => pos_integer() | undefined,
    inline_users => true | false | undefined
}.

-export_type([sdk_config_params/0]).
-export_type([sdk_config_streaming_params/0]).
-export_type([sdk_config_event_params/0]).

-spec null_to_undefined(Item :: any()) -> any().
null_to_undefined(null) -> undefined;
null_to_undefined(Item) -> Item.

-spec map_get_null_default(Name :: binary(), Map :: map(), Default :: any()) -> any().
map_get_null_default(Name, Map, Default) ->  null_to_undefined(maps:get(Name, Map, Default)).

-spec parse_config_params(Params :: map()) -> sdk_config_params().
parse_config_params(Params) ->
    Credential = map_get_null_default(<<"credential">>, Params, undefined),
    StartWaitTimeMS = map_get_null_default(<<"startWaitTimeMs">>, Params, undefined),
    InitCanFail = map_get_null_default(<<"initCanFail">>, Params, undefined),
    Streaming = parse_config_streaming_params(Params),
    Events = parse_config_events_params(Params),
    #{
        credential => Credential,
        start_wait_time_ms => StartWaitTimeMS,
        init_can_fail => InitCanFail,
        streaming => Streaming,
        events => Events
    }.

-spec parse_config_streaming_params(Params :: map()) -> sdk_config_streaming_params().
parse_config_streaming_params(#{<<"streaming">> := StreamingParams} = _Params) ->
    BaseUri = map_get_null_default(<<"baseUri">>, StreamingParams, undefined),
    InitialRetryDelayMs = map_get_null_default(<<"initialRetryDelayMs">>, StreamingParams, undefined),
    #{
        base_uri => BaseUri,
        initial_retry_delay_ms => InitialRetryDelayMs
    };
parse_config_streaming_params(_Params) -> #{base_uri => undefined, initial_retry_delay_ms => undefined}.

-spec parse_config_events_params(Params :: map()) -> sdk_config_event_params().

parse_config_events_params(#{<<"events">> := EventParams} = _Params) when EventParams =/= null  ->
    BaseUri = map_get_null_default(<<"baseUri">>, EventParams, undefined),
    Capacity = map_get_null_default(<<"capacity">>, EventParams, undefined),
    EnableDiagnostics = map_get_null_default(<<"enableDiagnostics">>, EventParams, undefined),
    AllAttributesPrivate = map_get_null_default(<<"allAttributesPrivate">>, EventParams, undefined),
    GlobalPrivateAttributes = map_get_null_default(<<"globalPrivateAttributes">>, EventParams, undefined),
    FlushIntervalMS = map_get_null_default(<<"flushIntervalMs">>, EventParams, undefined),
    InlineUsers = map_get_null_default(<<"inlineUsers">>, EventParams, undefined),
    #{
        base_uri => BaseUri,
        capacity => Capacity,
        enable_diagnostics => EnableDiagnostics,
        all_attributes_private => AllAttributesPrivate,
        global_private_attributes => GlobalPrivateAttributes,
        flush_interval_ms => FlushIntervalMS,
        inline_users => InlineUsers
    };
parse_config_events_params(_Params) ->
    #{
        base_uri => undefined,
        capacity => undefined,
        enable_diagnostics => undefined,
        all_attributes_private => undefined,
        global_private_attributes => undefined,
        flush_interval_ms => undefined,
        inline_users => undefined
    }.

-spec to_ldclient_options(Configuration :: sdk_config_params()) -> map().
to_ldclient_options(Configuration) ->
    WithEventsUri = add_events_uri(Configuration, #{}),
    WithStreamUri = add_stream_uri(Configuration, WithEventsUri),
    WithEventsCapacity = add_events_capacity(Configuration, WithStreamUri),
    WithPrivateAttributes = add_private_attributes(Configuration, WithEventsCapacity),
    WithFlushInterval = add_events_flush_interval(Configuration, WithPrivateAttributes),
    WithInlineUsers = add_inline_users(Configuration, WithFlushInterval),
    WithStreamRetryDelay = add_stream_retry_delay(Configuration, WithInlineUsers),
    WithStreamRetryDelay.

-spec add_stream_retry_delay(Configuration :: sdk_config_params(), Options:: map()) -> map().
add_stream_retry_delay(#{streaming := #{
    initial_retry_delay_ms := InitialRetryDelayMs
}} = _Configuration, Options) when is_integer(InitialRetryDelayMs) ->
    Options#{
        stream_initial_retry_delay_ms => InitialRetryDelayMs
    };
add_stream_retry_delay(_Configuration, Options) -> Options.

-spec add_stream_uri(Configuration :: sdk_config_params(), Options :: map()) -> map().
add_stream_uri(#{streaming := #{
    base_uri := BaseUri
}} = _SdkConfigParams, Options) when is_binary(BaseUri) ->
    Options#{stream_uri => binary_to_list(BaseUri), stream => true};
add_stream_uri(_SdkConfigParams, Options) -> Options.

-spec add_events_uri(Configuration :: sdk_config_params(), Options :: map()) -> map().
add_events_uri(#{events := #{
    base_uri := BaseUri
} = _EventParams} = _SdkConfigParams, Options) when is_binary(BaseUri) ->
    Options#{events_uri => binary_to_list(BaseUri)};
add_events_uri(_SdkConfigParams, Options) -> Options.

-spec add_events_capacity(Configuration :: sdk_config_params(), Options :: map()) -> map().
add_events_capacity(#{events := #{
    capacity := Capacity
}} = _SdkConfigParams, Options) when is_integer(Capacity) ->
    Options#{events_capacity => Capacity};
add_events_capacity(_SdkConfigParams, Options) -> Options.

-spec add_private_attributes(Configuration :: sdk_config_params(), Options :: map()) -> map().
add_private_attributes(#{events := #{
    all_attributes_private := AllAttributesPrivate
}} = _SdkConfigParams, Options) when is_boolean(AllAttributesPrivate) ->
    case AllAttributesPrivate of
        true -> Options#{private_attributes => all};
        false -> Options
    end;
add_private_attributes(#{events := #{
    global_private_attributes := GlobalPrivateAttributes
}} = _SdkConfigParams, Options) when is_list(GlobalPrivateAttributes) ->
    Options#{private_attributes => GlobalPrivateAttributes};
add_private_attributes(_SdkConfigParams, Options) -> Options.

-spec add_events_flush_interval(Configuration :: sdk_config_params(), Options :: map()) -> map().
add_events_flush_interval(#{events := #{
    flush_interval_ms := FlushInterval
}} = _SdkConfigParams, Options) when is_number(FlushInterval) ->
    Options#{events_flush_interval => FlushInterval};
add_events_flush_interval(_SdkConfigParams, Options) -> Options.

-spec add_inline_users(Configuration :: sdk_config_params(), Options :: map()) -> map().
add_inline_users(#{events := #{
    inline_users := InlineUsers
}} = _SdkConfigParams, Options) when is_boolean(InlineUsers) ->
    Options#{inline_users_in_events => InlineUsers};
add_inline_users(_SdkConfigParams, Options) -> Options.
