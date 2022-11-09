%-------------------------------------------------------------------
%% @doc `ts_command_params' module
%%
%% Parsers and types for command parameters.
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ts_command_params).

%% API
-export([
    parse_command/1,
    format_evaluate_flag_response/1
]).

-type command() :: evaluate | evaluate_all | identify_event | custom_event | flush_events.

-type command_params() :: #{
    command := command(),
    evaluate => evaluate_flag_params(),
    evaluate_all => evaluate_all_flags_params(),
    custom_event => custom_event_params(),
    identify_event => identify_event_params(),
}.

-type evaluate_flag_params() :: #{
    flag_key := binary(),
    user => ldclient_user:user(),
    value_type := binary(),
    default_value := ldclient_flag:variation_value(),
    detail := boolean()
}.

-type evaluate_flag_response() :: #{
    value := ldclient_flag:variation_value(),
    variationIndex => ldclient_eval:variation_index(),
    reason => ldclient_eval:reason()
}.

-type evaluate_all_flags_params() :: #{
    user => ldclient_user:user(),
    with_reasons := boolean(),
    client_side_only := boolean(),
    details_only_for_tracked_flags := boolean()
}.

-type evaluate_all_flags_response() :: #{
    %% TODO: All flags  state is not implemented. When it is this
    %% will need a proper type.
    state => map()
}.

-type custom_event_params() :: #{
    event_key := binary(),
    user => ldclient_user:user(),
    data => ldclient_flag:variation_value(),
    omit_null_data := boolean(),
    metric_value => float()
}.

-type identify_event_params() :: #{
    user := ldclient_user:user()
}.

-export_type([command_params/0]).
-export_type([evaluate_flag_params/0]).
-export_type([evaluate_flag_response/0]).
-export_type([evaluate_all_flags_params/0]).
-export_type([evaluate_all_flags_response/0]).
-export_type([custom_event_params/0]).
-export_type([identify_event_params/0]).

-spec parse_command(Command :: map()) -> command_params().
parse_command(#{<<"command">> := <<"evaluate">>,
    <<"evaluate">> := Evaluate} = _Command) -> #{
        command => evaluate,
        evaluate => parse_evaluate(Evaluate)
    };
parse_command(#{<<"command">> := <<"evaluateAll">>,
    <<"evaluateAll">> := EvaluateAll} = _Command) -> #{
        command => evaluate_all,
        evaluate_all => parse_evaluate_all(EvaluateAll)
    };
parse_command(#{<<"command">> := <<"identifyEvent">>,
    <<"identifyEvent">> := IdentifyEvent} = _Command) -> #{
        command => identify_event,
        identify_event => parse_identify_event(IdentifyEvent)
    };
parse_command(#{<<"command">> := <<"customEvent">>,
    <<"customEvent">> := CustomEvent} = _Command) -> #{
        command => custom_event,
        custom_event => parse_custom_event(CustomEvent)
    };
parse_command(#{<<"command">> := <<"flushEvents">>} = _Command) -> #{
    command => flush_events
};
parse_command(_Command) ->
    %% TODO: Provide some detail.
    error.

-spec parse_evaluate(Evaluate :: map()) -> evaluate_flag_params().
parse_evaluate(Evaluate) ->
    Parsed = #{
        flag_key => maps:get(<<"flagKey">>, Evaluate, <<>>),
        value_type => maps:get(<<"valueType">> , Evaluate, <<>>),
        default_value => maps:get(<<"defaultValue">>, Evaluate, <<>>),
        detail => maps:get(<<"detail">>, Evaluate, false)
    },
    maybe_add_user(parse_user(Evaluate), Parsed).

-spec maybe_add_user(User :: ldclient_user:user() | undefined, Map :: map()) -> map().
maybe_add_user(undefined, Map) -> Map;
maybe_add_user(User, Map) -> Map#{user => User}.

-spec parse_user_with_key(Container ::
    identify_event_params()
    | custom_event_params()
    | evaluate_flag_params()
    | evaluate_all_flags_params(),
    UserKey :: binary()) -> ldclient_user:user() | undefined.
parse_user_with_key(Container, UserKey) ->
    #{UserKey := User} = Container,
    User = maps:get(UserKey, Container, undefined),
    parse_user_map(User).

-spec parse_user_map(User :: map() | undefined) -> ldclient_user:user() | undefined.
parse_user_map(undefined) -> undefined;
parse_user_map(User) ->
    UserWithKey = #{
        key => maps:get(<<"key">>, User)
    },
    UserWithSecondary = parse_optional(<<"secondary">>, secondary, User, UserWithKey),
    UserWithIp = parse_optional(<<"ip">>, ip, User, UserWithSecondary),
    UserWithCountry = parse_optional(<<"country">>, country, User, UserWithIp),
    UserWithEmail = parse_optional(<<"email">>, email, User, UserWithCountry),
    UserWithFirstName = parse_optional(<<"firstName">>, first_name, User, UserWithEmail),
    UserWithLastName = parse_optional(<<"lastName">>, last_name, User, UserWithFirstName),
    UserWithAvatar = parse_optional(<<"avatar">>, avatar, User, UserWithLastName),
    UserWithName = parse_optional(<<"name">>, name, User, UserWithAvatar),
    UserWithAnonymous = parse_optional(<<"anonymous">>, anonymous, User, UserWithName),
    UserWithCustom = parse_optional(<<"custom">>, custom, User, UserWithAnonymous),
    parse_optional(<<"privateAttributeNames">>, private_attribute_names, User, UserWithCustom).

-spec parse_user(Container ::
identify_event_params()
| custom_event_params()
| evaluate_flag_params()
| evaluate_all_flags_params()) -> ldclient_user:user() | undefined.
parse_user(Container) ->
    parse_user_with_key(Container, <<"user">>).


%% Treats null as not included.
-spec parse_optional(InKey :: binary(), OutKey :: atom(),
    Input :: map(), Output :: map()) -> map().
parse_optional(InKey, OutKey, Input, Output) ->
    Value = maps:get(InKey, Input, undefined),
    add_if_defined(OutKey, Value, Output, false).

%% Conditionally will include null values.
-spec parse_optional(InKey :: binary(), OutKey :: atom(),
    Input :: map(), Output :: map(), IncludeNull :: boolean()) -> map().
parse_optional(InKey, OutKey, Input, Output, IncludeNull) ->
    Value = maps:get(InKey, Input, undefined),
    add_if_defined(OutKey, Value, Output, IncludeNull).


-spec add_if_defined(Key :: atom(), Value :: any(), Output :: map(), IncludeNull :: boolean()) -> map().
add_if_defined(_Key, undefined, Output, _) -> Output;
add_if_defined(_Key, null, Output, false) -> Output;
add_if_defined(Key, Value, Output, _) -> Output#{Key => Value}.

-spec parse_evaluate_all(EvaluateAll :: map()) -> evaluate_all_flags_params().
parse_evaluate_all(EvaluateAll) ->
    maybe_add_user(parse_user(EvaluateAll), #{
        with_reasons => maps:get(<<"withReasons">>, EvaluateAll, false),
        client_side_only => maps:get(<<"clientSideOnly">>, EvaluateAll, false),
        details_only_for_tracked_flags => maps:get(<<"detailsOnlyForTrackedFlags">>, EvaluateAll, false)
    }).

-spec parse_identify_event(IdentifyEvent :: map()) -> identify_event_params().
parse_identify_event(IdentifyEvent) ->
    #{
        user => parse_user(IdentifyEvent)
    }.

-spec parse_custom_event(CustomEvent :: map()) -> custom_event_params().
parse_custom_event(CustomEvent) ->
    CustomEventWithKey = #{
        event_key => maps:get(<<"eventKey">>, CustomEvent, <<>>),
        omit_null_value => maps:get(<<"omitNullData">>, CustomEvent, false)
    },
    CustomEventWithUser = maybe_add_user(parse_user(CustomEvent), CustomEventWithKey),
    CustomEventWithData = parse_optional(<<"data">>, data, CustomEvent, CustomEventWithUser, true),
    CustomEventWithOmitNullData = parse_optional(<<"omitNullData">>, omit_null_data,
        CustomEvent, CustomEventWithData),
    CustomEventWithMetricValue = parse_optional(<<"metricValue">>, metric_value,
        CustomEvent, CustomEventWithOmitNullData),
    CustomEventWithMetricValue.

-spec format_evaluate_flag_response(Result ::
    ldclient_eval:detail()
    | ldclient_eval:result_value()) -> evaluate_flag_response().
format_evaluate_flag_response({VariationIndex, Value, Reason}) ->
    #{
        value => Value,
        variationIndex => VariationIndex,
        reason => ldclient_eval_reason:format(Reason)
    };
format_evaluate_flag_response({VariationIndex, Value}) ->
    #{
        value => Value,
        variationIndex => VariationIndex
    };
format_evaluate_flag_response(Value) ->
    #{
        value => Value
    }.
