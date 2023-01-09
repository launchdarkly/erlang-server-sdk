%%-------------------------------------------------------------------
%% @doc Event data type
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_event).

%% API
-export([new/4]).
-export([new/5]).
-export([new_for_unknown_flag/4]).
-export([new_flag_eval/6]).
-export([new_prerequisite_eval/6]).
-export([new_identify/1]).
-export([new_index/2]).
-export([new_custom/3]).
-export([new_custom/4]).
-export([strip_eval_reason/1]).

%% Types
-type event() :: feature_request() | identify() | index() | custom().

-type event_type() :: identify | index | feature_request | custom.

-type event_data() ::
    null
    | boolean()
    | integer()
    | float()
    | binary()
    | list()
    | map().

%% Event type

-type feature_request() :: #{
    type      := feature_request,
    timestamp := non_neg_integer(),
    context      := ldclient_context:context(),
    data      := #{
        key                     := ldclient_flag:key(),
        variation               := ldclient_flag:variation() | null,
        value                   := ldclient_eval:result_value(),
        default                 := ldclient_eval:result_value(),
        version                 := ldclient_flag:version() | null,
        prereq_of               := ldclient_flag:key() | null,
        trackEvents            := boolean() | null,
        debugEventsUntilDate := boolean() | null,
        eval_reason             := ldclient_eval:reason() | null,
        include_reason          := boolean(),
        debug                   := boolean()
    }
}.

-type identify() :: #{
    type      := identify,
    timestamp := non_neg_integer(),
    context      := ldclient_context:context()
}.

-type index() :: #{
    type      := index,
    timestamp := non_neg_integer(),
    context      := ldclient_context:context()
}.

-type custom() :: #{
    type         := custom,
    timestamp    := non_neg_integer(),
    key          := binary(),
    context         := ldclient_context:context(),
    data         => event_data(),
    metric_value => number()
}.

-export_type([event/0]).

%%===================================================================
%% API
%%===================================================================

%% @doc Creates a new event of a given type
%%
%% Valid event type, context and timestamp are required. Additional data must be
%% empty for `identify' and `index' events. `feature_request' events must
%% contain valid feature fields in `Data'. `custom' events may contain any
%% `Data' fields.
%% @end
-spec new(
    Type :: event_type(),
    Context :: ldclient_context:context(),
    Timestamp :: non_neg_integer(),
    Data :: event_data()
) -> event().
new(identify, Context, Timestamp, #{}) ->
    #{
        type      => identify,
        timestamp => Timestamp,
        context      => Context
    };
new(index, Context, Timestamp, #{}) ->
    #{
        type      => index,
        timestamp => Timestamp,
        context      => Context
    };
new(feature_request, Context, Timestamp, #{
    key                     := Key,                  % ldclient_flag:key()
    variation               := Variation,            % null | ldclient_flag:variation()
    value                   := Value,                % ldclient_eval:result_value()
    default                 := Default,              % ldclient_eval:result_value()
    version                 := Version,              % null | ldclient_flag:version()
    prereq_of               := PrereqOf,             % null | ldclient_flag:key()
    trackEvents            := TrackEvents,          % null | boolean()
    debugEventsUntilDate := DebugEventsUntilDate, % null | boolean()
    eval_reason             := EvalReason,           % null | ldclient_eval:reason()
    include_reason          := IncludeReason         % boolean()
}) ->
    #{
        type      => feature_request,
        timestamp => Timestamp,
        context      => Context,
        data      => #{
            key                     => Key,
            variation               => Variation,
            value                   => Value,
            default                 => Default,
            version                 => Version,
            prereq_of               => PrereqOf,
            trackEvents            => TrackEvents,
            debugEventsUntilDate => DebugEventsUntilDate,
            eval_reason             => EvalReason,
            include_reason          => IncludeReason,
            debug                   => false
        }
    }.

-spec new(
    custom,
    Key :: binary(),
    Context:: ldclient_context:context(),
    Timestamp :: non_neg_integer(),
    Data :: event_data()
) -> event().
new(custom, Key, Context, Timestamp, Data) ->
    #{
        type      => custom,
        timestamp => Timestamp,
        key       => Key,
        context      => Context,
        data      => Data
    }.

-spec new_for_unknown_flag(
    FlagKey :: ldclient_flag:key(),
    Context :: ldclient_context:context(),
    DefaultValue :: ldclient_eval:result_value(),
    Reason :: ldclient_eval:reason()
) -> event().
new_for_unknown_flag(FlagKey, Context, DefaultValue, Reason) ->
    EventData = #{
        key                     => FlagKey,
        variation               => null,
        value                   => DefaultValue,
        default                 => DefaultValue,
        version                 => null,
        prereq_of               => null,
        trackEvents            => null,
        debugEventsUntilDate => null,
        eval_reason             => Reason,
        include_reason          => false,
        debug                   => false
    },
    new(feature_request, Context, erlang:system_time(milli_seconds), EventData).

-spec new_flag_eval(
    VariationIndex :: ldclient_flag:variation(),
    VariationValue :: ldclient_eval:result_value(),
    DefaultValue :: ldclient_eval:result_value(),
    Context :: ldclient_context:context(),
    Reason :: ldclient_eval:reason() | null,
    Flag :: ldclient_flag:flag()
) -> event().
new_flag_eval(VariationIndex, VariationValue, DefaultValue, Context, Reason, #{
    key                     := Key,
    version                 := Version,
    trackEvents            := TrackEvents,
    debugEventsUntilDate := DebugEventsUntilDate
} = Flag) ->
    IsExperiment = is_experiment(Reason, Flag),
    EventData = #{
        key                     => Key,
        variation               => VariationIndex,
        value                   => VariationValue,
        default                 => DefaultValue,
        version                 => Version,
        prereq_of               => null,
        trackEvents            => TrackEvents or IsExperiment,
        debugEventsUntilDate => DebugEventsUntilDate,
        eval_reason             => Reason,
        include_reason          => IsExperiment,
        debug                   => false
    },
    new(feature_request, Context, erlang:system_time(milli_seconds), EventData).

-spec new_prerequisite_eval(
    VariationIndex :: ldclient_flag:variation(),
    VariationValue :: ldclient_flag:variation_value(),
    PrerequisiteOf :: ldclient_flag:key(),
    Context :: ldclient_context:context(),
    Reason :: ldclient_eval:reason() | null,
    Flag :: ldclient_flag:flag()
) -> event().
new_prerequisite_eval(VariationIndex, VariationValue, PrerequisiteOf, Context, Reason, #{
    key                     := Key,
    version                 := Version,
    trackEvents            := TrackEvents,
    debugEventsUntilDate := DebugEventsUntilDate
} = Flag) ->
    IsExperiment = is_experiment(Reason, Flag),
    EventData = #{
        key                     => Key,
        variation               => VariationIndex,
        value                   => VariationValue,
        default                 => null,
        version                 => Version,
        prereq_of               => PrerequisiteOf,
        trackEvents            => TrackEvents or IsExperiment,
        debugEventsUntilDate => DebugEventsUntilDate,
        eval_reason             => Reason,
        include_reason          => IsExperiment,
        debug                   => false
    },
    new(feature_request, Context, erlang:system_time(milli_seconds), EventData).

-spec new_identify(Context :: ldclient_context:context()) -> event().
new_identify(Context) ->
    new(identify, Context, erlang:system_time(milli_seconds), #{}).

-spec new_index(Context :: ldclient_context:context(), Timestamp :: non_neg_integer()) -> event().
new_index(Context, Timestamp) ->
    new(index, Context, Timestamp, #{}).

-spec new_custom(Key :: binary(), Context :: ldclient_context:context(), Data :: event_data() | undefined) -> event().
new_custom(Key, Context, null) when is_binary(Key) -> new_custom(Key, Context, undefined);
new_custom(Key, Context, undefined) when is_binary(Key) ->
    #{
        type      => custom,
        timestamp => erlang:system_time(milli_seconds),
        key       => Key,
        context      => Context
    };
new_custom(Key, Context, Data) when is_binary(Key) ->
    #{
        type      => custom,
        timestamp => erlang:system_time(milli_seconds),
        key       => Key,
        context      => Context,
        data      => Data
    }.

-spec new_custom(Key :: binary(), Context :: ldclient_context:context(), Data :: event_data() | undefined, MetricValue :: number()) -> event().
new_custom(Key, Context, null, MetricValue) when is_binary(Key), is_number(MetricValue) -> new_custom(Key, Context, undefined, MetricValue);
new_custom(Key, Context, undefined, MetricValue) when is_binary(Key), is_number(MetricValue) ->
    #{
        type         => custom,
        timestamp    => erlang:system_time(milli_seconds),
        key          => Key,
        context         => Context,
        metric_value => MetricValue
    };
new_custom(Key, Context, Data, MetricValue) when is_binary(Key), is_number(MetricValue) ->
    #{
        type         => custom,
        timestamp    => erlang:system_time(milli_seconds),
        key          => Key,
        context         => Context,
        data         => Data,
        metric_value => MetricValue
    }.

-spec strip_eval_reason(ldclient_event:event()) -> ldclient_event:event().
strip_eval_reason(#{type := feature_request, data := Data} = Event) ->
    Event#{data => maps:remove(eval_reason, Data)}.

%%===================================================================
%% Internal functions
%%===================================================================

-spec is_experiment(ldclient_eval:reason(), ldclient_flag:flag()) -> boolean().
%% If the reason indicates it is inExperiment, then it is. Otherwise use legacy logic.
is_experiment({rule_match, _, _, in_experiment}, _) -> true;
is_experiment({fallthrough, in_experiment}, _) -> true;
is_experiment(fallthrough, #{trackEventsFallthrough := true}) -> true;
is_experiment({rule_match, RuleIndex, _}, #{rules := Rules}) when RuleIndex >=0, RuleIndex < length(Rules) ->
    Rule = lists:nth(RuleIndex + 1, Rules),
    maps:get(trackEvents, Rule, false);
is_experiment(_Reason, _Flag) -> false.
