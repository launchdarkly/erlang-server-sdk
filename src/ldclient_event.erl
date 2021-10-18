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
-export([new_alias/2]).

%% Types
-type event() :: feature_request() | identify() | index() | custom() | alias().

-type event_type() :: identify | index | feature_request | custom | alias.
%% Event type

-type feature_request() :: #{
    type      := feature_request,
    timestamp := non_neg_integer(),
    user      := ldclient_user:user(),
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
    user      := ldclient_user:user()
}.

-type index() :: #{
    type      := index,
    timestamp := non_neg_integer(),
    user      := ldclient_user:user()
}.

-type custom() :: #{
    type         := custom,
    timestamp    := non_neg_integer(),
    key          := binary(),
    user         := ldclient_user:user(),
    data         => map(),
    metric_value => number()
}.

-type alias() :: #{
    type            := alias,
    timestamp       := non_neg_integer(),
    user            := ldclient_user:user(),
    previous_user   := ldclient_user:user()
}.

-export_type([event/0]).

%%===================================================================
%% API
%%===================================================================

%% @doc Creates a new event of a given type
%%
%% Valid event type, user and timestamp are required. Additional data must be
%% empty for `identify' and `index' events. `feature_request' events must
%% contain valid feature fields in `Data'. `custom' events may contain any
%% `Data' fields.
%% @end
-spec new(
    Type :: event_type(),
    User :: ldclient_user:user(),
    Timestamp :: non_neg_integer(),
    Data :: map()
) -> event().
new(identify, User, Timestamp, #{}) ->
    #{
        type      => identify,
        timestamp => Timestamp,
        user      => ldclient_user:event_format(User)
    };
new(index, User, Timestamp, #{}) ->
    #{
        type      => index,
        timestamp => Timestamp,
        user      => ldclient_user:event_format(User)
    };
new(feature_request, User, Timestamp, #{
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
        user      => ldclient_user:event_format(User),
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
    User:: ldclient_user:user(),
    Timestamp :: non_neg_integer(),
    Data :: map()
) -> event().
new(custom, Key, User, Timestamp, Data) when is_map(Data) ->
    #{
        type      => custom,
        timestamp => Timestamp,
        key       => Key,
        user      => ldclient_user:event_format(User),
        data      => Data
    }.

-spec new_for_unknown_flag(
    FlagKey :: ldclient_flag:key(),
    User :: ldclient_user:user(),
    DefaultValue :: ldclient_eval:result_value(),
    Reason :: ldclient_eval:reason()
) -> event().
new_for_unknown_flag(FlagKey, User, DefaultValue, Reason) ->
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
    new(feature_request, User, erlang:system_time(milli_seconds), EventData).

-spec new_flag_eval(
    VariationIndex :: ldclient_flag:variation(),
    VariationValue :: ldclient_eval:result_value(),
    DefaultValue :: ldclient_eval:result_value(),
    User :: ldclient_user:user(),
    Reason :: ldclient_eval:reason() | null,
    Flag :: ldclient_flag:flag()
) -> event().
new_flag_eval(VariationIndex, VariationValue, DefaultValue, User, Reason, #{
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
    new(feature_request, User, erlang:system_time(milli_seconds), EventData).

-spec new_prerequisite_eval(
    VariationIndex :: ldclient_flag:variation(),
    VariationValue :: ldclient_flag:variation_value(),
    PrerequisiteOf :: ldclient_flag:key(),
    User :: ldclient_user:user(),
    Reason :: ldclient_eval:reason() | null,
    Flag :: ldclient_flag:flag()
) -> event().
new_prerequisite_eval(VariationIndex, VariationValue, PrerequisiteOf, User, Reason, #{
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
    new(feature_request, User, erlang:system_time(milli_seconds), EventData).

-spec new_identify(User :: ldclient_user:user()) -> event().
new_identify(User) ->
    new(identify, User, erlang:system_time(milli_seconds), #{}).

-spec new_index(User :: ldclient_user:user(), Timestamp :: non_neg_integer()) -> event().
new_index(User, Timestamp) ->
    new(index, User, Timestamp, #{}).

-spec new_custom(Key :: binary(), User :: ldclient_user:user(), Data :: map()) -> event().
new_custom(Key, User, Data) when is_binary(Key), is_map(Data) ->
    #{
        type      => custom,
        timestamp => erlang:system_time(milli_seconds),
        key       => Key,
        user      => ldclient_user:event_format(User),
        data      => Data
    }.

-spec new_custom(Key :: binary(), User :: ldclient_user:user(), Data :: map(), MetricValue :: number()) -> event().
new_custom(Key, User, Data, MetricValue) when is_binary(Key), is_map(Data), is_number(MetricValue) ->
    #{
        type         => custom,
        timestamp    => erlang:system_time(milli_seconds),
        key          => Key,
        user         => ldclient_user:event_format(User),
        data         => Data,
        metric_value => MetricValue
    }.

-spec strip_eval_reason(ldclient_event:event()) -> ldclient_event:event().
strip_eval_reason(#{type := feature_request, data := Data} = Event) ->
    Event#{data => maps:remove(eval_reason, Data)}.

-spec new_alias(User :: ldclient_user:user(), PreviousUser :: ldclient_user:user()) -> event().
new_alias(User, PreviousUser) -> 
    #{ 
        type            => alias,
        timestamp       => erlang:system_time(milli_seconds),
        user            => ldclient_user:event_format(User),
        previous_user   => ldclient_user:event_format(PreviousUser)
    }.

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
