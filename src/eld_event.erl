%%-------------------------------------------------------------------
%% @doc Event data type
%%
%% @end
%%-------------------------------------------------------------------

-module(eld_event).

%% API
-export([new/4]).
-export([new/5]).
-export([new_for_unknown_flag/4]).
-export([new_flag_eval/6]).
-export([new_prerequisite_eval/6]).
-export([new_identify/1]).
-export([new_custom/3]).

%% Types
-type event() :: #{
    type      => event_type(),
    timestamp => non_neg_integer(),
    key       => binary(),
    user      => eld_user:user(),
    data      => map()
}.

-type event_type() :: identify | index | feature_request | custom.
%% Event type

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
    User :: eld_user:user(),
    Timestamp :: non_neg_integer(),
    Data :: map()
) -> event().
new(identify, User, Timestamp, #{}) ->
    #{
        type      => identify,
        timestamp => Timestamp,
        user      => User
    };
new(index, User, Timestamp, #{}) ->
    #{
        type      => index,
        timestamp => Timestamp,
        user      => User
    };
new(feature_request, User, Timestamp, #{
    key                     := Key,                  % eld_flag:key()
    variation               := Variation,            % undefined | eld_flag:variation()
    value                   := Value,                % undefined | eld_flag:variation_value()
    default                 := Default,              % undefined | eld_flag:variation_value()
    version                 := Version,              % undefined | eld_flag:version()
    prereq_of               := PrereqOf,             % undefined | eld_flag:key()
    track_events            := TrackEvents,          % undefined | boolean()
    debug_events_until_date := DebugEventsUntilDate, % undefined | boolean()
    eval_reason             := EvalReason,           % undefined | eld_eval:reason()
    debug                   := Debug                 % undefined | boolean()
}) ->
    #{
        type      => feature_request,
        timestamp => Timestamp,
        user      => User,
        data      => #{
            key                    => Key,
            variation              => Variation,
            value                  => Value,
            default                => Default,
            version                => Version,
            prereq_of              => PrereqOf,
            track_events           => TrackEvents,
            debug_events_util_date => DebugEventsUntilDate,
            eval_reason            => EvalReason,
            debug                  => Debug
        }
    }.

-spec new(
    custom,
    Key :: binary(),
    User:: eld_user:user(),
    Timestamp :: non_neg_integer(),
    Data :: map()
) -> event().
new(custom, Key, User, Timestamp, Data) when is_map(Data) ->
    #{
        type      => custom,
        timestamp => Timestamp,
        key       => Key,
        user      => User,
        data      => Data
    }.

-spec new_for_unknown_flag(
    FlagKey :: eld_flag:key(),
    User :: eld_user:user(),
    DefaultValue :: eld_flag:variation_value(),
    Reason :: eld_eval:reason()
) -> event().
new_for_unknown_flag(FlagKey, User, DefaultValue, Reason) ->
    EventData = #{
        key                     => FlagKey,
        variation               => undefined,
        value                   => undefined,
        default                 => DefaultValue,
        version                 => undefined,
        prereq_of               => undefined,
        track_events            => undefined,
        debug_events_until_date => undefined,
        eval_reason             => Reason,
        debug                   => false
    },
    new(feature_request, User, erlang:system_time(milli_seconds), EventData).

-spec new_flag_eval(
    VariationIndex :: eld_flag:variation(),
    VariationValue :: eld_flag:variation_value(),
    DefaultValue :: eld_flag:variation_value(),
    User :: eld_user:user(),
    Reason :: eld_eval:reason(),
    Flag :: eld_flag:flag()
) -> event().
new_flag_eval(VariationIndex, VariationValue, DefaultValue, User, Reason, #{
    key                     := Key,
    version                 := Version,
    track_events            := TrackEvents,
    debug_events_until_date := DebugEventsUntilDate
}) ->
    EventData = #{
        key                     => Key,
        variation               => VariationIndex,
        value                   => VariationValue,
        default                 => DefaultValue,
        version                 => Version,
        prereq_of               => undefined,
        track_events            => TrackEvents,
        debug_events_until_date => DebugEventsUntilDate,
        eval_reason             => Reason,
        debug                   => false
    },
    new(feature_request, User, erlang:system_time(milli_seconds), EventData).

-spec new_prerequisite_eval(
    VariationIndex :: eld_flag:variation(),
    VariationValue :: eld_flag:variation_value(),
    PrerequisiteOf :: eld_flag:key(),
    User :: eld_user:user(),
    Reason :: eld_eval:reason(),
    Flag :: eld_flag:flag()
) -> event().
new_prerequisite_eval(VariationIndex, VariationValue, PrerequisiteOf, User, Reason, #{
    key                     := Key,
    version                 := Version,
    track_events            := TrackEvents,
    debug_events_until_date := DebugEventsUntilDate
}) ->
    EventData = #{
        key                     => Key,
        variation               => VariationIndex,
        value                   => VariationValue,
        default                 => undefined,
        version                 => Version,
        prereq_of               => PrerequisiteOf,
        track_events            => TrackEvents,
        debug_events_until_date => DebugEventsUntilDate,
        eval_reason             => Reason,
        debug                   => false
    },
    new(feature_request, User, erlang:system_time(milli_seconds), EventData).

-spec new_identify(User :: eld_user:user()) -> event().
new_identify(User) ->
    new(identify, User, erlang:system_time(milli_seconds), #{}).

-spec new_custom(Key :: binary(), User :: eld_user:user(), Data :: map()) -> event().
new_custom(Key, User, Data) when is_binary(Key), is_map(Data) ->
    new(custom, Key, User, erlang:system_time(milli_seconds), Data).