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
-export([new_index/2]).
-export([new_custom/3]).
-export([strip_eval_reason/1]).

%% Types
-type event() :: #{
    type      := event_type(),
    timestamp := non_neg_integer(),
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
    variation               := Variation,            % null | eld_flag:variation()
    value                   := Value,                % eld_eval:result_value()
    default                 := Default,              % eld_eval:result_value()
    version                 := Version,              % null | eld_flag:version()
    prereq_of               := PrereqOf,             % null | eld_flag:key()
    track_events            := TrackEvents,          % null | boolean()
    debug_events_until_date := DebugEventsUntilDate, % null | boolean()
    eval_reason             := EvalReason            % null | eld_eval:reason()
}) ->
    #{
        type      => feature_request,
        timestamp => Timestamp,
        user      => User,
        data      => #{
            key                     => Key,
            variation               => Variation,
            value                   => Value,
            default                 => Default,
            version                 => Version,
            prereq_of               => PrereqOf,
            track_events            => TrackEvents,
            debug_events_until_date => DebugEventsUntilDate,
            eval_reason             => EvalReason,
            debug                   => false
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
    DefaultValue :: eld_eval:result_value(),
    Reason :: eld_eval:reason()
) -> event().
new_for_unknown_flag(FlagKey, User, DefaultValue, Reason) ->
    EventData = #{
        key                     => FlagKey,
        variation               => null,
        value                   => DefaultValue,
        default                 => DefaultValue,
        version                 => null,
        prereq_of               => null,
        track_events            => null,
        debug_events_until_date => null,
        eval_reason             => Reason,
        debug                   => false
    },
    new(feature_request, User, erlang:system_time(milli_seconds), EventData).

-spec new_flag_eval(
    VariationIndex :: eld_flag:variation(),
    VariationValue :: eld_eval:result_value(),
    DefaultValue :: eld_eval:result_value(),
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
        prereq_of               => null,
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
        default                 => null,
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

-spec new_index(User :: eld_user:user(), Timestamp :: non_neg_integer()) -> event().
new_index(User, Timestamp) ->
    new(index, User, Timestamp, #{}).

-spec new_custom(Key :: binary(), User :: eld_user:user(), Data :: map()) -> event().
new_custom(Key, User, Data) when is_binary(Key), is_map(Data) ->
    new(custom, Key, User, erlang:system_time(milli_seconds), Data).

-spec strip_eval_reason(eld_event:event()) -> eld_event:event().
strip_eval_reason(#{type := feature_request, data := Data} = Event) ->
    Event#{data => maps:remove(eval_reason, Data)}.
