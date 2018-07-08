%%%-------------------------------------------------------------------
%%% @doc Flag data type
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(eld_flag).

-export([new/1, get_variation/2]).

-type flag() :: #{
    debug_events_until_date => null | pos_integer(),
    deleted                 => boolean(),
    fallthrough             => tuple(),
    key                     => binary(),
    off_variation           => pos_integer(),
    on                      => boolean(),
    prerequisites           => list(),
    rules                   => list(),
    salt                    => binary(),
    sel                     => binary(),
    targets                 => list(),
    track_events            => boolean(),
    variations              => list(),
    version                 => pos_integer()
}.
-export_type([flag/0]).

-spec new(map()) -> flag().
new(#{
    <<"debugEventsUntilDate">> := DebugEventsUntilDate,
    <<"deleted">>              := Deleted,
    <<"fallthrough">>          := Fallthrough,
    <<"key">>                  := Key,
    <<"offVariation">>         := OffVariation,
    <<"on">>                   := On,
    <<"prerequisites">>        := Prerequisites,
    <<"rules">>                := Rules,
    <<"salt">>                 := Salt,
    <<"sel">>                  := Sel,
    <<"targets">>              := Targets,
    <<"trackEvents">>          := TrackEvents,
    <<"variations">>           := Variations,
    <<"version">>              := Version
}) ->
    #{
        debug_events_until_date => DebugEventsUntilDate,
        deleted                 => Deleted,
        fallthrough             => Fallthrough,
        key                     => Key,
        off_variation           => OffVariation,
        on                      => On,
        prerequisites           => Prerequisites,
        rules                   => Rules,
        salt                    => Salt,
        sel                     => Sel,
        targets                 => Targets,
        track_events            => TrackEvents,
        variations              => Variations,
        version                 => Version
    }.

-spec get_variation(Flag :: flag(), VariationNumber :: pos_integer()) -> term().
get_variation(#{variations := Variations}, VariationNumber) ->
    lists:nth(VariationNumber, Variations).
