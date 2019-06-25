%%-------------------------------------------------------------------
%% @doc Flag data type
%%
%% @end
%%-------------------------------------------------------------------

-module(eld_flag).

%% API
-export([new/2]).
-export([get_variation/2]).

%% Types
-type flag() :: #{
    debug_events_until_date => pos_integer() | null,
    deleted                 => boolean(),
    fallthrough             => variation_or_rollout(),
    key                     => key(),
    off_variation           => variation(),
    on                      => boolean(),
    prerequisites           => [prerequisite()],
    rules                   => [eld_rule:rule()],
    salt                    => binary(),
    sel                     => binary(),
    targets                 => [target()],
    track_events            => boolean(),
    variations              => [variation_value()],
    version                 => version()
}.

-type key() :: binary().
%% Flag key

-type variation() :: non_neg_integer() | null.
%% Variation index

-type variation_or_rollout() :: variation() | eld_rollout:rollout().
%% Contains either the fixed variation or percent rollout to serve

-type prerequisite() :: #{
    key       => key(),
    variation => variation()
}.
%% Describes a requirement that another feature flag return a specific variation

-type target() :: #{
    values    => [eld_user:key()],
    variation => variation()
}.
%% Describes a set of users who will receive a specific variation

-type variation_value() ::
    boolean()
    | integer()
    | float()
    | binary()
    | list()
    | map().

-type version() :: pos_integer().

-export_type([flag/0]).
-export_type([key/0]).
-export_type([prerequisite/0]).
-export_type([target/0]).
-export_type([variation/0]).
-export_type([variation_or_rollout/0]).
-export_type([variation_value/0]).
-export_type([version/0]).

%%===================================================================
%% API
%%===================================================================

-spec new(Key :: eld_flag:key(), Properties :: map()) -> flag().
new(Key, #{
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
        fallthrough             => parse_variation_or_rollout(Fallthrough),
        key                     => Key,
        off_variation           => OffVariation,
        on                      => On,
        prerequisites           => parse_prerequisites(Prerequisites),
        rules                   => parse_rules(Rules),
        salt                    => Salt,
        sel                     => Sel,
        targets                 => parse_targets(Targets),
        track_events            => TrackEvents,
        variations              => Variations,
        version                 => Version
    }.

-spec get_variation(Flag :: flag(), VariationIndex :: non_neg_integer()) -> term().
get_variation(_Flag, Variation) when Variation < 0 -> null;
get_variation(#{variations := Variations}, VariationIndex)
    when VariationIndex + 1 > length(Variations) -> null;
get_variation(#{variations := Variations}, VariationIndex) ->
    lists:nth(VariationIndex + 1, Variations).

%%===================================================================
%% Internal functions
%%===================================================================

-spec parse_prerequisites([map()]) -> [prerequisite()].
parse_prerequisites(Prerequisites) ->
    F = fun(#{<<"key">> := Key, <<"variation">> := Variation}) ->
            #{key => Key, variation => Variation}
        end,
    lists:map(F, Prerequisites).

-spec parse_rules([map()]) -> [eld_rule:rule()].
parse_rules(Rules) ->
    F = fun(Rule) -> eld_rule:new(Rule) end,
    lists:map(F, Rules).

-spec parse_targets([map()]) -> [target()].
parse_targets(Targets) ->
    F = fun(#{<<"values">> := Values, <<"variation">> := Variation}) ->
            #{values => Values, variation => Variation}
        end,
    lists:map(F, Targets).

-spec parse_variation_or_rollout(map()) -> variation_or_rollout().
parse_variation_or_rollout(#{<<"variation">> := Variation}) when is_integer(Variation) ->
    Variation;
parse_variation_or_rollout(#{<<"rollout">> := Rollout}) when is_map(Rollout) ->
    eld_rollout:new(Rollout).
