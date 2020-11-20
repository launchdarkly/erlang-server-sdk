%%-------------------------------------------------------------------
%% @doc Flag data type
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_flag).

%% API
-export([new/1]).
-export([get_variation/2]).

%% Types
-type flag() :: #{
    debug_events_until_date  => pos_integer() | null,
    deleted                  => boolean(),
    fallthrough              => variation_or_rollout(),
    key                      => key(),
    off_variation            => variation(),
    on                       => boolean(),
    prerequisites            => [prerequisite()],
    rules                    => [ldclient_rule:rule()],
    salt                     => binary(),
    targets                  => [target()],
    track_events             => boolean(),
    track_events_fallthrough => boolean(),
    variations               => [variation_value()],
    version                  => version()
}.

-type key() :: binary().
%% Flag key

-type variation() :: non_neg_integer() | null.
%% Variation index

-type variation_or_rollout() :: variation() | ldclient_rollout:rollout().
%% Contains either the fixed variation or percent rollout to serve

-type prerequisite() :: #{
    key       => key(),
    variation => variation()
}.
%% Describes a requirement that another feature flag return a specific variation

-type target() :: #{
    values    => [ldclient_user:key()],
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

-type version() :: non_neg_integer().

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

-spec new(RawFlagMap :: map()) -> flag().
new(RawFlagMap) ->
    FlagTemplate = #{
        <<"debugEventsUntilDate">>   => null,
        <<"deleted">>                => false,
        <<"fallthrough">>            => #{},
        <<"key">>                    => <<>>,
        <<"offVariation">>           => 0,
        <<"on">>                     => false,
        <<"prerequisites">>          => [],
        <<"rules">>                  => [],
        <<"salt">>                   => <<>>,
        <<"targets">>                => [],
        <<"trackEvents">>            => false,
        <<"trackEventsFallthrough">> => false,
        <<"variations">>             => [],
        <<"version">>                => 0
    },
    FlagMap = maps:merge(FlagTemplate, RawFlagMap),
    new_from_template(FlagMap).

-spec get_variation(Flag :: flag(), VariationIndex :: non_neg_integer()|null) -> term().
get_variation(_Flag, null) -> null;
get_variation(_Flag, Variation) when is_integer(Variation), Variation < 0 -> null;
get_variation(#{variations := Variations}, VariationIndex)
    when is_integer(VariationIndex), VariationIndex + 1 > length(Variations) -> null;
get_variation(#{variations := Variations}, VariationIndex) when is_integer(VariationIndex) ->
    lists:nth(VariationIndex + 1, Variations).

%%===================================================================
%% Internal functions
%%===================================================================

-spec new_from_template(FlagMap :: map()) -> flag().
new_from_template(#{
    <<"debugEventsUntilDate">>   := DebugEventsUntilDate,
    <<"deleted">>                := Deleted,
    <<"fallthrough">>            := Fallthrough,
    <<"key">>                    := Key,
    <<"offVariation">>           := OffVariation,
    <<"on">>                     := On,
    <<"prerequisites">>          := Prerequisites,
    <<"rules">>                  := Rules,
    <<"salt">>                   := Salt,
    <<"targets">>                := Targets,
    <<"trackEvents">>            := TrackEvents,
    <<"trackEventsFallthrough">> := TrackEventsFallthrough,
    <<"variations">>             := Variations,
    <<"version">>                := Version
}) ->
    #{
        debug_events_until_date  => DebugEventsUntilDate,
        deleted                  => Deleted,
        fallthrough              => parse_variation_or_rollout(Fallthrough),
        key                      => Key,
        off_variation            => OffVariation,
        on                       => On,
        prerequisites            => parse_prerequisites(Prerequisites),
        rules                    => parse_rules(Rules),
        salt                     => Salt,
        targets                  => parse_targets(Targets),
        track_events             => TrackEvents,
        track_events_fallthrough => TrackEventsFallthrough,
        variations               => Variations,
        version                  => Version
    }.

-spec parse_prerequisites([map()]) -> [prerequisite()].
parse_prerequisites(Prerequisites) ->
    F = fun(#{<<"key">> := Key, <<"variation">> := Variation}, Acc) ->
            [#{key => Key, variation => Variation}|Acc];
        (_, Acc) ->
            Acc
        end,
    lists:foldr(F, [], Prerequisites).

-spec parse_rules([map()]) -> [ldclient_rule:rule()].
parse_rules(Rules) ->
    F = fun(#{<<"clauses">> := Clauses} = Rule, Acc) when is_list(Clauses) ->
            [ldclient_rule:new(Rule)|Acc];
        (_, Acc) ->
            Acc
        end,
    lists:foldr(F, [], Rules).

-spec parse_targets([map()]) -> [target()].
parse_targets(Targets) ->
    F = fun(#{<<"values">> := Values, <<"variation">> := Variation}, Acc) ->
            [#{values => Values, variation => Variation}|Acc];
        (_, Acc) ->
            Acc
        end,
    lists:foldr(F, [], Targets).

-spec parse_variation_or_rollout(map()) -> variation_or_rollout().
parse_variation_or_rollout(#{<<"variation">> := Variation}) when is_integer(Variation) -> Variation;
parse_variation_or_rollout(#{<<"rollout">> := #{<<"variations">> := Variations} = Rollout}) when is_list(Variations) ->
    ldclient_rollout:new(Rollout);
parse_variation_or_rollout(_) -> ldclient_rollout:new(#{<<"variations">> => []}).
