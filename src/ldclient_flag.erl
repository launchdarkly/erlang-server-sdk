%%-------------------------------------------------------------------
%% @doc Flag data type
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_flag).

%% API
-export([new/1]).
-export([new_simple/2]).
-export([get_variation/2]).
-export([without_null_properties/1]).

%% Types
-type flag() :: #{
    debugEventsUntilDate     => pos_integer() | null,
    deleted                  => boolean(),
    fallthrough              => variation_or_rollout(),
    key                      => key(),
    offVariation             => variation(),
    on                       => boolean(),
    prerequisites            => [prerequisite()],
    rules                    => [ldclient_rule:rule()],
    salt                     => binary(),
    targets                  => [target()],
    contextTargets           => [target()],
    trackEvents              => boolean(),
    trackEventsFallthrough   => boolean(),
    variations               => [variation_value()],
    version                  => version(),
    clientSideAvailability   => client_side_availability()
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
    contextKind => ldclient_context:kind_value(),
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

-type client_side_availability() :: #{
    usingEnvironmentId => boolean(),
    usingMobileKey => boolean()
}.
%% Describes the availability of the flag to client-side SDKs.

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
        <<"offVariation">>           => null,
        <<"on">>                     => false,
        <<"prerequisites">>          => [],
        <<"rules">>                  => [],
        <<"salt">>                   => <<>>,
        <<"targets">>                => [],
        <<"contextTargets">>         => [],
        <<"trackEvents">>            => false,
        <<"trackEventsFallthrough">> => false,
        <<"variations">>             => [],
        <<"version">>                => 0,
        <<"clientSideAvailability">> => #{
            <<"usingEnvironmentId">> => false,
            <<"usingMobileKey">> => false
        }
    },
    FlagMap = maps:merge(FlagTemplate, without_null_properties(RawFlagMap)),
    new_from_template(FlagMap).

-spec new_simple(Key :: key(), Value :: variation_value()) -> flag().
new_simple(Key, Value) ->
    new(#{
        <<"key">> => Key,
        <<"on">> => true,
        <<"variations">> => [Value],
        <<"fallthrough">> => #{
            <<"variation">> => 0
        }
    }).

%% @doc Removes the properties whose value is null.
%%
%% A null property in flag or segment data is the same as an omitted property.
%% This applies only to the top level of the map. Values in lists, such as
%% variations and clause values, do not change.
%% @end
-spec without_null_properties(map()) -> map().
without_null_properties(RawMap) ->
    maps:filter(fun(_Key, Value) -> Value =/= null end, RawMap).

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
    <<"contextTargets">>         := ContextTargets,
    <<"trackEvents">>            := TrackEvents,
    <<"trackEventsFallthrough">> := TrackEventsFallthrough,
    <<"variations">>             := Variations,
    <<"version">>                := Version,
    <<"clientSideAvailability">> := ClientSideAvailability
}) ->
    #{
        debugEventsUntilDate     => DebugEventsUntilDate,
        deleted                  => Deleted,
        fallthrough              => parse_variation_or_rollout(Fallthrough),
        key                      => Key,
        offVariation             => OffVariation,
        on                       => On,
        prerequisites            => parse_prerequisites(Prerequisites),
        rules                    => parse_rules(Rules),
        salt                     => Salt,
        targets                  => parse_targets(Targets),
        contextTargets           => parse_targets(ContextTargets),
        trackEvents              => TrackEvents,
        trackEventsFallthrough   => TrackEventsFallthrough,
        variations               => Variations,
        version                  => Version,
        clientSideAvailability   => parse_client_side_availability(ClientSideAvailability)
    }.

-spec parse_client_side_availability(ClientSideAvailability :: map()) -> client_side_availability().
parse_client_side_availability(RawClientSideAvailability) ->
    ClientSideAvailability = without_null_properties(RawClientSideAvailability),
    UsingEnvironmentId = maps:get(<<"usingEnvironmentId">>, ClientSideAvailability, false),
    UsingMobileKey = maps:get(<<"usingMobileKey">>, ClientSideAvailability, false),
    #{
        usingEnvironmentId => UsingEnvironmentId,
        usingMobileKey => UsingMobileKey
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
    F = fun(#{<<"values">> := Values, <<"variation">> := Variation} = Target, Acc) ->
            [#{
                contextKind => maps:get(<<"contextKind">>, without_null_properties(Target), <<"user">>),
                values => Values,
                variation => Variation
            }|Acc];
        (_, Acc) ->
            Acc
        end,
    lists:foldr(F, [], Targets).

-spec parse_variation_or_rollout(map()) -> variation_or_rollout().
parse_variation_or_rollout(#{<<"variation">> := Variation}) when is_integer(Variation) -> Variation;
parse_variation_or_rollout(#{<<"rollout">> := #{<<"variations">> := Variations} = Rollout}) when is_list(Variations) ->
    ldclient_rollout:new(Rollout);
parse_variation_or_rollout(Variation) when is_integer(Variation) -> Variation;
parse_variation_or_rollout(_) -> ldclient_rollout:new(#{<<"variations">> => []}).
