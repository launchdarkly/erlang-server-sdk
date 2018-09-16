%%%-------------------------------------------------------------------
%%% @doc Flag data type
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(eld_flag).

%% API
-export([new/1, get_variation/2]).

%% Types
-type flag() :: #{
    debug_events_until_date => pos_integer() | undefined,
    deleted                 => boolean(),
    fallthrough             => variation_or_rollout(),
    key                     => key(),
    off_variation           => variation(),
    on                      => boolean(),
    prerequisites           => [prerequisite()],
    rules                   => [rule()],
    salt                    => binary(),
    sel                     => binary(),
    targets                 => [target()],
    track_events            => boolean(),
    variations              => [variation_value()],
    version                 => pos_integer()
}.

-type key() :: binary().
%% Flag key

-type variation() :: pos_integer().
%% Variation number

-type rule() :: #{
    clauses              => [clause()],
    variation_or_rollout => variation_or_rollout()
}.
%% Expresses a set of AND-ed matching conditions for a user, along with either
%% a fixed variation or a set of rollout percentages

-type clause() :: #{
    attribute => binary(),
    op        => operator(),
    values    => [variation_value()],
    negate    => boolean()
}.
%% Describes an individual clause within a targeting rule

-type operator() :: in | ends_with | starts_with | matches | contains
    | less_than | less_than_or_equal | greater_than | greater_than_or_equal
    | before | 'after' | segment_match | semver_equal | semver_less_than
    | semver_greater_than.
%% List of available operators

-type variation_or_rollout() :: variation() | rollout().
%% Contains either the fixed variation or percent rollout to serve

-type rollout() :: #{
    variations => [weighted_variation()],
    bucket_by  => binary() | undefined
}.
%% Describes how users will be bucketed into variations during a percentage rollout

-type weighted_variation() :: #{
    variation => variation(),
    weight    => non_neg_integer() % 0 to 100000
}.
%% Describes a fraction of users who will receive a specific variation

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
    | {json, binary()}.

-export_type([flag/0]).
-export_type([variation_value/0]).

%%%===================================================================
%%% API
%%%===================================================================

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
