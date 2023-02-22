%%-------------------------------------------------------------------
%% @doc Flagbuilder
%%
%% @end
%%-------------------------------------------------------------------

-module(ldclient_flagbuilder).

% Internal functions
-export([new/1, key/1, build/2]).

% Public API
-export([
    boolean_flag/1,
    on/2,
    off_variation/2,
    fallthrough_variation/2,
    variations/2,
    variation_for_all/2,
    value_for_all/2,
    variation_for_context/4,
    if_match/3,
    if_not_match/3,
    and_match/3,
    and_not_match/3,
    then_return/2,
    clear_rules/1,
    clear_targets/1,
    if_match/4,
    if_not_match/4,
    and_match/4,
    and_not_match/4
]).

-export_type([flag_builder/0, flag_rule_builder/0]).


-opaque flag_builder() :: #{
        key => binary(),
        on  => boolean(),
        variations => [ldclient_flag:variations()],
        off_variation => non_neg_integer(),
        fallthrough_variation => non_neg_integer(),
        rules => [ldclient_rules:rule()],
        context_targets => builder_targets()
}. %% A builder for feature flag configurations to be used with {@link ldclient_testdata}.

-type builder_targets() :: #{
        Variation :: non_neg_integer() => #{
        ContextKind :: ldclient_context:kind_value() => sets:set()
    }
}. %% Representation of targets used by the flag builder.

%% In the LaunchDarkly model, a flag can have any number of rules, and a rule can have any number of
%% clauses. A clause is an individual test such as "name is 'X'". A rule matches a user if all of the
%% rule's clauses match the user.
%%
%% To start defining a rule, use either {@link if_match/4} or {@link if_not_match/4}.
%% This defines the first clause for the rule.
%% Optionally, you may add more clauses with {@link and_match/4} or {@link and_not_match/4} .
%% Finally, call {@link then_return/2} to finish defining the rule.
-opaque flag_rule_builder() :: #{
        variation => non_neg_integer(),
        clauses => [ldclient_clause:clause()],
        flag => flag_builder()
}. %% A builder for feature flag rules to be used with {@link flag_builder()}.

-ifdef(TEST).
-compile(export_all).
-endif.

%% @doc Create a new flag builder.
%%
%% This method is used by other SDK modules, but should not be used by non-SDK code.
%% @private
%% @end
-spec new(FlagName :: binary() | string()) -> flag_builder().
new(FlagName) when is_list(FlagName) ->
    new(list_to_binary(FlagName));
new(FlagName) -> #{
    key => FlagName,
    on  => true,
    fallthrough_variation => 0,
    off_variation => 1,
    variations => [true, false]
}.

%% @doc Build a flag from a flag builder.
%%
%% This method is used by other SDK modules, but should not be used by non-SDK code.
%% @private
%% @end
-spec build(Flag :: flag_builder(), Version :: non_neg_integer()) -> ldclient_flag:flag().
build(Flag = #{ key := Key,
                on := On,
                variations := Variations,
                off_variation := OffVariation,
                fallthrough_variation := FallthroughVariation }, Version) ->
    Rules = maps:get(rules, Flag, []),
    {ContextTargets, UserTargets} = build_targets(maps:get(context_targets, Flag, #{})),
    #{
        key => Key,
        version => Version,
        on => On,
        variations => Variations,
        offVariation => OffVariation,
        fallthrough => FallthroughVariation,
        trackEvents => false,
        trackEventsFallthrough => false,
        deleted => false,
        debugEventsUntilDate => null,
        prerequisites => [],
        salt => <<"salt">>,
        rules => Rules,
        contextTargets => ContextTargets,
        targets => UserTargets
     }.

%% @doc Set the key on the flag builder.
%%
%% This method is used by other SDK modules, but should not be used by non-SDK code.
%% @private
%% @end
-spec key(FlagBuilder :: flag_builder()) -> binary().
key(#{key := FlagName}) ->
    FlagName.

%% @doc Sets targeting to be on or off for this flag.
%%
%% The effect of this depends on the rest of the flag configuration, just as it does on the
%% real LaunchDarkly dashboard. In the default configuration that you get from calling
%% {@link ldclient_testdata:flag/2} with a new flag key, the flag will return `false'
%% whenever targeting is off, and `true' when targeting is on.
%%
%% @param IsOn true if targeting should be on
%% @param FlagBuilder the flag builder to modify
%% @returns the modified builder
%%
%% @end
-spec on(IsOn :: boolean(), FlagBuilder :: flag_builder()) -> flag_builder().
on(IsOn, FlagBuilder) ->
    FlagBuilder#{on := IsOn}.

-spec is_boolean_flag(FlagBuilder :: flag_builder()) -> boolean().
is_boolean_flag(#{ variations := [true, false]}) -> true;
is_boolean_flag(_) -> false.

%% @doc Removes any existing rules from the flag.
%%
%% This undoes the effect of {@link if_match/3} and {@link if_not_match/3}.
%%
%% @param FlagBuilder the flag builder to modify
%% @returns the modified builder
%% @end
-spec clear_rules(FlagBuilder :: flag_builder()) -> flag_builder().
clear_rules(FlagBuilder) ->
    maps:remove(rules, FlagBuilder).

%% @doc Removes any existing targets from the flag.
%%
%% This undoes the effect of {@link variation_for_context/4}.
%%
%% @param FlagBuilder the flag builder to modify
%% @returns the modified builder
%% @end
-spec clear_targets(FlagBuilder :: flag_builder()) -> flag_builder().
clear_targets(FlagBuilder) ->
    maps:remove(context_targets, FlagBuilder).

%% @doc A shortcut for setting the flag to use the standard boolean configuration.
%%
%% This is the default for all new flags created with {@link ldclient_testdata:flag/2}.
%% The flag will have two variations, `true' and `false' (in that order); it will return
%% `false' whenever targeting is off, and `true' when targeting is on if no other
%% settings specify otherwise.
%%
%% @param FlagBuilder the flag builder to modify
%% @returns the modified builder
%% @end
-spec boolean_flag(FlagBuilder :: flag_builder()) -> flag_builder().
boolean_flag(FlagBuilder) ->
    case is_boolean_flag(FlagBuilder) of
        true -> FlagBuilder;
        false -> fallthrough_variation(0,
                  off_variation(1,
                  variations([true, false],
                  FlagBuilder)))
    end.

-spec variation_for_boolean(Variation :: boolean()) -> non_neg_integer().
variation_for_boolean(true) -> 0;
variation_for_boolean(false) -> 1.

-type variation() :: boolean() | non_neg_integer().

%% @doc Specifies the off variation for a flag.
%%
%% The off variation is the value that is returned whenever targeting is off
%%
%% If the flag was previously configured with other variations and a boolean Variation is specified,
%% this also changes the FlagBuilder to a boolean flag.
%%
%% @param Variation `true', `false', or the index of the desired variation to return: 0 for the first, 1 for the second, etc.
%% @param FlagBuilder the flag builder to modify
%% @returns the modified builder
%% @end
-spec off_variation(Variation :: variation(), FlagBuilder :: flag_builder()) -> flag_builder().
off_variation(Variation, FlagBuilder) when is_boolean(Variation) ->
    off_variation(variation_for_boolean(Variation), boolean_flag(FlagBuilder));
off_variation(Variation, FlagBuilder) when is_integer(Variation) ->
    FlagBuilder#{off_variation := Variation}.

%% @doc Specifies the fallthrough variation for a flag.
%%
%% The fallthrough is the value that is returned if targeting is on
%% and the user was not matched by a more specific target or rule.
%%
%% If the flag was previously configured with other variations and a boolean variation is specified,
%% this also changes the flagbuilder to a boolean flag.
%%
%% @param Variation `true', `false', or the index of the desired variation to return: 0 for the first, 1 for the second, etc.
%% @param FlagBuilder the flag builder to modify
%% @returns the modified builder
%% @end
-spec fallthrough_variation(Variation :: variation(), FlagBuilder :: flag_builder()) -> flag_builder().
fallthrough_variation(Variation, FlagBuilder) when is_boolean(Variation) ->
    fallthrough_variation(variation_for_boolean(Variation), boolean_flag(FlagBuilder));
fallthrough_variation(Variation, FlagBuilder) when is_integer(Variation) ->
    FlagBuilder#{fallthrough_variation := Variation}.

%% @doc Sets the flag to always return the specified variation value for all contexts.
%%
%% The value may be of any JSON type.
%% This method changes the flag to have only a single variation, which is this value,
%% and to return the same variation regardless of whether targeting is on or off.
%% Any existing targets or rules are removed.
%%
%% @param Value the desired value to be returned for all contexts
%% @param FlagBuilder the flag builder to modify
%% @returns the modified builder
%% @end
-spec variations(Values :: [ldclient_flag:variation_value()], FlagBuilder :: flag_builder()) -> flag_builder().
variations(Values, FlagBuilder) ->
    FlagBuilder#{variations := Values}.

%% @doc Sets the flag to always return the specified variation for all contexts.
%%
%% The variation is set, targeting is switched on, and any existing targets or rules are removed.
%% The fallthrough variation is set to the specified value.
%% The off variation is left unchanged.
%%
%% If the flag was previously configured with other variations and a boolean variation is specified,
%% this also changes the flagbuilder to a boolean flag.
%%
%% @param Variation `true', `false', or the index of the desired variation to return: 0 for the first, 1 for the second, etc.
%% @param FlagBuilder the flag builder to modify
%% @returns the modified builder
%% @end
-spec variation_for_all(Variation :: variation(), FlagBuilder :: flag_builder()) -> flag_builder().
variation_for_all(Variation, FlagBuilder) when is_boolean(Variation) ->
    variation_for_all(variation_for_boolean(Variation), boolean_flag(FlagBuilder));
variation_for_all(Variation, FlagBuilder) when is_integer(Variation) ->
    Fallthrough = fallthrough_variation(Variation, FlagBuilder),
    NoTargets = clear_targets(Fallthrough),
    NoRules = clear_rules(NoTargets),
    on(true, NoRules).

%% @doc Sets the flag to always return the specified variation value for all contexts.
%%
%% The value may be of any JSON type, as defined by }. This method changes the
%% flag to have only a single variation, which is this value, and to return the same
%% variation regardless of whether targeting is on or off. Any existing targets or rules
%% are removed.
%%
%% @param Value the desired value to be returned for all contexts
%% @param FlagBuilder the flag builder to modify
%% @returns the modified builder
%% @end
-spec value_for_all(Value :: term(), FlagBuilder :: flag_builder()) -> flag_builder().
value_for_all(Value, FlagBuilder) ->
    variation_for_all(0, variations([Value], FlagBuilder)).

%% @doc Sets the flag to return the specified variation for a specific context kind and key when
%% targeting is on.
%%
%% This has no effect when targeting is turned off for the flag.
%%
%% If the flag was previously configured with other variations and a boolean variation is specified,
%% this also changes the flagbuilder to a boolean flag.
%%
%% @param Variation `true', `false', or the index of the desired variation to return: 0 for the first, 1 for the second, etc.
%% @param ContextKind the kind of context to target
%% @param ContextKey the key of the context to target
%% @param FlagBuilder the flag builder to modify
%% @returns the modified builder
%% @end
-spec variation_for_context(
    Variation :: variation(),
    ContextKind :: ldclient_context:kind_value(),
    ContextKey :: binary(),
    FlagBuilder :: flag_builder()
) -> flag_builder().
variation_for_context(Variation, ContextKind, ContextKey, FlagBuilder) when is_boolean(Variation) ->
    variation_for_context(variation_for_boolean(Variation), ContextKind, ContextKey, FlagBuilder);
variation_for_context(Variation, ContextKind, ContextKey, FlagBuilder) ->
    ContextTargets = maps:get(context_targets, FlagBuilder, #{}),
    TargetsForVariation = maps:get(Variation, ContextTargets, #{}),
    KeysForKind = maps:get(ContextKind, TargetsForVariation, sets:new()),
    FlagBuilder#{
        context_targets => ContextTargets#{Variation => #{ContextKind => sets:add_element(ContextKey, KeysForKind)}}
    }.

%% @doc Starts defining a flag rule, using the "is one of" operator. The kind of the context is implicitly "user"
%% for non-user contexts use {@link if_match/4}.
%%
%% For example, this creates a rule that returns `true' if the name is "Patsy" or "Edina":
%%
%% ```
%%     {ok, Flag} = ldclient_testdata:flag(TestData, <<"flag">>),
%%     RuleBuilder = ldclient_flagbuilder:if_match(<<"name">>, [<<"Patsy">>, <<"Edina">>], Flag),
%%     UpdatedFlag = ldclient_flagbuilder:then_return(true, RuleBuilder),
%%     ldclient_testdata:update(TestData, UpdatedFlag).
%% '''
%%
%% @param ContextAttribute the context attribute to match against
%% @param Values values to compare to
%% @param FlagBuilder the flag builder to modify
%% @returns a {@link flag_rule_builder()}; call {@link then_return/2} to finish the rule,
%%          or add more tests with {@link and_match/4} or {@link and_not_match/4}.
%% @end
-spec if_match(
    ContextAttribute :: ldclient_context:attribute_key(),
    Values :: [term()],
    FlagBuilder :: flag_builder()) -> flag_rule_builder().
if_match(ContextAttribute, Values, FlagBuilder) ->
    if_match(<<"user">>, ContextAttribute, Values, FlagBuilder).

%% @doc Starts defining a flag rule, using the "is one of" operator.
%%
%% For example, this creates a rule that returns `true' if the name is "Patsy" or "Edina":
%%
%% ```
%%     {ok, Flag} = ldclient_testdata:flag(TestData, <<"flag">>),
%%     RuleBuilder = ldclient_flagbuilder:if_match(<<"user">>, <<"name">>, [<<"Patsy">>, <<"Edina">>], Flag),
%%     UpdatedFlag = ldclient_flagbuilder:then_return(true, RuleBuilder),
%%     ldclient_testdata:update(TestData, UpdatedFlag).
%% '''
%%
%% @param ContextAttribute the context attribute to match against
%% @param Values values to compare to
%% @param FlagBuilder the flag builder to modify
%% @returns a {@link flag_rule_builder()}; call {@link then_return/2} to finish the rule,
%%          or add more tests with {@link and_match/4} or {@link and_not_match/4}.
%% @end
-spec if_match(
    ContextKind :: ldclient_context:kind_value(),
    ContextAttribute :: ldclient_context:attribute_key(),
    Values :: [term()],
    FlagBuilder :: flag_builder()) -> flag_rule_builder().
if_match(ContextKind, ContextAttribute, Values, FlagBuilder) ->
    and_match(ContextKind, ContextAttribute, Values, #{ flag => FlagBuilder }).

%% @doc Starts defining a flag rule, using the "is not one of" operator. The kind of the context is implicitly
%% "user" for non-user contexts use {@link if_not_match/4}.
%%
%% For example, this creates a rule that returns `true' if the name is neither "Saffron" nor "Bubble":
%%
%% ```
%%     {ok, Flag} = ldclient_testdata:flag(TestData, <<"flag">>),
%%     RuleBuilder = ldclient_flagbuilder:if_not_match(<<"name">>, [<<"Saffron">>, <<"Bubble">>], Flag),
%%     UpdatedFlag = ldclient_flagbuilder:then_return(true, RuleBuilder),
%%     ldclient_testdata:update(TestData, UpdatedFlag).
%% '''
%%
%% @param ContextAttribute the context attribute to match against
%% @param Values values to compare to
%% @param FlagBuilder the flag builder to modify
%% @returns a {@link flag_rule_builder()}; call {@link then_return/2} to finish the rule,
%%          or add more tests with {@link and_match/4} or {@link and_not_match/4}.
%% @end
-spec if_not_match(
    ContextAttribute :: ldclient_context:attribute_key(),
    Values :: [term()],
    FlagBuilder :: flag_builder()) -> flag_rule_builder().
if_not_match(ContextAttribute, Values, FlagBuilder) ->
    if_not_match(<<"user">>, ContextAttribute, Values, FlagBuilder).

%% @doc Starts defining a flag rule, using the "is not one of" operator.
%%
%% For example, this creates a rule that returns `true' if the name is neither "Saffron" nor "Bubble":
%%
%% ```
%%     {ok, Flag} = ldclient_testdata:flag(TestData, <<"flag">>),
%%     RuleBuilder = ldclient_flagbuilder:if_not_match(<<"user">>, <<"name">>, [<<"Saffron">>, <<"Bubble">>], Flag),
%%     UpdatedFlag = ldclient_flagbuilder:then_return(true, RuleBuilder),
%%     ldclient_testdata:update(TestData, UpdatedFlag).
%% '''
%%
%% @param ContextAttribute the context attribute to match against
%% @param Values values to compare to
%% @param FlagBuilder the flag builder to modify
%% @returns a {@link flag_rule_builder()}; call {@link then_return/2} to finish the rule,
%%          or add more tests with {@link and_match/4} or {@link and_not_match/4}.
%% @end
-spec if_not_match(
    ContextKind :: ldclient_context:kind_value(),
    ContextAttribute :: ldclient_context:attribute_key(),
    Values :: [term()],
    FlagBuilder :: flag_builder()) -> flag_rule_builder().
if_not_match(ContextKind, ContextAttribute, Values, FlagBuilder) ->
    and_not_match(ContextKind, ContextAttribute, Values, #{ flag => FlagBuilder }).

%%-------------------------------------------------------------------
%% Flag Rule Builder
%%-------------------------------------------------------------------

%% @doc Adds another clause, using the "is one of" operator. The kind of the context is implicitly "user"
%% for non-user contexts use {@link any_match/4}.
%%
%% For example, this creates a rule that returns `true' if the name is "Patsy" and the
%% country is "gb":
%%
%% ```
%%     {ok, Flag} = ldclient_testdata:flag(TestData, <<"flag">>),
%%     RuleBuilder = ldclient_flagbuilder:and_match(<<"country">>, [<<"gb">>],
%%                   ldclient_flagbuilder:if_match(<<"name">>, [<<"Patsy">>], Flag)),
%%     UpdatedFlag = ldclient_flagbuilder:then_return(true, RuleBuilder),
%%     ldclient_testdata:update(TestData, UpdatedFlag).
%% '''
%%
%% @param ContextAttribute the context attribute to match against
%% @param Values values to compare to
%% @param RuleBuilder the rule builder to modify
%% @returns the modified rule builder
%% @end
-spec and_match(
    ContextAttribute :: ldclient_context:attribute_key(),
    Values :: [term()],
    RuleBuilder :: flag_rule_builder()) -> flag_rule_builder().
and_match(ContextAttribute, Values, RuleBuilder) ->
    and_match(<<"user">>, ContextAttribute, Values, RuleBuilder).

%% @doc Adds another clause, using the "is one of" operator. The kind of the context is implicitly "user"
%% for non-user contexts use {@link any_match/4}.
%%
%% For example, this creates a rule that returns `true' if the name is "Patsy" and the
%% country is "gb":
%%
%% ```
%%     {ok, Flag} = ldclient_testdata:flag(TestData, <<"flag">>),
%%     RuleBuilder = ldclient_flagbuilder:and_match(<<"user">>, <<"country">>, [<<"gb">>],
%%                   ldclient_flagbuilder:if_match(<<"user">>, <<"name">>, [<<"Patsy">>], Flag)),
%%     UpdatedFlag = ldclient_flagbuilder:then_return(true, RuleBuilder),
%%     ldclient_testdata:update(TestData, UpdatedFlag).
%% '''
%%
%% @param ContextAttribute the context attribute to match against
%% @param Values values to compare to
%% @param RuleBuilder the rule builder to modify
%% @returns the modified rule builder
-spec and_match(
    ContextKind:: ldclient_context:kind_value(),
    ContextAttribute :: ldclient_context:attribute_key(),
    Values :: [term()], RuleBuilder :: flag_rule_builder()) -> flag_rule_builder().
and_match(ContextKind, ContextAttribute, Values, RuleBuilder) ->
    Clauses = maps:get(clauses, RuleBuilder, []),
    maps:put(clauses, [new_clause(ContextKind, ContextAttribute, Values, false)|Clauses], RuleBuilder).

%% @doc Adds another clause, using the "is not one of" operator. The kind of the context is implicitly "user"
%% for non-user contexts use {@link any_match/4}.
%%
%% For example, this creates a rule that returns `true' if the name is "Patsy" and the
%% country is not "gb":
%%
%% ```
%%     {ok, Flag} = ldclient_testdata:flag(TestData, <<"flag">>),
%%     RuleBuilder = ldclient_flagbuilder:and_not_match(<<"country">>, [<<"gb">>],
%%                   ldclient_flagbuilder:if_match(<<"name">>, [<<"Patsy">>], Flag)),
%%     UpdatedFlag = ldclient_flagbuilder:then_return(true, RuleBuilder),
%%     ldclient_testdata:update(TestData, UpdatedFlag).
%% '''
%%
%% @param ContextAttribute the context attribute to match against
%% @param Values values to compare to
%% @param RuleBuilder the rule builder to modify
%% @returns the modified rule builder
%% @end
-spec and_not_match(
    ContextAttribute :: ldclient_context:attribute_key(),
    Values :: [term()],
    RuleBuilder :: flag_rule_builder()) -> flag_rule_builder().
and_not_match(ContextAttribute, Values, RuleBuilder) ->
    and_not_match(<<"user">>, ContextAttribute, Values, RuleBuilder).

%% @doc Adds another clause, using the "is not one of" operator.
%%
%% For example, this creates a rule that returns `true' if the name is "Patsy" and the
%% country is not "gb":
%%
%% ```
%%     {ok, Flag} = ldclient_testdata:flag(TestData, <<"flag">>),
%%     RuleBuilder = ldclient_flagbuilder:and_not_match(<<"user">>, <<"country">>, [<<"gb">>],
%%                   ldclient_flagbuilder:if_match(<<"user">>, <<"name">>, [<<"Patsy">>], Flag)),
%%     UpdatedFlag = ldclient_flagbuilder:then_return(true, RuleBuilder),
%%     ldclient_testdata:update(TestData, UpdatedFlag).
%% '''
%%
%% @param ContextAttribute the context attribute to match against
%% @param Values values to compare to
%% @param RuleBuilder the rule builder to modify
%% @returns the modified rule builder
%% @end
-spec and_not_match(
    ContextKind :: ldclient_context:kind_value(),
    ContextAttribute :: ldclient_context:attribute_key(),
    Values :: [term()],
    RuleBuilder :: flag_rule_builder()) -> flag_rule_builder().
and_not_match(ContextKind, ContextAttribute, Values, RuleBuilder) ->
    Clauses = maps:get(clauses, RuleBuilder, []),
    maps:put(clauses, [new_clause(ContextKind, ContextAttribute, Values, true)|Clauses], RuleBuilder).

%% @doc Finishes defining the rule, specifying the result variation.
%%
%% If the flag was previously configured with other variations and a boolean variation is specified,
%% this also changes the FlagBuilder to a boolean flag.
%%
%% @param Variation `true', `false', or the index of the desired variation to return: 0 for the first, 1 for the second, etc.
%% @param RuleBuilder the rule builder to use
%% @returns the modified flag builder that initially created this rule builder
%% @end
-spec then_return(Variation :: variation(), RuleBuilder :: flag_rule_builder()) -> flag_builder().
then_return(Variation, RuleBuilder) when is_boolean(Variation) ->
    Flag = maps:get(flag, RuleBuilder),
    BooleanRuleBuilder = maps:put(flag, boolean_flag(Flag), RuleBuilder),
    then_return(variation_for_boolean(Variation), BooleanRuleBuilder);
then_return(Variation, RuleBuilder) when is_integer(Variation) ->
    #{ flag := RuleFlag } = RuleBuilder,
    ExistingRules = maps:get(rules, RuleFlag, []),
    RuleBuilderWithVariation = maps:put(variation, Variation, RuleBuilder),
    Rule = build_rule(length(ExistingRules), RuleBuilderWithVariation),
    maps:put(rules, [Rule|ExistingRules], RuleFlag).

%%===================================================================
%% Internal functions
%%===================================================================

-spec new_clause(
    ContextKind :: ldclient_context:kind_value(),
    ContextAttribute :: ldclient_context:attribute_key(),
    Values :: [term()],
    Negate :: boolean()) -> ldclient_clause:clause().
new_clause(ContextKind, ContextAttribute, Values, Negate) ->
    AttributeBinary = if
                          is_binary(ContextAttribute) -> ContextAttribute;
                          is_atom(ContextAttribute) -> atom_to_binary(ContextAttribute, utf8);
                          true -> unknown
                      end,
    #{attribute => ldclient_attribute_reference:new(AttributeBinary),
        values => Values,
        negate => Negate,
        op => in,
        context_kind => ContextKind
    }.

-spec build_rule(Index :: non_neg_integer(), RuleBuilder :: flag_rule_builder()) -> ldclient_rule:rule().
build_rule(Index, #{variation := Variation, clauses := Clauses}) ->
    IndexBinary = integer_to_binary(Index),
    #{
        id => <<<<"rule">>/binary, IndexBinary/binary>>,
        clauses => Clauses,
        trackEvents => false,
        variationOrRollout => Variation
    }.

%% @doc Builds both context targets and user targets.
%%
%% When building targets there are special rules for user targets. A user target needs an entry in the context targets
%% to control the ordering of target evaluations, but this entry does not contain the targeted keys. Instead
%% evaluation will find a corresponding user target with the same variation. Here we always build both and add
%% the special entries for user targets.
%% @end
-spec build_targets(InTargets :: builder_targets()) ->
    {ContextTargets :: [ldclient_flag:target()], UserTargets :: [ldclient_flag:target()]}.
build_targets(InTargets) ->
    maps:fold(fun(Variation, KindsAndKeys, AccIn) ->
        {ContextTargets, UserTargets} = AccIn,
        {ContextTargetsToAdd, UserTargetsToAdd} = targets_for_kinds(Variation, KindsAndKeys),
        {ContextTargets ++ ContextTargetsToAdd, UserTargets ++ UserTargetsToAdd}
              end, {[], []}, InTargets).

-spec targets_for_kinds(
    Variation :: non_neg_integer(), KindsAndKeys :: #{ContextKind :: ldclient_context:kind_value() => sets:set()}
) -> {ContextTargets :: [ldclient_flag:target()], UserTargets :: [ldclient_flag:target()]}.
targets_for_kinds(Variation, KindsAndKeys) ->
    ContextTargets = maps:fold(fun(Kind, KeySet, AccIn) ->
        [#{
            variation => Variation,
            contextKind => Kind,
            values => case Kind of
                          <<"user">> -> [];
                          _ -> sets:to_list(KeySet)
                      end
        }|AccIn]
                               end, [], KindsAndKeys),
    UserTargets = maps:fold(fun(Kind, KeySet, AccIn) ->
        case Kind of
            <<"user">> -> [#{
                contextKind => Kind,
                variation => Variation,
                values => sets:to_list(KeySet)
            }|AccIn];
            _ -> AccIn
        end
                            end, [], KindsAndKeys),
    {ContextTargets, UserTargets}.
