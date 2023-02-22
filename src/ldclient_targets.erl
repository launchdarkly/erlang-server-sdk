%%-------------------------------------------------------------------
%% @doc Methods for evaluating targets.
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_targets).

%% API
-export([eval_flag_targets/2]).

-type match_result() :: no_match | {match, non_neg_integer()}.

%% @doc Evaluate the targets of the flag against the specified context.
%%
%% If there are no matches, then return `no_match'.
%%
%% If there is a match, then return `{match, Variation}'.
%% @end
-spec eval_flag_targets(Flag :: ldclient_flag:flag(), Context :: ldclient_context:context()) -> match_result().
eval_flag_targets(
    #{targets := [], contextTargets := []} = _Flag,
    _Context
) ->
    %% There are no targets to evaluate, so there is no match.
    no_match;
eval_flag_targets(
    #{targets := UserTargets, contextTargets := []} = _Flag,
    Context
) ->
    %% The context targets are empty, so evaluate user targets only.
    check_user_targets(UserTargets, Context);
eval_flag_targets(
    #{targets := UserTargets, contextTargets := ContextTargets} = _Flag,
    Context
) ->
    check_context_targets(ContextTargets, UserTargets, Context).

%%===================================================================
%% Internal functions
%%===================================================================

%% @doc Check context targets for any matches.
%%
%% This logic is independent of user targets checking because context targets have special logic
%% for when their kind is "user". The values list will be empty, and instead the user targets should
%% be checked for a target of the same variation. This allows for user targets to remain compatible
%% with existing SDKs and also not need to be duplicated for context targets.
%% @end
-spec check_context_targets(
    ContextTargets :: [ldclient_flag:target()],
    UserTargets :: [ldclient_flag:target()],
    Context :: ldclient_context:context()
) -> match_result().
check_context_targets([], _UserTargets, _Context) -> no_match;
check_context_targets([#{contextKind := ContextKind, variation := Variation} = ContextTarget|Rest] = _Target, UserTargets, Context) ->
    Result = case ContextKind of
        %% In the case the context targets are for a user kind, then we are just using it for ordering, and we want
        %% To check a user target with the same variation as the context target.
        <<"user">> -> eval_target(find_user_target_for_variation(Variation, UserTargets), Context);
        %% This was not a user kind, so just evaluate the target.
        _ -> eval_target(ContextTarget, Context)
    end,
    %% If there is no match, then continue checking, if there is a match, then stop early and return it.
    case Result of
        no_match ->check_context_targets(Rest, UserTargets, Context);
        _ -> Result
    end.

%% @doc Check the user targets of the flag to see if there are any matches.
%%
%% @end
-spec check_user_targets(UserTargets :: [ldclient_flag:target()], Context :: ldclient_context:context()) -> match_result().
check_user_targets([], _Context) ->
    no_match;
check_user_targets([Target|Rest] = _Targets, Context) ->
    case eval_target(Target, Context) of
        %% There was not a match, so continue checking.
        no_match -> check_user_targets(Rest, Context);
        %% There was a match, so return it and stop checking.
        Match -> Match
    end.

%% @doc Find a user target associated with a given variation.
%%
%% If there is no user target with a matching variation, then null will be returned.
%%
%% If there is a user target with a matching variation, then that target will be returned.
%% @end
-spec find_user_target_for_variation(
    Variation :: non_neg_integer(),
    UserTargets :: [ldclient_flag:target()]
) -> ldclient_flag:target() | null.
find_user_target_for_variation(Variation, UserTargets) ->
    find_user_target_for_variation_result(lists:search(fun(Target) ->
            #{variation := TargetVariation} = Target,
            TargetVariation =:= Variation
        end, UserTargets)).

%% @doc Convenience method to convert a lists:search result into either null or a target().
%%
%% @end
-spec find_user_target_for_variation_result(Result :: false | {value, ldclient_flag:target()}) -> null | ldclient_flag:target().
find_user_target_for_variation_result(false) -> null;
find_user_target_for_variation_result({value, Value}) -> Value.

%% @doc Evaluate a single target against a context.
%%
%% @end
-spec eval_target(Target :: null | ldclient_flag:target(), Context :: ldclient_context:context()) -> match_result().
eval_target(null, _Context) -> no_match;
eval_target(
    #{contextKind := ContextKind, values := Values, variation := Variation} = _Target,
    Context
) ->
    Key = ldclient_context:get_key(ContextKind, Context),
    Match = eval_target_key(Key, Values),
    eval_target_result(Match, Variation).

%% @doc Convenience method to search for a key in a list of values. If the key is null, then it returns
%% false instead of performing the search.
%% @end
-spec eval_target_key(Key :: null | binary(), Values :: [binary()]) -> boolean().
eval_target_key(null = _Key, _Values) -> false;
eval_target_key(Key, Values) -> lists:member(Key, Values).

%% @doc Convenience method for turning a search result, and a variation into a match_result().
%%
%% @end
-spec eval_target_result(Match :: boolean(), Variation :: pos_integer()) -> match_result().
eval_target_result(false = _Match, _Variation) -> no_match;
eval_target_result(true = _Match, Variation) -> {match, Variation}.
