-module(ldclient_targets_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    can_target_single_kind_context/1,
    can_match_multi_kind_context/1,
    can_match_user_target_from_context_targets/1,
    can_match_user_when_only_user_targets/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        can_target_single_kind_context,
        can_match_multi_kind_context,
        can_match_user_target_from_context_targets,
        can_match_user_when_only_user_targets
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

%%====================================================================
%% Tests
%%====================================================================

can_target_single_kind_context(_) ->
    {match, 1} = ldclient_targets:eval_flag_targets(
        ldclient_flag:new(#{
            <<"contextTargets">> => [
                #{<<"variation">> => 0, <<"contextKind">> => <<"user">>, <<"values">> => [<<"user-key">>]}
                #{<<"variation">> => 1, <<"contextKind">> => <<"org">>, <<"values">> => [<<"org-key">>]}
            ]
        }),
        ldclient_context:new(<<"org-key">>, <<"org">>)).

can_match_multi_kind_context(_) ->
    Flag = ldclient_flag:new(#{
        <<"contextTargets">> => [
            #{<<"variation">> => 1, <<"contextKind">> => <<"org">>, <<"values">> => [<<"org-key">>]},
            #{<<"variation">> => 0, <<"contextKind">> => <<"location">>, <<"values">> => [<<"location-key">>]}
        ]
    }),
    {match, 1} = ldclient_targets:eval_flag_targets(
        Flag,
        ldclient_context:new_multi_from([
            ldclient_context:new(<<"org-key">>, <<"org">>),
            ldclient_context:new(<<"bad-key">>, <<"location">>)
        ])),
    {match, 0} = ldclient_targets:eval_flag_targets(
        Flag,
        ldclient_context:new_multi_from([
            ldclient_context:new(<<"bad-key">>, <<"org">>),
            ldclient_context:new(<<"location-key">>, <<"location">>)
        ])).

can_match_user_target_from_context_targets(_) ->
    %% Should be variation 2 because the order should be based on contextTargets and not on targets.
    {match, 2} = ldclient_targets:eval_flag_targets(
        ldclient_flag:new(#{
            <<"contextTargets">> => [
                #{<<"variation">> => 2, <<"contextKind">> => <<"user">>, <<"values">> => []},
                #{<<"variation">> => 1, <<"contextKind">> => <<"user">>, <<"values">> => []}
            ],
            <<"targets">> => [
                #{<<"variation">> => 1, <<"contextKind">> => <<"user">>, <<"values">> => [<<"user-key">>]},
                #{<<"variation">> => 2, <<"contextKind">> => <<"user">>, <<"values">> => [<<"user-key">>]}
            ]
        }),
        ldclient_context:new(<<"user-key">>)).

can_match_user_when_only_user_targets(_) ->
    {match, 2} = ldclient_targets:eval_flag_targets(
        ldclient_flag:new(#{
            <<"targets">> => [
                #{<<"variation">> => 2, <<"contextKind">> => <<"user">>, <<"values">> => [<<"user-key">>]}
            ]
        }),
        ldclient_context:new(<<"user-key">>)).
