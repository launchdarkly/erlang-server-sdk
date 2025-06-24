-module(ldclient_rollout_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    can_rollout_non_experiment/1,
    can_rollout_experiment/1,
    untracked_variation_for_experiment_rollout_is_not_in_experiment/1,
    missing_kind_is_not_in_experiment/1,
    experiment_rollouts_do_not_use_bucket_by/1,
    missing_bucket_bucket_by_does_not_error_with_seed/1,
    bucket_by_null_attribute_does_not_error_with_seed/1,
    missing_bucket_bucket_by_does_not_error_without_seed/1,
    bucket_by_null_attribute_does_not_error_without_seed/1,
    missing_kind_is_not_in_experiment_with_seed/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        can_rollout_non_experiment,
        can_rollout_experiment,
        untracked_variation_for_experiment_rollout_is_not_in_experiment,
        missing_kind_is_not_in_experiment,
        experiment_rollouts_do_not_use_bucket_by,
        missing_bucket_bucket_by_does_not_error_with_seed,
        bucket_by_null_attribute_does_not_error_with_seed,
        missing_bucket_bucket_by_does_not_error_without_seed,
        bucket_by_null_attribute_does_not_error_without_seed,
        missing_kind_is_not_in_experiment_with_seed
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

can_rollout_non_experiment(_) ->
    {0, false} = ldclient_rollout:rollout_context(
        ldclient_rollout:new(#{
            <<"variations">> => [#{<<"variation">> => 0, <<"weight">> => 100000, <<"untracked">> => true}]
        }),
        #{key => <<"flagKey">>, salt => <<"flagSalt">>},
        ldclient_context:new(<<"user-key">>)).

can_rollout_experiment(_) ->
    {0, true} = ldclient_rollout:rollout_context(
        ldclient_rollout:new(#{
            <<"kind">> => <<"experiment">>,
            <<"variations">> => [#{<<"variation">> => 0, <<"weight">> => 100000, <<"untracked">> => false}]
        }),
        #{key => <<"flagKey">>, salt => <<"flagSalt">>},
        ldclient_context:new(<<"user-key">>)).

untracked_variation_for_experiment_rollout_is_not_in_experiment(_) ->
    {0, false} = ldclient_rollout:rollout_context(
        ldclient_rollout:new(#{
            <<"kind">> => <<"experiment">>,
            <<"variations">> => [#{<<"variation">> => 0, <<"weight">> => 100000, <<"untracked">> => true}]
        }),
        #{key => <<"flagKey">>, salt => <<"flagSalt">>},
        ldclient_context:new(<<"user-key">>)).

missing_kind_is_not_in_experiment(_) ->
    {0, false} = ldclient_rollout:rollout_context(
        ldclient_rollout:new(#{
            <<"kind">> => <<"experiment">>,
            <<"contextKind">> => <<"org">>,
            <<"variations">> => [#{<<"variation">> => 0, <<"weight">> => 100000, <<"untracked">> => false}]
        }),
        #{key => <<"flagKey">>, salt => <<"flagSalt">>},
        ldclient_context:new(<<"user-key">>)).

missing_kind_is_not_in_experiment_with_seed(_) ->
    {0, false} = ldclient_rollout:rollout_context(
        ldclient_rollout:new(#{
            <<"kind">> => <<"experiment">>,
            <<"contextKind">> => <<"org">>,
            <<"seed">> => 1234567890,
            <<"variations">> => [#{<<"variation">> => 0, <<"weight">> => 100000, <<"untracked">> => false}]
        }),
        #{key => <<"flagKey">>, salt => <<"flagSalt">>},
        ldclient_context:new(<<"user-key">>)).

experiment_rollouts_do_not_use_bucket_by(_) ->
    {1, true} = ldclient_rollout:rollout_context(
        ldclient_rollout:new(#{
            <<"kind">> => <<"experiment">>,
            <<"contextKind">> => <<"org">>,
            <<"bucketBy">> => <<"decoy">>,
            <<"variations">> => [
                #{<<"variation">> => 0, <<"weight">> => 59168, <<"untracked">> => false},
                #{<<"variation">> => 1, <<"weight">> => 2, <<"untracked">> => false}, %% org-key should be here.
                #{<<"variation">> => 2, <<"weight">> => 40830, <<"untracked">> => false} %% valueZZZ would be here.
                %% valueZZZ derived by adding Z until the hash value was larger than org-key.
            ]
        }),
        #{key => <<"flagKey">>, salt => <<"flagSalt">>},
        ldclient_context:set(<<"decoy">>, <<"valueZZZ">>, ldclient_context:new(<<"org-key">>, <<"org">>))),
    %% This is a counter example, to show the some rollout, but with kind=rollout, will use bucketBy.
    {2, false} = ldclient_rollout:rollout_context(
        ldclient_rollout:new(#{
            <<"kind">> => <<"rollout">>,
            <<"contextKind">> => <<"org">>,
            <<"bucketBy">> => <<"decoy">>,
            <<"variations">> => [
                #{<<"variation">> => 0, <<"weight">> => 59168, <<"untracked">> => false},
                #{<<"variation">> => 1, <<"weight">> => 2, <<"untracked">> => false},
                #{<<"variation">> => 2, <<"weight">> => 40830, <<"untracked">> => false}
            ]
        }),
        #{key => <<"flagKey">>, salt => <<"flagSalt">>},
        ldclient_context:set(<<"decoy">>, <<"valueZZZ">>, ldclient_context:new(<<"org-key">>, <<"org">>))).

missing_bucket_bucket_by_does_not_error_with_seed(_) ->
    {0, false} = ldclient_rollout:rollout_context(
        ldclient_rollout:new(#{
            <<"kind">> => <<"rollout">>,
            <<"contextKind">> => <<"user">>,
            <<"bucketBy">> => <<"missing">>,
            <<"seed">> => 1234567890,
            <<"variations">> => [
                #{<<"variation">> => 0, <<"weight">> => 50000, <<"untracked">> => false},
                #{<<"variation">> => 1, <<"weight">> => 50000, <<"untracked">> => false}
            ]
        }),
        #{key => <<"flagKey">>, salt => <<"flagSalt">>},
        ldclient_context:new(<<"user-key">>)).

bucket_by_null_attribute_does_not_error_with_seed(_) ->
    {0, false} = ldclient_rollout:rollout_context(
        ldclient_rollout:new(#{
            <<"kind">> => <<"rollout">>,
            <<"contextKind">> => <<"user">>,
            <<"bucketBy">> => <<"null">>,
            <<"seed">> => 1234567890,
            <<"variations">> => [
                #{<<"variation">> => 0, <<"weight">> => 50000, <<"untracked">> => false},
                #{<<"variation">> => 1, <<"weight">> => 50000, <<"untracked">> => false}
            ]
        }),
        #{key => <<"flagKey">>, salt => <<"flagSalt">>},
        ldclient_context:set(<<"null">>, null, ldclient_context:new(<<"user-key">>))).

missing_bucket_bucket_by_does_not_error_without_seed(_) ->
    {0, false} = ldclient_rollout:rollout_context(
        ldclient_rollout:new(#{
            <<"kind">> => <<"rollout">>,
            <<"contextKind">> => <<"user">>,
            <<"bucketBy">> => <<"missing">>,
            <<"variations">> => [
                #{<<"variation">> => 0, <<"weight">> => 50000, <<"untracked">> => false},
                #{<<"variation">> => 1, <<"weight">> => 50000, <<"untracked">> => false}
            ]
        }),
        #{key => <<"flagKey">>, salt => <<"flagSalt">>},
        ldclient_context:new(<<"user-key">>)).

bucket_by_null_attribute_does_not_error_without_seed(_) ->
    {0, false} = ldclient_rollout:rollout_context(
        ldclient_rollout:new(#{
            <<"kind">> => <<"rollout">>,
            <<"contextKind">> => <<"user">>,
            <<"bucketBy">> => <<"null">>,
            <<"variations">> => [
                #{<<"variation">> => 0, <<"weight">> => 50000, <<"untracked">> => false},
                #{<<"variation">> => 1, <<"weight">> => 50000, <<"untracked">> => false}
            ]
        }),
        #{key => <<"flagKey">>, salt => <<"flagSalt">>},
        ldclient_context:set(<<"null">>, null, ldclient_context:new(<<"user-key">>))).    
