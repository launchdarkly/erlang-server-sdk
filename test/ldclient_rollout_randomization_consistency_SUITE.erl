-module(ldclient_rollout_randomization_consistency_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Tests
-export([
    bucket_context_by_key/1,
    bucket_context_with_seed/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() -> 
    [ 
        bucket_context_by_key, 
        bucket_context_with_seed
    ].

init_per_suite(Config) -> 
    Config.
end_per_suite(_) -> 
    ok.
init_per_testcase(_, Config) -> 
    Config.
end_per_testcase(_, _) -> 
    ok.

%%====================================================================
%% Helpers
%%====================================================================

-spec fl_eq(float(), float()) -> nil.
fl_eq(L, R) -> 
    true = abs(L - R) < 0.0000001.

make_and_bucket(Seed, Key, Salty) ->
    Point1 = ldclient_rollout:bucket_context(Seed, Key, Salty,
        ldclient_context:new(<<"userKeyA">>), ldclient_attribute_reference:new(<<"key">>), <<"user">>),
    Point2 = ldclient_rollout:bucket_context(Seed, Key, Salty,
        ldclient_context:new(<<"userKeyB">>), ldclient_attribute_reference:new(<<"key">>), <<"user">>),
    Point3 = ldclient_rollout:bucket_context(Seed, Key, Salty,
        ldclient_context:new(<<"userKeyC">>), ldclient_attribute_reference:new(<<"key">>), <<"user">>),
    {Point1, Point2, Point3}.

%%====================================================================
%% Tests
%%====================================================================

%% Note: These tests are meant to be exact duplicates of tests
%% in other SDKs. Do not change any of the values unless they
%% are also changed in other SDKs. These are not traditional behavioral
%% tests so much as consistency tests to guarantee that the implementation
%% is identical across SDKs.

bucket_context_by_key(_) ->
    Seed = null,
    Salty = <<"saltyA">>,
    Key = <<"hashKey">>,
    {Point1, Point2, Point3} = make_and_bucket(Seed, Key, Salty),
    fl_eq(0.42157587, Point1),
    fl_eq(0.6708485, Point2),
    fl_eq(0.10343106, Point3).

bucket_context_with_seed(_) -> 
    Seed = 61,
    Salty = <<"saltyA">>,
    Key = <<"hashKey">>,
    {Point1, Point2, Point3} = make_and_bucket(Seed, Key, Salty),
    fl_eq(0.09801207, Point1),
    fl_eq(0.14483777, Point2),
    fl_eq(0.9242641, Point3).
