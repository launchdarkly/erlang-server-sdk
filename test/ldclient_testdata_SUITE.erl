-module(ldclient_testdata_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).


%%====================================================================
%% ct functions
%%====================================================================
all() ->
    [
        test_default_flags,
        test_targeting,
        test_rules,
        test_multiple_clients,
        test_value_for_all,
        test_multiple_testdata_sources,
        test_targeting_non_users
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ldclient),
    Config.

end_per_suite(_) ->
    ok = application:stop(ldclient).

init_per_testcase(_, Config) ->
    Options = #{
        send_events => false,
        feature_store => ldclient_storage_map,
        datasource => testdata
    },
    ldclient:start_instance("", Options),
    Config.

end_per_testcase(_, _Config) ->
    ldclient:stop_all_instances().

%%====================================================================
%% Tests
%%====================================================================

test_default_flags(_) ->
    {ok, Flag} = ldclient_testdata:flag(<<"flag1">>),
    ldclient_testdata:update(Flag),
    User = ldclient_user:new(<<"user">>),
    true = ldclient:variation(<<"flag1">>, User, false),
    ldclient_testdata:update(ldclient_flagbuilder:on(false, Flag)),
    false = ldclient:variation(<<"flag1">>, User, true).

test_targeting(_) ->
    {ok, Flag} = ldclient_testdata:flag(<<"flag1">>),
    UpdatedFlag = ldclient_flagbuilder:off_variation(2,
                  ldclient_flagbuilder:fallthrough_variation(1,
                  ldclient_flagbuilder:variation_for_context(2, <<"user">>, <<"greg">>,
                  ldclient_flagbuilder:variation_for_context(0, <<"user">>, <<"ben">>,
                  ldclient_flagbuilder:variations([<<"red">>, <<"green">>, <<"blue">>], Flag))))),
    ldclient_testdata:update(UpdatedFlag),
    User = ldclient_user:new(<<"user">>),
    Ben = ldclient_user:new(<<"ben">>),
    Greg = ldclient_user:new(<<"greg">>),
    <<"red">> = ldclient:variation(<<"flag1">>, Ben, <<"nothing">>),
    <<"blue">> = ldclient:variation(<<"flag1">>, Greg, <<"nothing">>),
    <<"green">> = ldclient:variation(<<"flag1">>, User, <<"nothing">>),

    {ok, SavedFlag} = ldclient_testdata:flag(<<"flag1">>),
    ldclient_testdata:update(ldclient_flagbuilder:on(false, SavedFlag)),
    <<"blue">> = ldclient:variation(<<"flag1">>, Ben, <<"nothing">>),
    <<"blue">> = ldclient:variation(<<"flag1">>, Greg, <<"nothing">>),
    <<"blue">> = ldclient:variation(<<"flag1">>, User, <<"nothing">>).

test_rules(_) ->
    {ok, Flag} = ldclient_testdata:flag(<<"flag1">>),
    UpdatedFlag = ldclient_flagbuilder:fallthrough_variation(2,
                  ldclient_flagbuilder:then_return(1,
                  ldclient_flagbuilder:and_match(country, [<<"usa">>],
                  ldclient_flagbuilder:if_not_match(name, [<<"ben">>],
                  ldclient_flagbuilder:then_return(0,
                  ldclient_flagbuilder:and_match(name, [<<"ben">>, <<"evelyn">>],
                  ldclient_flagbuilder:if_match(country, [<<"gb">>],
                  ldclient_flagbuilder:variations([<<"red">>, <<"green">>, <<"blue">>], Flag)))))))),
    ldclient_testdata:update(UpdatedFlag),
    BenW = ldclient_user:set(name, <<"ben">>,
           ldclient_user:set(country, <<"usa">>,
           ldclient_user:new(<<"user">>))),
    BenL = ldclient_user:set(name, <<"ben">>,
           ldclient_user:set(country, <<"gb">>,
           ldclient_user:new(<<"ben">>))),
    Greg = ldclient_user:set(name, <<"greg">>,
           ldclient_user:set(country, <<"usa">>,
           ldclient_user:new(<<"greg">>))),
    Evelyn = ldclient_user:set(name, <<"evelyn">>,
             ldclient_user:set(country, <<"gb">>,
             ldclient_user:new(<<"evelyn">>))),
    <<"blue">> = ldclient:variation(<<"flag1">>, BenW, <<"nothing">>),
    <<"red">> = ldclient:variation(<<"flag1">>, BenL, <<"nothing">>),
    <<"green">> = ldclient:variation(<<"flag1">>, Greg, <<"nothing">>),
    <<"red">> = ldclient:variation(<<"flag1">>, Evelyn, <<"nothing">>).

test_multiple_clients(_) ->
    User = ldclient_user:new(<<"user">>),

    {ok, Flag} = ldclient_testdata:flag(<<"flag1">>),
    ldclient_testdata:update(default,
        ldclient_flagbuilder:variation_for_all(0,
        ldclient_flagbuilder:variations([<<"red">>, <<"green">>, <<"blue">>], Flag))),

    ldclient:start_instance("", second_client, #{
        send_events => false,
        feature_store => ldclient_storage_map,
        datasource => testdata
    }),

    <<"red">> = ldclient:variation(<<"flag1">>, User, <<"none">>),
    <<"red">> = ldclient:variation(<<"flag1">>, User, <<"none">>, second_client),

    {ok, SavedFlag} = ldclient_testdata:flag(<<"flag1">>),
    ldclient_testdata:update(ldclient_flagbuilder:variation_for_all(1, SavedFlag)),
    <<"green">> = ldclient:variation(<<"flag1">>, User, <<"none">>),
    <<"green">> = ldclient:variation(<<"flag1">>, User, <<"none">>, second_client),

    ldclient:stop_instance(second_client),

    ldclient_testdata:update(ldclient_flagbuilder:variation_for_all(2, SavedFlag)),
    <<"blue">> = ldclient:variation(<<"flag1">>, User, <<"none">>).

test_value_for_all(_) ->
    {ok, Flag} = ldclient_testdata:flag(<<"flag1">>),
    UpdatedFlag = ldclient_flagbuilder:value_for_all(42,
                  ldclient_flagbuilder:variation_for_context(0, <<"user">>, <<"ben">>,
                  ldclient_flagbuilder:variations([<<"red">>, <<"green">>, <<"blue">>], Flag))),
    ldclient_testdata:update( UpdatedFlag),
    Ben = ldclient_user:new(<<"ben">>),
    Greg = ldclient_user:new(<<"greg">>),
    42 = ldclient:variation(<<"flag1">>, Ben, null),
    42 = ldclient:variation(<<"flag1">>, Greg, null),
    ldclient_testdata:update(ldclient_flagbuilder:value_for_all(<<"6 multiplied by 9">>, Flag)),
    <<"6 multiplied by 9">> = ldclient:variation(<<"flag1">>, Ben, null),
    <<"6 multiplied by 9">> = ldclient:variation(<<"flag1">>, Greg, null).

test_multiple_testdata_sources(_) ->
    ldclient:start_instance("", second_client, #{
        send_events => false,
        feature_store => ldclient_storage_map,
        datasource => testdata,
        testdata_tag => second_testdata
    }),

    {ok, Flag} = ldclient_testdata:flag(<<"flag1">>),
    ldclient_testdata:update(ldclient_flagbuilder:value_for_all(<<"orange">>, Flag)),

    {ok, SecondFlag} = ldclient_testdata:flag(second_testdata, <<"flag1">>),
    ldclient_testdata:update(second_testdata, ldclient_flagbuilder:value_for_all(<<"red">>, SecondFlag)),

    User = ldclient_user:new(<<"user">>),
    <<"orange">> = ldclient:variation(<<"flag1">>, User, <<"">>),
    <<"red">> = ldclient:variation(<<"flag1">>, User, <<"">>, second_client),
    ok.

test_targeting_non_users(_) ->
    {ok, Flag} = ldclient_testdata:flag(<<"flag1">>),
    UpdatedFlag = ldclient_flagbuilder:off_variation(2,
        ldclient_flagbuilder:fallthrough_variation(1,
            ldclient_flagbuilder:variation_for_context(2, <<"org">>, <<"gregorg">>,
                ldclient_flagbuilder:variation_for_context(0, <<"jamin">>, <<"benjamin">>,
                    ldclient_flagbuilder:variations([<<"red">>, <<"green">>, <<"blue">>], Flag))))),
    ldclient_testdata:update(UpdatedFlag),
    Org = ldclient_context:new(<<"org-key">>, <<"org">>),
    Benjamin = ldclient_context:new(<<"benjamin">>, <<"jamin">>),
    Gregorg = ldclient_context:new(<<"gregorg">>, <<"org">>),
    Benorg = ldclient_context:new(<<"benjamin">>, <<"org">>),
    <<"red">> = ldclient:variation(<<"flag1">>, Benjamin, <<"nothing">>),
    %% Same key, but different kind, falls through.
    <<"green">> = ldclient:variation(<<"flag1">>, Benorg, <<"nothing">>),
    <<"blue">> = ldclient:variation(<<"flag1">>, Gregorg, <<"nothing">>),
    <<"green">> = ldclient:variation(<<"flag1">>, Org, <<"nothing">>).
