-module(ldclient_eval_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    sdk_offline/1,
    unknown_flag/1,
    unknown_flag_another/1,
    user_with_no_key/1,
    user_with_null_key/1,
    off_flag/1,
    off_flag_another/1,
    off_flag_null_off_variation/1,
    deleted_flag/1,
    prerequisite_fail_off/1,
    prerequisite_fail_off_null/1,
    prerequisite_fail_variation/1,
    prerequisite_success/1,
    target_user/1,
    target_user_another/1,
    segment_included/1,
    segment_excluded_negated/1,
    segment_excluded_negated_nonuser/1,
    segment_excluded_another/1,
    rule_match_in/1,
    rule_match_ends_with/1,
    rule_match_ends_and_starts_with_order/1,
    rule_match_starts_with/1,
    rule_match_regex/1,
    rule_match_contains/1,
    rule_match_less_than/1,
    rule_match_less_than_or_equal/1,
    rule_match_greater_than/1,
    rule_match_greater_than_or_equal/1,
    rule_match_before_int/1,
    rule_match_after_int/1,
    rule_match_semver_equal/1,
    rule_match_semver_greater_than/1,
    rule_match_semver_less_than/1,
    rule_match_before_date/1,
    rule_match_after_date/1,
    rule_nomatch_in_negated_null_attribute/1,
    fallthrough_rollout/1,
    fallthrough_rollout_custom/1,
    fallthrough_rollout_custom_integer/1,
    fallthrough_rollout_custom_float/1,
    fallthrough_rollout_custom_float_invalid/1,
    fallthrough_rollout_invalid_last_variation/1,
    variation_out_of_range/1,
    extra_fields/1,
    missing_some_fields/1,
    missing_all_fields/1,
    missing_rollout_for_rule/1,
    missing_rollout_for_rule_match_rule/1,
    fallthrough_no_rollout_or_variation/1,
    fallthrough_rollout_in_experiment/1,
    fallthrough_rollout_not_in_experiment/1,
    rule_match_rollout_in_experiment/1,
    rule_match_rollout_not_in_experiment/1,
    all_flags_state/1,
    all_flags_state_with_reason/1,
    all_flags_state_offline/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        sdk_offline,
        unknown_flag,
        unknown_flag_another,
        user_with_no_key,
        user_with_null_key,
        off_flag,
        off_flag_another,
        off_flag_null_off_variation,
        deleted_flag,
        prerequisite_fail_off,
        prerequisite_fail_off_null,
        prerequisite_fail_variation,
        prerequisite_success,
        target_user,
        target_user_another,
        segment_included,
        segment_excluded_negated,
        segment_excluded_negated_nonuser,
        segment_excluded_another,
        rule_match_in,
        rule_match_ends_with,
        rule_match_ends_and_starts_with_order,
        rule_match_starts_with,
        rule_match_regex,
        rule_match_contains,
        rule_match_less_than,
        rule_match_less_than_or_equal,
        rule_match_greater_than,
        rule_match_greater_than_or_equal,
        rule_match_before_int,
        rule_match_after_int,
        rule_match_semver_equal,
        rule_match_semver_greater_than,
        rule_match_semver_less_than,
        rule_match_before_date,
        rule_match_after_date,
        rule_nomatch_in_negated_null_attribute,
        fallthrough_rollout,
        fallthrough_rollout_custom,
        fallthrough_rollout_custom_integer,
        fallthrough_rollout_custom_float,
        fallthrough_rollout_custom_float_invalid,
        fallthrough_rollout_invalid_last_variation,
        variation_out_of_range,
        extra_fields,
        missing_some_fields,
        missing_all_fields,
        missing_rollout_for_rule,
        missing_rollout_for_rule_match_rule,
        fallthrough_no_rollout_or_variation,
        fallthrough_rollout_in_experiment,
        fallthrough_rollout_not_in_experiment,
        rule_match_rollout_in_experiment,
        rule_match_rollout_not_in_experiment,
        all_flags_state,
        all_flags_state_with_reason,
        all_flags_state_offline
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ldclient),
    Options = #{
        stream => false,
        polling_update_requestor => ldclient_update_requestor_test
    },
    OfflineOptions = #{
        polling_update_requestor => ldclient_update_requestor_test,
        offline => true
    },
    ldclient:start_instance("", Options),
    ldclient:start_instance("", another1, Options),
    ldclient:start_instance("", offline, OfflineOptions),
    ok = create_flags(),
    Config.

end_per_suite(_) ->
    ok = application:stop(ldclient).

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.

%%====================================================================
%% Helpers
%%====================================================================

create_flags() ->
    DataFilename = code:priv_dir(ldclient) ++ "/flags-segments-put-data.json",
    DataFilename2 = code:priv_dir(ldclient) ++ "/flags-segments-put-data-another1.json",
    {ok, PutData} = file:read_file(DataFilename),
    {ok, PutData2} = file:read_file(DataFilename2),
    ok = ldclient_update_stream_server:process_event(#{event => <<"put">>, data => PutData}, ldclient_storage_ets, default),
    ok = ldclient_update_stream_server:process_event(#{event => <<"put">>, data => PutData2}, ldclient_storage_ets, another1).

extract_events(Events) ->
    extract_events(Events, false).

extract_events(Events, false) ->
    [
        {Key, Type, Variation, VariationValue, DefaultValue, Reason, PrereqOf} ||
        #{
            type := Type,
            data := #{
                key := Key,
                variation := Variation,
                value := VariationValue,
                default := DefaultValue,
                eval_reason := Reason,
                prereq_of := PrereqOf
            }
        } <- Events
    ];
extract_events(Events, true) ->
    [
        {Key, Type, Variation, VariationValue, DefaultValue, Reason, PrereqOf, TrackEvents, IncludeReason} ||
        #{
            type := Type,
            data := #{
                key := Key,
                variation := Variation,
                value := VariationValue,
                default := DefaultValue,
                eval_reason := Reason,
                prereq_of := PrereqOf,
                trackEvents := TrackEvents,
                include_reason := IncludeReason
            }
        } <- Events
    ].

%%====================================================================
%% Tests
%%====================================================================

sdk_offline(_) ->
    {{null, "foo", {error, client_not_ready}}, []} =
        ldclient_eval:flag_key_for_user(offline, <<"keep-it-off">>, #{key => <<"some-user">>}, "foo").

unknown_flag(_) ->
    {{null, "foo", {error, flag_not_found}}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"flag-that-does-not-exist">>, #{key => <<"some-user">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"flag-that-does-not-exist">>, feature_request, null, "foo", "foo", {error, flag_not_found}, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

unknown_flag_another(_) ->
    % Flag exists in default instance, doesn't exist in another1 instance
    {{null, "foo", {error, flag_not_found}}, Events} =
        ldclient_eval:flag_key_for_user(another1, <<"bad-variation">>, #{key => <<"some-user">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"bad-variation">>, feature_request, null, "foo", "foo", {error, flag_not_found}, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

user_with_no_key(_) ->
    {{null, "foo", {error, user_not_specified}}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"keep-it-off">>, #{name => <<"some-user">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"keep-it-off">>, feature_request, null, "foo", "foo", {error, user_not_specified}, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

user_with_null_key(_) ->
    {{null, "foo", {error, user_not_specified}}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"keep-it-off">>, #{key => null}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"keep-it-off">>, feature_request, null, "foo", "foo", {error, user_not_specified}, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

off_flag(_) ->
    {{1, false, off}, Events} = ldclient_eval:flag_key_for_user(default, <<"keep-it-off">>, #{key => <<"user123">>}, "foo"),
    ExpectedEvents = lists:sort([{<<"keep-it-off">>, feature_request, 1, false, "foo", off, null}]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

off_flag_another(_) ->
    {{1, false, off}, Events} = ldclient_eval:flag_key_for_user(another1, <<"keep-it-off">>, #{key => <<"user123">>}, "foo"),
    ExpectedEvents = lists:sort([{<<"keep-it-off">>, feature_request, 1, false, "foo", off, null}]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

off_flag_null_off_variation(_) ->
    {{null, "foo", off}, Events} = ldclient_eval:flag_key_for_user(default, <<"keep-it-off-null-off-variation">>, #{key => <<"user123">>}, "foo"),
    ExpectedEvents = lists:sort([{<<"keep-it-off-null-off-variation">>, feature_request, null, "foo", "foo", off, null}]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

deleted_flag(_) ->
    {{null, "foo", {error, flag_not_found}}, Events} = ldclient_eval:flag_key_for_user(default, <<"keep-it-deleted">>, #{key => <<"user123">>}, "foo"),
    ExpectedEvents = lists:sort([{<<"keep-it-deleted">>, feature_request, null, "foo", "foo", {error, flag_not_found}, null}]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

prerequisite_fail_off(_) ->
    {{1, false, {prerequisite_failed, [<<"keep-it-off">>]}}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"prereqs-fail-off">>, #{key => <<"user123">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"prereqs-fail-off">>, feature_request, 1, false, "foo", {prerequisite_failed, [<<"keep-it-off">>]}, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

prerequisite_fail_off_null(_) ->
    {{null, "foo", {prerequisite_failed, [<<"keep-it-off">>]}}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"prereqs-fail-off-null">>, #{key => <<"user123">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"prereqs-fail-off-null">>, feature_request, null, "foo", "foo", {prerequisite_failed, [<<"keep-it-off">>]}, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

prerequisite_fail_variation(_) ->
    {{1, false, {prerequisite_failed, [<<"keep-it-on">>]}}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"prereqs-fail-variation">>, #{key => <<"user123">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"keep-it-on">>, feature_request, 0, true, null, fallthrough, <<"prereqs-fail-variation">>},
        {<<"keep-it-on-two">>, feature_request, 0, true, null, fallthrough, <<"keep-it-on">>},
        {<<"prereqs-fail-variation">>, feature_request, 1, false, "foo", {prerequisite_failed, [<<"keep-it-on">>]}, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

prerequisite_success(_) ->
    {{1, false, fallthrough}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"prereqs-success">>, #{key => <<"user123">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"prereqs-success">>, feature_request, 1, false, "foo", fallthrough, null},
        {<<"keep-it-on-another">>, feature_request, 0, true, null, fallthrough, <<"prereqs-success">>},
        {<<"keep-it-on">>, feature_request, 0, true, null, fallthrough, <<"prereqs-success">>},
        {<<"keep-it-on-two">>, feature_request, 0, true, null, fallthrough, <<"keep-it-on">>}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

target_user(_) ->
    {{0, true, target_match}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"target-me">>, #{key => <<"user-33333">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"target-me">>, feature_request, 0, true, "foo", target_match, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

target_user_another(_) ->
    % Same user, different instance, different target result
    {{1, false, target_match}, Events} =
        ldclient_eval:flag_key_for_user(another1, <<"target-me">>, #{key => <<"user-33333">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"target-me">>, feature_request, 1, false, "foo", target_match, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

segment_included(_) ->
    ExpectedReason = {rule_match, 0, <<"ab4a9fb3-7e85-429f-8078-23aa70094540">>},
    {{1, false, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"segment-me">>, #{key => <<"user-12345">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"segment-me">>, feature_request, 1, false, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

segment_excluded_negated(_) ->
    ExpectedReason = {rule_match, 1, <<"489a185d-caaf-4db9-b192-e09e927d070c">>},
    {{1, false, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"segment-me">>, #{key => <<"user-33333">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"segment-me">>, feature_request, 1, false, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

segment_excluded_negated_nonuser(_) ->
    % This user isn't specified in a segment
    ExpectedReason = {rule_match, 1, <<"489a185d-caaf-4db9-b192-e09e927d070c">>},
    {{1, false, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"segment-me">>, #{key => <<"user-99999">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"segment-me">>, feature_request, 1, false, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

segment_excluded_another(_) ->
    % Same user, different instance, different segment result
    ExpectedReason = {rule_match, 1, <<"489a185d-caaf-4db9-b192-e09e927d070c">>},
    {{1, false, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(another1, <<"segment-me">>, #{key => <<"user-12345">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"segment-me">>, feature_request, 1, false, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_in(_) ->
    ExpectedReason = {rule_match, 0, <<"08b9b261-5df6-4881-892b-e25bdb28b6d3">>},
    {{0, <<"a">>, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"rule-me">>, #{key => <<"user-key-match@example.com">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 0, <<"a">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_ends_with(_) ->
    ExpectedReason = {rule_match, 1, <<"2fac50d0-d912-424a-831e-ab60ad0547b4">>},
    {{1, <<"b">>, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"rule-me">>, #{key => <<"user-ends-with@example.com">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 1, <<"b">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_ends_and_starts_with_order(_) ->
    ExpectedReason = {rule_match, 1, <<"2fac50d0-d912-424a-831e-ab60ad0547b4">>},
    {{1, <<"b">>, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"rule-me">>, #{key => <<"user-starts-with@example.com">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 1, <<"b">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_starts_with(_) ->
    ExpectedReason = {rule_match, 2, <<"e3b70ddf-a000-4649-93c5-ac0eaea675f8">>},
    {{2, <<"c">>, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"rule-me">>, #{key => <<"user-starts-with@foo.com">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 2, <<"c">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_regex(_) ->
    ExpectedReason = {rule_match, 3, <<"1d63c99a-3016-4778-bf1f-68d1fce5004e">>},
    {{3, <<"d">>, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"rule-me">>, #{key => <<"user-regex-match@foo.com">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 3, <<"d">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_contains(_) ->
    ExpectedReason = {rule_match, 4, <<"1f1dadfc-0e66-42e0-b479-979186d972ce">>},
    {{4, <<"e">>, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"rule-me">>, #{key => <<"user-contains@foo.com">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 4, <<"e">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_less_than(_) ->
    User = #{
        key => <<"user-foo">>,
        custom => #{
            <<"custom1">> => 30
        }
    },
    ExpectedReason = {rule_match, 5, <<"ca092500-1cb7-4b14-a11c-81b46ca19cae">>},
    {{5, <<"f">>, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"rule-me">>, User, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 5, <<"f">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_less_than_or_equal(_) ->
    User = #{
        key => <<"user-foo">>,
        custom => #{
            <<"custom1">> => 50
        }
    },
    ExpectedReason = {rule_match, 6, <<"d38e11f8-93d1-453e-8022-6d8ed7f106ea">>},
    {{6, <<"g">>, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"rule-me">>, User, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 6, <<"g">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_greater_than(_) ->
    User = #{
        key => <<"user-foo">>,
        custom => #{
            <<"custom2">> => 70
        }
    },
    ExpectedReason = {rule_match, 7, <<"a92a93c2-2004-482b-9e4a-38abe81d7050">>},
    {{7, <<"h">>, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"rule-me">>, User, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 7, <<"h">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_greater_than_or_equal(_) ->
    User = #{
        key => <<"user-foo">>,
        custom => #{
            <<"custom2">> => 50
        }
    },
    ExpectedReason = {rule_match, 8, <<"9158e01a-a70f-4924-8cf8-9401e2cf6c67">>},
    {{8, <<"i">>, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"rule-me">>, User, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 8, <<"i">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_before_int(_) ->
    User = #{
        key => <<"user-foo">>,
        custom => #{
            <<"beforeInt">> => 1451772244
        }
    },
    ExpectedReason = {rule_match, 9, <<"500633a7-2c82-4baf-8201-4892b68b31b4">>},
    {{9, <<"j">>, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"rule-me">>, User, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 9, <<"j">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_after_int(_) ->
    User = #{
        key => <<"user-foo">>,
        custom => #{
            <<"afterInt">> => 1451772246
        }
    },
    ExpectedReason = {rule_match, 10, <<"77473bea-d93f-4787-84d2-92cf08b35f2b">>},
    {{10, <<"k">>, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"rule-me">>, User, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 10, <<"k">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_semver_equal(_) ->
    User = #{
        key => <<"user-foo">>,
        custom => #{
            <<"version">> => <<"5.0.0">>
        }
    },
    ExpectedReason = {rule_match, 11, <<"9398cafc-0ab7-4d0d-8e01-6683cc4d17ec">>},
    {{11, <<"l">>, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"rule-me">>, User, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 11, <<"l">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_semver_greater_than(_) ->
    User = #{
        key => <<"user-foo">>,
        custom => #{
            <<"version">> => <<"5.1.0">>
        }
    },
    ExpectedReason = {rule_match, 12, <<"3570714f-d03b-4068-ab79-18f15c74382d">>},
    {{12, <<"m">>, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"rule-me">>, User, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 12, <<"m">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_semver_less_than(_) ->
    User = #{
        key => <<"user-foo">>,
        custom => #{
            <<"version">> => <<"4.0.0">>
        }
    },
    ExpectedReason = {rule_match, 13, <<"2c002923-2db0-4fcc-a95e-f3cb5b4bd13d">>},
    {{13, <<"n">>, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"rule-me">>, User, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 13, <<"n">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_before_date(_) ->
    User = #{
        key => <<"user-foo">>,
        custom => #{
            <<"beforeDate">> => <<"2018-01-01T11:59:59Z">>
        }
    },
    ExpectedReason = {rule_match, 14, <<"b6c5ceec-364d-4c23-a041-7865f4f136d3">>},
    {{14, <<"o">>, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"rule-me">>, User, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 14, <<"o">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_after_date(_) ->
    User = #{
        key => <<"user-foo">>,
        custom => #{
            <<"afterDate">> => <<"2018-01-01T12:00:01Z">>
        }
    },
    ExpectedReason = {rule_match, 15, <<"764c5346-6478-4d34-83e7-59c0afc7a15b">>},
    {{15, <<"p">>, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"rule-me">>, User, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 15, <<"p">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_nomatch_in_negated_null_attribute(_) ->
    ExpectedReason = fallthrough,
    {{0, <<"a">>, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"rule-me">>, #{key => <<"no-match">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 0, <<"a">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

fallthrough_rollout(_) ->
    ExpectedReason = fallthrough,
    {{1, <<"b">>, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"roll-me">>, #{key => <<"user-foo">>, secondary => <<"bar">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"roll-me">>, feature_request, 1, <<"b">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

fallthrough_rollout_custom(_) ->
    ExpectedReason = fallthrough,
    User = #{
        key => <<"user-foo">>,
        custom => #{
            <<"customProp">> => <<"514343">>
        }
    },
    {{4, <<"e">>, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"roll-me-custom">>, User, "foo"),
    ExpectedEvents = lists:sort([
        {<<"roll-me-custom">>, feature_request, 4, <<"e">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

fallthrough_rollout_custom_integer(_) ->
    ExpectedReason = fallthrough,
    User = #{
        key => <<"user-foo">>,
        custom => #{
            <<"customProp">> => 514343
        }
    },
    {{4, <<"e">>, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"roll-me-custom">>, User, "foo"),
    ExpectedEvents = lists:sort([
        {<<"roll-me-custom">>, feature_request, 4, <<"e">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

fallthrough_rollout_custom_float(_) ->
    ExpectedReason = fallthrough,
    User = #{
        key => <<"user-foo">>,
        custom => #{
            <<"customProp">> => 514343.0
        }
    },
    {{4, <<"e">>, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"roll-me-custom">>, User, "foo"),
    ExpectedEvents = lists:sort([
        {<<"roll-me-custom">>, feature_request, 4, <<"e">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

fallthrough_rollout_custom_float_invalid(_) ->
    ExpectedReason = fallthrough,
    User = #{
        key => <<"user-foo">>,
        custom => #{
            <<"customProp">> => 514343.05
        }
    },
    {{0, <<"a">>, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"roll-me-custom">>, User, "foo"),
    ExpectedEvents = lists:sort([
        {<<"roll-me-custom">>, feature_request, 0, <<"a">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

fallthrough_rollout_invalid_last_variation(_) ->
    ExpectedReason = fallthrough,
    {{1, <<"b">>, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"roll-me-invalid">>, #{key => <<"user-foo">>, secondary => <<"bar">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"roll-me-invalid">>, feature_request, 1, <<"b">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

variation_out_of_range(_) ->
    {{null, "foo", {error, malformed_flag}}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"bad-variation">>, #{key => <<"some-user">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"bad-variation">>, feature_request, null, "foo", "foo", {error, malformed_flag}, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

extra_fields(_) ->
    ExpectedReason = {rule_match, 0, <<"ab4a9fb3-7e85-429f-8078-23aa70094540">>},
    {{1, false, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"extra-fields">>, #{key => <<"user-12345">>, secondary => <<"bar">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"extra-fields">>, feature_request, 1, false, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

missing_some_fields(_) ->
    ExpectedReason = fallthrough,
    {{0, true, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"missing-some-fields">>, #{key => <<"user-msf">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"missing-some-fields">>, feature_request, 0, true, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

missing_all_fields(_) ->
    ExpectedReason = {error, malformed_flag},
    {{null, "foo", ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"missing-all-fields">>, #{key => <<"user-maf">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"missing-all-fields">>, feature_request, null, "foo", "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

missing_rollout_for_rule(_) -> 
    {{2,<<"FallthroughValue">>,fallthrough}, Events} = ldclient_eval:flag_key_for_user(default, <<"missing-rollout-for-rule">>, #{key => <<"user123">>}, "DefaultValue"),
    ExpectedEvents = lists:sort([{<<"missing-rollout-for-rule">>, feature_request, 2, <<"FallthroughValue">>, "DefaultValue", fallthrough, null}]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

missing_rollout_for_rule_match_rule(_) ->
    % For this test the user key matters because the rule matches keys containing "maybe".
    {{null, "DefaultValue", {error, malformed_flag}}, Events} = ldclient_eval:flag_key_for_user(default, <<"missing-rollout-for-rule">>, #{key => <<"key1Maybe">>}, "DefaultValue"),
    ExpectedEvents = lists:sort([{<<"missing-rollout-for-rule">>, feature_request, null, "DefaultValue", "DefaultValue", {error, malformed_flag}, null}]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

fallthrough_no_rollout_or_variation(_) ->
    {{null, "DefaultValue", {error, malformed_flag}}, Events} = ldclient_eval:flag_key_for_user(default, <<"fallthrough-no-rollout-or-variation">>, #{key => <<"user123">>}, "DefaultValue"),
    ExpectedEvents = lists:sort([{<<"fallthrough-no-rollout-or-variation">>, feature_request, null, "DefaultValue", "DefaultValue", {error, malformed_flag}, null}]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

fallthrough_rollout_in_experiment(_) ->
    ExpectedReason = {fallthrough, in_experiment},
    {{0, <<"a">>, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"experiment-traffic-allocation-v2">>, #{key => <<"userKeyA">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"experiment-traffic-allocation-v2">>, feature_request, 0, <<"a">>, "foo", ExpectedReason, null, true, true}
    ]),
    ActualEvents = lists:sort(extract_events(Events, true)),
    ExpectedEvents = ActualEvents.

fallthrough_rollout_not_in_experiment(_) ->
    ExpectedReason = fallthrough,
    {{1, <<"b">>, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"experiment-traffic-allocation-v2">>, #{key => <<"userKeyB">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"experiment-traffic-allocation-v2">>, feature_request, 1, <<"b">>, "foo", ExpectedReason, null, false, false}
    ]),
    ActualEvents = lists:sort(extract_events(Events, true)),
    ExpectedEvents = ActualEvents.

rule_match_rollout_in_experiment(_) ->
    ExpectedReason = {rule_match, 0, <<"ab4a9fb3-7e85-429f-8078-23aa70094540">>, in_experiment},
    {{0, <<"a">>, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"experiment-traffic-allocation-v2-rules">>, #{key => <<"userKeyA">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"experiment-traffic-allocation-v2-rules">>, feature_request, 0, <<"a">>, "foo", ExpectedReason, null, true, true}
    ]),
    ActualEvents = lists:sort(extract_events(Events, true)),
    ExpectedEvents = ActualEvents.

rule_match_rollout_not_in_experiment(_) ->
    ExpectedReason = {rule_match, 0, <<"ab4a9fb3-7e85-429f-8078-23aa70094540">>},
    {{1, <<"b">>, ExpectedReason}, Events} =
        ldclient_eval:flag_key_for_user(default, <<"experiment-traffic-allocation-v2-rules">>, #{key => <<"userKeyB">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"experiment-traffic-allocation-v2-rules">>, feature_request, 1, <<"b">>, "foo", ExpectedReason, null, true, false}
    ]),
    ActualEvents = lists:sort(extract_events(Events, true)),
    ExpectedEvents = ActualEvents.

all_flags_state(_) ->
    #{<<"$flagsState">> :=
        #{<<"bad-variation">> :=
            #{<<"reason">> :=
                #{errorKind := <<"MALFORMED_FLAG">>,
                    kind := <<"ERROR">>},
                <<"trackEvents">> := true,
                <<"version">> := 5},
        <<"experiment-traffic-allocation-v2">> :=
            #{<<"reason">> :=
                #{inExperiment := true,
                    kind := <<"FALLTHROUGH">>},
                <<"variation">> := 0,
                <<"version">> := 5},
        <<"experiment-traffic-allocation-v2-rules">> :=
            #{<<"reason">> :=
                #{inExperiment := true,
                kind := <<"RULE_MATCH">>,
                ruleId :=
                <<"ab4a9fb3-7e85-429f-8078-23aa70094540">>,
                ruleIndex := 0},
            <<"trackEvents">> := true,
            <<"variation">> := 0,
            <<"version">> := 1},
        <<"extra-fields">> :=
            #{<<"reason">> := #{kind := <<"FALLTHROUGH">>},
            <<"trackEvents">> := true,
            <<"variation">> := 0,
            <<"version">> := 5},
        <<"fallthrough-no-rollout-or-variation">> :=
            #{<<"reason">> :=
            #{errorKind := <<"MALFORMED_FLAG">>,
            kind := <<"ERROR">>},
            <<"trackEvents">> := true,
            <<"version">> := 7243},
        <<"keep-it-deleted">> :=
            #{<<"reason">> :=
            #{errorKind := <<"FLAG_NOT_FOUND">>,
            kind := <<"ERROR">>},
            <<"trackEvents">> := true,
            <<"version">> := 5},
        <<"keep-it-off">> :=
            #{<<"reason">> := #{kind := <<"OFF">>},
            <<"trackEvents">> := true,
            <<"variation">> := 1,
            <<"version">> := 5},
        <<"keep-it-off-null-off-variation">> :=
            #{<<"reason">> := #{kind := <<"OFF">>},
            <<"trackEvents">> := true,
            <<"version">> := 5},
        <<"keep-it-on">> :=
            #{<<"reason">> := #{kind := <<"FALLTHROUGH">>},
            <<"trackEvents">> := true,
            <<"variation">> := 0,
            <<"version">> := 5},
        <<"keep-it-on-another">> :=
            #{<<"reason">> := #{kind := <<"FALLTHROUGH">>},
            <<"trackEvents">> := true,
            <<"variation">> := 0,
            <<"version">> := 5},
        <<"keep-it-on-two">> :=
            #{<<"reason">> := #{kind := <<"FALLTHROUGH">>},
            <<"trackEvents">> := true,
            <<"variation">> := 0,
            <<"version">> := 5},
        <<"missing-all-fields">> :=
            #{<<"reason">> :=
            #{errorKind := <<"MALFORMED_FLAG">>,
                kind := <<"ERROR">>},
                <<"version">> := 0},
        <<"missing-rollout-for-rule">> :=
            #{<<"reason">> := #{kind := <<"FALLTHROUGH">>},
            <<"trackEvents">> := true,
            <<"variation">> := 2,
                <<"version">> := 4243},
        <<"missing-some-fields">> :=
            #{<<"variation">> := 0,
            <<"version">> := 0},
        <<"prereqs-fail-off">> :=
            #{<<"reason">> :=
            #{kind := <<"PREREQUISITE_FAILED">>,
                prerequisiteKey := <<"keep-it-off">>},
            <<"trackEvents">> := true,
            <<"variation">> := 1,
            <<"version">> := 245},
        <<"prereqs-fail-off-null">> :=
            #{<<"reason">> :=
                #{kind := <<"PREREQUISITE_FAILED">>,
                    prerequisiteKey := <<"keep-it-off">>},
            <<"trackEvents">> := true,
            <<"version">> := 2},
        <<"prereqs-fail-variation">> :=
            #{<<"reason">> :=
            #{kind := <<"PREREQUISITE_FAILED">>,
                prerequisiteKey := <<"keep-it-on">>},
                <<"trackEvents">> := true,
                <<"variation">> := 1,
                <<"version">> := 245},
        <<"prereqs-success">> :=
            #{<<"reason">> := #{kind := <<"FALLTHROUGH">>},
                <<"trackEvents">> := true,
                <<"variation">> := 1,
                <<"version">> := 24},
        <<"roll-me">> :=
            #{<<"reason">> := #{kind := <<"FALLTHROUGH">>},
            <<"trackEvents">> := true,
            <<"variation">> := 4,
            <<"version">> := 5},
        <<"roll-me-custom">> :=
            #{<<"reason">> := #{kind := <<"FALLTHROUGH">>},
            <<"trackEvents">> := true,
            <<"variation">> := 0,
            <<"version">> := 5},
        <<"roll-me-invalid">> :=
            #{<<"reason">> := #{kind := <<"FALLTHROUGH">>},
            <<"trackEvents">> := true,
            <<"variation">> := 1,
            <<"version">> := 5},
        <<"rule-me">> :=
            #{<<"reason">> := #{kind := <<"FALLTHROUGH">>},
            <<"trackEvents">> := true,
            <<"variation">> := 0,
            <<"version">> := 5},
        <<"segment-me">> :=
            #{<<"reason">> :=
                #{kind := <<"RULE_MATCH">>,
                    ruleId :=
                    <<"489a185d-caaf-4db9-b192-e09e927d070c">>,
                    ruleIndex := 1},
            <<"trackEvents">> := true,
            <<"variation">> := 1,
            <<"version">> := 5},
        <<"target-me">> :=
            #{<<"reason">> := #{kind := <<"FALLTHROUGH">>},
            <<"trackEvents">> := true,
            <<"variation">> := 0,
            <<"version">> := 5}
        },
        <<"$valid">> := true,
        <<"bad-variation">> := null,
        <<"experiment-traffic-allocation-v2">> := <<"a">>,
        <<"experiment-traffic-allocation-v2-rules">> := <<"a">>,
        <<"extra-fields">> := true,
        <<"fallthrough-no-rollout-or-variation">> := null,
        <<"keep-it-deleted">> := null,
        <<"keep-it-off">> := false,
        <<"keep-it-off-null-off-variation">> := null,
        <<"keep-it-on">> := true,
        <<"keep-it-on-another">> := true,
        <<"keep-it-on-two">> := true,
        <<"missing-all-fields">> := null,
        <<"missing-rollout-for-rule">> := <<"FallthroughValue">>,
        <<"missing-some-fields">> := true,
        <<"prereqs-fail-off">> := false,
        <<"prereqs-fail-off-null">> := null,
        <<"prereqs-fail-variation">> := false,
        <<"prereqs-success">> := false,
        <<"roll-me">> := <<"e">>,
        <<"roll-me-custom">> := <<"a">>,
        <<"roll-me-invalid">> := <<"b">>,
        <<"rule-me">> := <<"a">>,
        <<"segment-me">> := false,
        <<"target-me">> := true
    } = ldclient_eval:all_flags_state(#{key => <<"userKeyA">>}, #{with_reasons => true}, default).

all_flags_state_with_reason(_) ->
    #{<<"$flagsState">> :=
    #{<<"bad-variation">> :=
    #{<<"reason">> :=
    #{errorKind := <<"MALFORMED_FLAG">>,
        kind := <<"ERROR">>},
        <<"trackEvents">> := true,
        <<"version">> := 5},
        <<"experiment-traffic-allocation-v2">> :=
        #{<<"reason">> :=
        #{inExperiment := true,
            kind := <<"FALLTHROUGH">>},
            <<"variation">> := 0,
            <<"version">> := 5},
        <<"experiment-traffic-allocation-v2-rules">> :=
        #{<<"reason">> :=
        #{inExperiment := true,
            kind := <<"RULE_MATCH">>,
            ruleId :=
            <<"ab4a9fb3-7e85-429f-8078-23aa70094540">>,
            ruleIndex := 0},
            <<"trackEvents">> := true,
            <<"variation">> := 0,
            <<"version">> := 1},
        <<"extra-fields">> :=
        #{<<"reason">> := #{kind := <<"FALLTHROUGH">>},
            <<"trackEvents">> := true,<<"variation">> := 0,
            <<"version">> := 5},
        <<"fallthrough-no-rollout-or-variation">> :=
        #{<<"reason">> :=
        #{errorKind := <<"MALFORMED_FLAG">>,
            kind := <<"ERROR">>},
            <<"trackEvents">> := true,
            <<"version">> := 7243},
        <<"keep-it-deleted">> :=
        #{<<"reason">> :=
        #{errorKind := <<"FLAG_NOT_FOUND">>,
            kind := <<"ERROR">>},
            <<"trackEvents">> := true,
            <<"version">> := 5},
        <<"keep-it-off">> :=
        #{<<"reason">> := #{kind := <<"OFF">>},
            <<"trackEvents">> := true,
            <<"variation">> := 1,
            <<"version">> := 5},
        <<"keep-it-off-null-off-variation">> :=
        #{<<"reason">> := #{kind := <<"OFF">>},
            <<"trackEvents">> := true,
            <<"version">> := 5},
        <<"keep-it-on">> :=
        #{<<"reason">> := #{kind := <<"FALLTHROUGH">>},
            <<"trackEvents">> := true,
            <<"variation">> := 0,
            <<"version">> := 5},
        <<"keep-it-on-another">> :=
        #{<<"reason">> := #{kind := <<"FALLTHROUGH">>},
            <<"trackEvents">> := true,
            <<"variation">> := 0,
            <<"version">> := 5},
        <<"keep-it-on-two">> :=
        #{<<"reason">> := #{kind := <<"FALLTHROUGH">>},
            <<"trackEvents">> := true,
            <<"variation">> := 0,
            <<"version">> := 5},
        <<"missing-all-fields">> :=
        #{<<"reason">> :=
        #{errorKind := <<"MALFORMED_FLAG">>,
            kind := <<"ERROR">>},
            <<"version">> := 0},
        <<"missing-rollout-for-rule">> :=
        #{<<"reason">> := #{kind := <<"FALLTHROUGH">>},
            <<"trackEvents">> := true,
            <<"variation">> := 2,
            <<"version">> := 4243},
        <<"missing-some-fields">> :=
        #{<<"reason">> := #{kind := <<"FALLTHROUGH">>},
            <<"variation">> := 0,
            <<"version">> := 0},
        <<"prereqs-fail-off">> :=
        #{<<"reason">> :=
        #{kind := <<"PREREQUISITE_FAILED">>,
            prerequisiteKey := <<"keep-it-off">>},
            <<"trackEvents">> := true,
            <<"variation">> := 1,
            <<"version">> := 245},
        <<"prereqs-fail-off-null">> :=
        #{<<"reason">> :=
        #{kind := <<"PREREQUISITE_FAILED">>,
            prerequisiteKey := <<"keep-it-off">>},
            <<"trackEvents">> := true,
            <<"version">> := 2},
        <<"prereqs-fail-variation">> :=
        #{<<"reason">> :=
        #{kind := <<"PREREQUISITE_FAILED">>,
            prerequisiteKey := <<"keep-it-on">>},
            <<"trackEvents">> := true,
            <<"variation">> := 1,
            <<"version">> := 245},
        <<"prereqs-success">> :=
        #{<<"reason">> := #{kind := <<"FALLTHROUGH">>},
            <<"trackEvents">> := true,
            <<"variation">> := 1,
            <<"version">> := 24},
        <<"roll-me">> :=
        #{<<"reason">> := #{kind := <<"FALLTHROUGH">>},
            <<"trackEvents">> := true,
            <<"variation">> := 4,
            <<"version">> := 5},
        <<"roll-me-custom">> :=
        #{<<"reason">> := #{kind := <<"FALLTHROUGH">>},
            <<"trackEvents">> := true,
            <<"variation">> := 0,
            <<"version">> := 5},
        <<"roll-me-invalid">> :=
        #{<<"reason">> := #{kind := <<"FALLTHROUGH">>},
            <<"trackEvents">> := true,
            <<"variation">> := 1,
            <<"version">> := 5},
        <<"rule-me">> :=
        #{<<"reason">> := #{kind := <<"FALLTHROUGH">>},
            <<"trackEvents">> := true,
            <<"variation">> := 0,
            <<"version">> := 5},
        <<"segment-me">> :=
        #{<<"reason">> :=
        #{kind := <<"RULE_MATCH">>,
            ruleId :=
            <<"489a185d-caaf-4db9-b192-e09e927d070c">>,
            ruleIndex := 1},
            <<"trackEvents">> := true,
            <<"variation">> := 1,
            <<"version">> := 5},
        <<"target-me">> :=
        #{<<"reason">> := #{kind := <<"FALLTHROUGH">>},
            <<"trackEvents">> := true,
            <<"variation">> := 0,
            <<"version">> := 5}
    },
        <<"$valid">> := true,
        <<"bad-variation">> := null,
        <<"experiment-traffic-allocation-v2">> := <<"a">>,
        <<"experiment-traffic-allocation-v2-rules">> := <<"a">>,
        <<"extra-fields">> := true,
        <<"fallthrough-no-rollout-or-variation">> := null,
        <<"keep-it-deleted">> := null,
        <<"keep-it-off">> := false,
        <<"keep-it-off-null-off-variation">> := null,
        <<"keep-it-on">> := true,
        <<"keep-it-on-another">> := true,
        <<"keep-it-on-two">> := true,
        <<"missing-all-fields">> := null,
        <<"missing-rollout-for-rule">> := <<"FallthroughValue">>,
        <<"missing-some-fields">> := true,
        <<"prereqs-fail-off">> := false,
        <<"prereqs-fail-off-null">> := null,
        <<"prereqs-fail-variation">> := false,
        <<"prereqs-success">> := false,
        <<"roll-me">> := <<"e">>,
        <<"roll-me-custom">> := <<"a">>,
        <<"roll-me-invalid">> := <<"b">>,
        <<"rule-me">> := <<"a">>,
        <<"segment-me">> := false,
        <<"target-me">> := true
    } = ldclient_eval:all_flags_state(#{key => <<"userKeyA">>}, #{with_reasons => true}, default).

all_flags_state_offline(_) ->
    #{
        <<"$flagsState">> := #{},
        <<"$valid">> := false
    } = ldclient_eval:all_flags_state(#{key => <<"userKeyA">>}, #{with_reasons => true}, offline).
