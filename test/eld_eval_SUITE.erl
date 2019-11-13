-module(eld_eval_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
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
    variation_out_of_range/1,
    extra_fields/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
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
        variation_out_of_range,
        extra_fields
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(eld),
    eld:start_instance("", #{start_stream => false}),
    eld:start_instance("", another1, #{start_stream => false}),
    ok = create_flags(),
    Config.

end_per_suite(_) ->
    ok = application:stop(eld).

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.

%%====================================================================
%% Helpers
%%====================================================================

create_flags() ->
    DataFilename = code:priv_dir(eld) ++ "/flags-segments-put-data.json",
    DataFilename2 = code:priv_dir(eld) ++ "/flags-segments-put-data-another1.json",
    {ok, PutData} = file:read_file(DataFilename),
    {ok, PutData2} = file:read_file(DataFilename2),
    ok = eld_stream_server:process_event(#{event => <<"put">>, data => PutData}, eld_storage_ets, default),
    ok = eld_stream_server:process_event(#{event => <<"put">>, data => PutData2}, eld_storage_ets, another1).

extract_events(Events) ->
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
    ].

%%====================================================================
%% Tests
%%====================================================================

unknown_flag(_) ->
    {{null, "foo", {error, flag_not_found}}, Events} =
        eld_eval:flag_key_for_user(default, <<"flag-that-does-not-exist">>, #{key => <<"some-user">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"flag-that-does-not-exist">>, feature_request, null, "foo", "foo", {error, flag_not_found}, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

unknown_flag_another(_) ->
    % Flag exists in default instance, doesn't exist in another1 instance
    {{null, "foo", {error, flag_not_found}}, Events} =
        eld_eval:flag_key_for_user(another1, <<"bad-variation">>, #{key => <<"some-user">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"bad-variation">>, feature_request, null, "foo", "foo", {error, flag_not_found}, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

user_with_no_key(_) ->
    {{null, "foo", {error, user_not_specified}}, Events} =
        eld_eval:flag_key_for_user(default, <<"keep-it-off">>, #{name => <<"some-user">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"keep-it-off">>, feature_request, null, "foo", "foo", {error, user_not_specified}, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

user_with_null_key(_) ->
    {{null, "foo", {error, user_not_specified}}, Events} =
        eld_eval:flag_key_for_user(default, <<"keep-it-off">>, #{key => null}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"keep-it-off">>, feature_request, null, "foo", "foo", {error, user_not_specified}, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

off_flag(_) ->
    {{1, false, off}, Events} = eld_eval:flag_key_for_user(default, <<"keep-it-off">>, #{key => <<"user123">>}, "foo"),
    ExpectedEvents = lists:sort([{<<"keep-it-off">>, feature_request, 1, false, "foo", off, null}]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

off_flag_another(_) ->
    {{1, false, off}, Events} = eld_eval:flag_key_for_user(another1, <<"keep-it-off">>, #{key => <<"user123">>}, "foo"),
    ExpectedEvents = lists:sort([{<<"keep-it-off">>, feature_request, 1, false, "foo", off, null}]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

off_flag_null_off_variation(_) ->
    {{null, "foo", off}, Events} = eld_eval:flag_key_for_user(default, <<"keep-it-off-null-off-variation">>, #{key => <<"user123">>}, "foo"),
    ExpectedEvents = lists:sort([{<<"keep-it-off-null-off-variation">>, feature_request, null, "foo", "foo", off, null}]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

deleted_flag(_) ->
    {{null, "foo", {error, flag_not_found}}, Events} = eld_eval:flag_key_for_user(default, <<"keep-it-deleted">>, #{key => <<"user123">>}, "foo"),
    ExpectedEvents = lists:sort([{<<"keep-it-deleted">>, feature_request, null, "foo", "foo", {error, flag_not_found}, null}]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

prerequisite_fail_off(_) ->
    {{1, false, {prerequisite_failed, [<<"keep-it-off">>]}}, Events} =
        eld_eval:flag_key_for_user(default, <<"prereqs-fail-off">>, #{key => <<"user123">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"prereqs-fail-off">>, feature_request, 1, false, "foo", {prerequisite_failed, [<<"keep-it-off">>]}, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

prerequisite_fail_off_null(_) ->
    {{null, "foo", {prerequisite_failed, [<<"keep-it-off">>]}}, Events} =
        eld_eval:flag_key_for_user(default, <<"prereqs-fail-off-null">>, #{key => <<"user123">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"prereqs-fail-off-null">>, feature_request, null, "foo", "foo", {prerequisite_failed, [<<"keep-it-off">>]}, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

prerequisite_fail_variation(_) ->
    {{1, false, {prerequisite_failed, [<<"keep-it-on">>]}}, Events} =
        eld_eval:flag_key_for_user(default, <<"prereqs-fail-variation">>, #{key => <<"user123">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"keep-it-on">>, feature_request, 0, true, null, fallthrough, <<"prereqs-fail-variation">>},
        {<<"keep-it-on-two">>, feature_request, 0, true, null, fallthrough, <<"keep-it-on">>},
        {<<"prereqs-fail-variation">>, feature_request, 1, false, "foo", {prerequisite_failed, [<<"keep-it-on">>]}, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

prerequisite_success(_) ->
    {{1, false, fallthrough}, Events} =
        eld_eval:flag_key_for_user(default, <<"prereqs-success">>, #{key => <<"user123">>}, "foo"),
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
        eld_eval:flag_key_for_user(default, <<"target-me">>, #{key => <<"user-33333">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"target-me">>, feature_request, 0, true, "foo", target_match, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

target_user_another(_) ->
    % Same user, different instance, different target result
    {{1, false, target_match}, Events} =
        eld_eval:flag_key_for_user(another1, <<"target-me">>, #{key => <<"user-33333">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"target-me">>, feature_request, 1, false, "foo", target_match, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

segment_included(_) ->
    ExpectedReason = {rule_match, 0, <<"ab4a9fb3-7e85-429f-8078-23aa70094540">>},
    {{1, false, ExpectedReason}, Events} =
        eld_eval:flag_key_for_user(default, <<"segment-me">>, #{key => <<"user-12345">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"segment-me">>, feature_request, 1, false, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

segment_excluded_negated(_) ->
    ExpectedReason = {rule_match, 1, <<"489a185d-caaf-4db9-b192-e09e927d070c">>},
    {{1, false, ExpectedReason}, Events} =
        eld_eval:flag_key_for_user(default, <<"segment-me">>, #{key => <<"user-33333">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"segment-me">>, feature_request, 1, false, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

segment_excluded_negated_nonuser(_) ->
    % This user isn't specified in a segment
    ExpectedReason = {rule_match, 1, <<"489a185d-caaf-4db9-b192-e09e927d070c">>},
    {{1, false, ExpectedReason}, Events} =
        eld_eval:flag_key_for_user(default, <<"segment-me">>, #{key => <<"user-99999">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"segment-me">>, feature_request, 1, false, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

segment_excluded_another(_) ->
    % Same user, different instance, different segment result
    ExpectedReason = {rule_match, 1, <<"489a185d-caaf-4db9-b192-e09e927d070c">>},
    {{1, false, ExpectedReason}, Events} =
        eld_eval:flag_key_for_user(another1, <<"segment-me">>, #{key => <<"user-12345">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"segment-me">>, feature_request, 1, false, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_in(_) ->
    ExpectedReason = {rule_match, 0, <<"08b9b261-5df6-4881-892b-e25bdb28b6d3">>},
    {{0, <<"a">>, ExpectedReason}, Events} =
        eld_eval:flag_key_for_user(default, <<"rule-me">>, #{key => <<"user-key-match@example.com">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 0, <<"a">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_ends_with(_) ->
    ExpectedReason = {rule_match, 1, <<"2fac50d0-d912-424a-831e-ab60ad0547b4">>},
    {{1, <<"b">>, ExpectedReason}, Events} =
        eld_eval:flag_key_for_user(default, <<"rule-me">>, #{key => <<"user-ends-with@example.com">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 1, <<"b">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_ends_and_starts_with_order(_) ->
    ExpectedReason = {rule_match, 1, <<"2fac50d0-d912-424a-831e-ab60ad0547b4">>},
    {{1, <<"b">>, ExpectedReason}, Events} =
        eld_eval:flag_key_for_user(default, <<"rule-me">>, #{key => <<"user-starts-with@example.com">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 1, <<"b">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_starts_with(_) ->
    ExpectedReason = {rule_match, 2, <<"e3b70ddf-a000-4649-93c5-ac0eaea675f8">>},
    {{2, <<"c">>, ExpectedReason}, Events} =
        eld_eval:flag_key_for_user(default, <<"rule-me">>, #{key => <<"user-starts-with@foo.com">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 2, <<"c">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_regex(_) ->
    ExpectedReason = {rule_match, 3, <<"1d63c99a-3016-4778-bf1f-68d1fce5004e">>},
    {{3, <<"d">>, ExpectedReason}, Events} =
        eld_eval:flag_key_for_user(default, <<"rule-me">>, #{key => <<"user-regex-match@foo.com">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 3, <<"d">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_contains(_) ->
    ExpectedReason = {rule_match, 4, <<"1f1dadfc-0e66-42e0-b479-979186d972ce">>},
    {{4, <<"e">>, ExpectedReason}, Events} =
        eld_eval:flag_key_for_user(default, <<"rule-me">>, #{key => <<"user-contains@foo.com">>}, "foo"),
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
        eld_eval:flag_key_for_user(default, <<"rule-me">>, User, "foo"),
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
        eld_eval:flag_key_for_user(default, <<"rule-me">>, User, "foo"),
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
        eld_eval:flag_key_for_user(default, <<"rule-me">>, User, "foo"),
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
        eld_eval:flag_key_for_user(default, <<"rule-me">>, User, "foo"),
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
        eld_eval:flag_key_for_user(default, <<"rule-me">>, User, "foo"),
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
        eld_eval:flag_key_for_user(default, <<"rule-me">>, User, "foo"),
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
        eld_eval:flag_key_for_user(default, <<"rule-me">>, User, "foo"),
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
        eld_eval:flag_key_for_user(default, <<"rule-me">>, User, "foo"),
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
        eld_eval:flag_key_for_user(default, <<"rule-me">>, User, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 13, <<"n">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_before_date(_) ->
    User = #{
        key => <<"user-foo">>,
        custom => #{
            <<"beforeDate">> => <<"2018-01-01T11:59:59">>
        }
    },
    ExpectedReason = {rule_match, 14, <<"b6c5ceec-364d-4c23-a041-7865f4f136d3">>},
    {{14, <<"o">>, ExpectedReason}, Events} =
        eld_eval:flag_key_for_user(default, <<"rule-me">>, User, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 14, <<"o">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_after_date(_) ->
    User = #{
        key => <<"user-foo">>,
        custom => #{
            <<"afterDate">> => <<"2018-01-01T12:00:01">>
        }
    },
    ExpectedReason = {rule_match, 15, <<"764c5346-6478-4d34-83e7-59c0afc7a15b">>},
    {{15, <<"p">>, ExpectedReason}, Events} =
        eld_eval:flag_key_for_user(default, <<"rule-me">>, User, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 15, <<"p">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_nomatch_in_negated_null_attribute(_) ->
    ExpectedReason = fallthrough,
    {{0, <<"a">>, ExpectedReason}, Events} =
        eld_eval:flag_key_for_user(default, <<"rule-me">>, #{key => <<"no-match">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 0, <<"a">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

fallthrough_rollout(_) ->
    ExpectedReason = fallthrough,
    {{1, <<"b">>, ExpectedReason}, Events} =
        eld_eval:flag_key_for_user(default, <<"roll-me">>, #{key => <<"user-foo">>, secondary => <<"bar">>}, "foo"),
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
        eld_eval:flag_key_for_user(default, <<"roll-me-custom">>, User, "foo"),
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
        eld_eval:flag_key_for_user(default, <<"roll-me-custom">>, User, "foo"),
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
        eld_eval:flag_key_for_user(default, <<"roll-me-custom">>, User, "foo"),
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
        eld_eval:flag_key_for_user(default, <<"roll-me-custom">>, User, "foo"),
    ExpectedEvents = lists:sort([
        {<<"roll-me-custom">>, feature_request, 0, <<"a">>, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

variation_out_of_range(_) ->
    {{null, null, {error, malformed_flag}}, Events} =
        eld_eval:flag_key_for_user(default, <<"bad-variation">>, #{key => <<"some-user">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"bad-variation">>, feature_request, null, null, "foo", {error, malformed_flag}, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

extra_fields(_) ->
    ExpectedReason = {rule_match, 0, <<"ab4a9fb3-7e85-429f-8078-23aa70094540">>},
    {{1, false, ExpectedReason}, Events} =
        eld_eval:flag_key_for_user(default, <<"extra-fields">>, #{key => <<"user-12345">>, secondary => <<"bar">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"extra-fields">>, feature_request, 1, false, "foo", ExpectedReason, null}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.
