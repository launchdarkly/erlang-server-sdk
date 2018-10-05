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
    off_flag/1,
    prerequisite_fail_off/1,
    prerequisite_fail_variation/1,
    prerequisite_success/1,
    target_user/1,
    segment_included/1,
    segment_excluded_negated/1,
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
    rule_match_after_int/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        unknown_flag,
        off_flag,
        prerequisite_fail_off,
        prerequisite_fail_variation,
        prerequisite_success,
        target_user,
        segment_included,
        segment_excluded_negated,
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
        rule_match_after_int
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(eld),
    application:set_env(eld, storage_backend, eld_storage_ets),
    eld:start_storage(eld_storage_ets),
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
    {ok, PutData} = file:read_file(DataFilename),
    ok = eld_stream_server:process_event(#{event => <<"put">>, data => PutData}, eld_storage_ets).

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
    {{undefined, "foo", {error, flag_not_found}}, Events} =
        eld_eval:flag_key_for_user(<<"flag-that-does-not-exist">>, #{key => <<"some-user">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"flag-that-does-not-exist">>, feature_request, undefined, undefined, "foo", {error, flag_not_found}, undefined}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

off_flag(_) ->
    {{1, false, off}, Events} = eld_eval:flag_key_for_user(<<"keep-it-off">>, #{key => <<"user123">>}, "foo"),
    ExpectedEvents = lists:sort([{<<"keep-it-off">>, feature_request, 1, false, "foo", off, undefined}]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

prerequisite_fail_off(_) ->
    {{1, false, {prerequisite_failed, [<<"keep-it-off">>]}}, Events} =
        eld_eval:flag_key_for_user(<<"prereqs-fail-off">>, #{key => <<"user123">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"keep-it-off">>, feature_request, 0, true, undefined, fallthrough, <<"prereqs-fail-off">>},
        {<<"prereqs-fail-off">>, feature_request, 1, false, "foo", {prerequisite_failed, [<<"keep-it-off">>]}, undefined}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

prerequisite_fail_variation(_) ->
    {{1, false, {prerequisite_failed, [<<"keep-it-on">>]}}, Events} =
        eld_eval:flag_key_for_user(<<"prereqs-fail-variation">>, #{key => <<"user123">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"keep-it-on">>, feature_request, 0, true, undefined, fallthrough, <<"prereqs-fail-variation">>},
        {<<"keep-it-on-two">>, feature_request, 0, true, undefined, fallthrough, <<"keep-it-on">>},
        {<<"prereqs-fail-variation">>, feature_request, 1, false, "foo", {prerequisite_failed, [<<"keep-it-on">>]}, undefined}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

prerequisite_success(_) ->
    {{1, false, fallthrough}, Events} =
        eld_eval:flag_key_for_user(<<"prereqs-success">>, #{key => <<"user123">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"prereqs-success">>, feature_request, 1, false, "foo", fallthrough, undefined},
        {<<"keep-it-on-another">>, feature_request, 0, true, undefined, fallthrough, <<"prereqs-success">>},
        {<<"keep-it-on">>, feature_request, 0, true, undefined, fallthrough, <<"prereqs-success">>},
        {<<"keep-it-on-two">>, feature_request, 0, true, undefined, fallthrough, <<"keep-it-on">>}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

target_user(_) ->
    {{0, true, target_match}, Events} =
        eld_eval:flag_key_for_user(<<"target-me">>, #{key => <<"user-33333">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"target-me">>, feature_request, 0, true, "foo", target_match, undefined}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

segment_included(_) ->
    ExpectedReason = {rule_match, 0, <<"ab4a9fb3-7e85-429f-8078-23aa70094540">>},
    {{1, false, ExpectedReason}, Events} =
        eld_eval:flag_key_for_user(<<"segment-me">>, #{key => <<"user-12345">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"segment-me">>, feature_request, 1, false, "foo", ExpectedReason, undefined}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

segment_excluded_negated(_) ->
    ExpectedReason = {rule_match, 1, <<"ab4a9fb3-7e85-429f-8078-23aa70094540">>},
    {{1, false, ExpectedReason}, Events} =
        eld_eval:flag_key_for_user(<<"segment-me">>, #{key => <<"user-33333">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"segment-me">>, feature_request, 1, false, "foo", ExpectedReason, undefined}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_in(_) ->
    ExpectedReason = {rule_match, 0, <<"08b9b261-5df6-4881-892b-e25bdb28b6d3">>},
    {{0, <<"a">>, ExpectedReason}, Events} =
        eld_eval:flag_key_for_user(<<"rule-me">>, #{key => <<"user-key-match@example.com">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 0, <<"a">>, "foo", ExpectedReason, undefined}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_ends_with(_) ->
    ExpectedReason = {rule_match, 1, <<"2fac50d0-d912-424a-831e-ab60ad0547b4">>},
    {{1, <<"b">>, ExpectedReason}, Events} =
        eld_eval:flag_key_for_user(<<"rule-me">>, #{key => <<"user-ends-with@example.com">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 1, <<"b">>, "foo", ExpectedReason, undefined}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_ends_and_starts_with_order(_) ->
    ExpectedReason = {rule_match, 1, <<"2fac50d0-d912-424a-831e-ab60ad0547b4">>},
    {{1, <<"b">>, ExpectedReason}, Events} =
        eld_eval:flag_key_for_user(<<"rule-me">>, #{key => <<"user-starts-with@example.com">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 1, <<"b">>, "foo", ExpectedReason, undefined}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_starts_with(_) ->
    ExpectedReason = {rule_match, 2, <<"e3b70ddf-a000-4649-93c5-ac0eaea675f8">>},
    {{2, <<"c">>, ExpectedReason}, Events} =
        eld_eval:flag_key_for_user(<<"rule-me">>, #{key => <<"user-starts-with@foo.com">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 2, <<"c">>, "foo", ExpectedReason, undefined}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_regex(_) ->
    ExpectedReason = {rule_match, 3, <<"1d63c99a-3016-4778-bf1f-68d1fce5004e">>},
    {{3, <<"d">>, ExpectedReason}, Events} =
        eld_eval:flag_key_for_user(<<"rule-me">>, #{key => <<"user-regex-match@foo.com">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 3, <<"d">>, "foo", ExpectedReason, undefined}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_contains(_) ->
    ExpectedReason = {rule_match, 4, <<"1f1dadfc-0e66-42e0-b479-979186d972ce">>},
    {{4, <<"e">>, ExpectedReason}, Events} =
        eld_eval:flag_key_for_user(<<"rule-me">>, #{key => <<"user-contains@foo.com">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 4, <<"e">>, "foo", ExpectedReason, undefined}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_less_than(_) ->
    ExpectedReason = {rule_match, 5, <<"ca092500-1cb7-4b14-a11c-81b46ca19cae">>},
    {{5, <<"f">>, ExpectedReason}, Events} =
        eld_eval:flag_key_for_user(<<"rule-me">>, #{key => <<"user-foo">>, <<"custom1">> => 30}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 5, <<"f">>, "foo", ExpectedReason, undefined}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_less_than_or_equal(_) ->
    ExpectedReason = {rule_match, 6, <<"d38e11f8-93d1-453e-8022-6d8ed7f106ea">>},
    {{6, <<"g">>, ExpectedReason}, Events} =
        eld_eval:flag_key_for_user(<<"rule-me">>, #{key => <<"user-foo">>, <<"custom1">> => 50}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 6, <<"g">>, "foo", ExpectedReason, undefined}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_greater_than(_) ->
    ExpectedReason = {rule_match, 7, <<"a92a93c2-2004-482b-9e4a-38abe81d7050">>},
    {{7, <<"h">>, ExpectedReason}, Events} =
        eld_eval:flag_key_for_user(<<"rule-me">>, #{key => <<"user-foo">>, <<"custom2">> => 70}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 7, <<"h">>, "foo", ExpectedReason, undefined}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_greater_than_or_equal(_) ->
    ExpectedReason = {rule_match, 8, <<"9158e01a-a70f-4924-8cf8-9401e2cf6c67">>},
    {{8, <<"i">>, ExpectedReason}, Events} =
        eld_eval:flag_key_for_user(<<"rule-me">>, #{key => <<"user-foo">>, <<"custom2">> => 50}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 8, <<"i">>, "foo", ExpectedReason, undefined}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_before_int(_) ->
    ExpectedReason = {rule_match, 9, <<"500633a7-2c82-4baf-8201-4892b68b31b4">>},
    {{9, <<"j">>, ExpectedReason}, Events} =
        eld_eval:flag_key_for_user(<<"rule-me">>, #{key => <<"user-foo">>, <<"date">> => 1451772244}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 9, <<"j">>, "foo", ExpectedReason, undefined}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.

rule_match_after_int(_) ->
    ExpectedReason = {rule_match, 10, <<"77473bea-d93f-4787-84d2-92cf08b35f2b">>},
    {{10, <<"k">>, ExpectedReason}, Events} =
        eld_eval:flag_key_for_user(<<"rule-me">>, #{key => <<"user-foo">>, <<"date">> => 1451772246}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"rule-me">>, feature_request, 10, <<"k">>, "foo", ExpectedReason, undefined}
    ]),
    ActualEvents = lists:sort(extract_events(Events)),
    ExpectedEvents = ActualEvents.
