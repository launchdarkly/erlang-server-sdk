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
    segment_excluded_negated/1
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
        segment_excluded_negated
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
    {{0, true, fallthrough}, Events} =
        eld_eval:flag_key_for_user(<<"prereqs-success">>, #{key => <<"user123">>}, "foo"),
    ExpectedEvents = lists:sort([
        {<<"prereqs-success">>, feature_request, 0, true, "foo", fallthrough, undefined},
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
