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
    off_flag/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        unknown_flag,
        off_flag
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

get_flags() ->
    <<"\"abc\":{",
        "\"clientSide\":false,",
        "\"debugEventsUntilDate\":null,",
        "\"deleted\":false,",
        "\"fallthrough\":{\"variation\":0},",
        "\"key\":\"abc\",",
        "\"offVariation\":1,",
        "\"on\":false,",
        "\"prerequisites\":[],",
        "\"rules\":[],",
        "\"salt\":\"d0888ec5921e45c7af5bc10b47b033ba\",",
        "\"sel\":\"8b4d79c59adb4df492ebea0bf65dfd4c\",",
        "\"targets\":[],",
        "\"trackEvents\":true,",
        "\"variations\":[true,false],",
        "\"version\":5",
    "}">>.

create_flags() ->
    FlagsBin = get_flags(),
    PutData = <<"{\"path\":\"/\",",
        "\"data\":{",
        "\"flags\":{",
            FlagsBin/binary,
        "},",
        "\"segments\":{}",
        "}",
        "}">>,
    ok = eld_stream_server:process_event(#{event => <<"put">>, data => PutData}, eld_storage_ets).

%%====================================================================
%% Tests
%%====================================================================

unknown_flag(_) ->
    FlagKey = <<"flag-that-does-not-exist">>,
    {{undefined, "foo", {error, flag_not_found}}, _} = eld_eval:flag_key_for_user(FlagKey, "some-user", "foo").

off_flag(_) ->
    {{1, false, off}, _} = eld_eval:flag_key_for_user(<<"abc">>, "some-user", "foo").
