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
    unknown_flag/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        unknown_flag
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(eld),
    application:set_env(eld, storage_backend, eld_storage_ets),
    eld:start_storage(eld_storage_ets),
    Config.

end_per_suite(_) ->
    ok = application:stop(eld).

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok = eld_storage_ets:empty(flags),
    ok.

%%====================================================================
%% Helpers
%%====================================================================

%%====================================================================
%% Tests
%%====================================================================

unknown_flag(_) ->
    FlagKey = <<"flag-that-does-not-exist">>,
    {undefined, undefined, {error, flag_not_found}, _} = eld_eval:flag_key_for_user(FlagKey, "some-user").
