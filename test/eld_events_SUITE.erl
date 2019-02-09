-module(eld_events_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    add_flag_eval_events_flush/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        add_flag_eval_events_flush
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(eld),
    eld:start_instance("", #{start_stream => false}),
    eld:start_instance("", another1, #{start_stream => false}),
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

get_simple_flag() ->
    {
        <<"abc">>,
        <<"\"abc\":{",
            "\"clientSide\":false,",
            "\"debugEventsUntilDate\":null,",
            "\"deleted\":false,",
            "\"fallthrough\":{\"variation\":0},",
            "\"key\":\"abc\",",
            "\"offVariation\":1,",
            "\"on\":true,",
            "\"prerequisites\":[],",
            "\"rules\":[],",
            "\"salt\":\"d0888ec5921e45c7af5bc10b47b033ba\",",
            "\"sel\":\"8b4d79c59adb4df492ebea0bf65dfd4c\",",
            "\"targets\":[],",
            "\"trackEvents\":true,",
            "\"variations\":[true,false],",
            "\"version\":5",
            "}">>,
        #{
            <<"clientSide">> => false,
            <<"debugEventsUntilDate">> => null,
            <<"deleted">> => false,
            <<"fallthrough">> => #{<<"variation">> => 0},
            <<"key">> => <<"abc">>,
            <<"offVariation">> => 1,
            <<"on">> => true,
            <<"prerequisites">> => [],
            <<"rules">> => [],
            <<"salt">> => <<"d0888ec5921e45c7af5bc10b47b033ba">>,
            <<"sel">> => <<"8b4d79c59adb4df492ebea0bf65dfd4c">>,
            <<"targets">> => [],
            <<"trackEvents">> => true,
            <<"variations">> => [true,false],
            <<"version">> => 5
        }
    }.

%%====================================================================
%% Tests
%%====================================================================

add_flag_eval_events_flush(_) ->
    {FlagKey, _, FlagBin} = get_simple_flag(),
    Flag = eld_flag:new(FlagKey, FlagBin),
    Event = eld_event:new_flag_eval(
        5,
        "variation-value-5",
        "default-value",
        #{key => <<"12345">>},
        target_match,
        Flag
    ),
    ok = eld_event_server:add_event(default, Event),
    ok = eld_event_server:flush(default),
    ok.
