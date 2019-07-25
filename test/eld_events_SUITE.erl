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
    add_flag_eval_events_flush_with_track/1,
    add_flag_eval_events_flush_no_track/1,
    add_flag_eval_events_with_debug/1,
    add_identify_events/1,
    auto_flush/1,
    exceed_capacity/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        add_flag_eval_events_flush_with_track,
        add_flag_eval_events_flush_no_track,
        add_flag_eval_events_with_debug,
        add_identify_events,
        auto_flush,
        exceed_capacity
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(eld),
    eld:start_instance("", #{start_stream => false, events_dispatcher => eld_event_dispatch_test}),
    Another1Options = #{
        start_stream => false,
        events_capacity => 2,
        events_dispatcher => eld_event_dispatch_test,
        events_flush_interval => 1000
    },
    eld:start_instance("", another1, Another1Options),
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

get_simple_flag_track() ->
    {
        <<"abc">>,
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

get_simple_flag_no_track() ->
    {
        <<"abc">>,
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
            <<"trackEvents">> => false,
            <<"variations">> => [true,false],
            <<"version">> => 5
        }
    }.

get_simple_flag_debug() ->
    % Now + 30 secs
    DebugDate = erlang:system_time(milli_seconds) + 30000,
    {
        <<"abc">>,
        #{
            <<"clientSide">> => false,
            <<"debugEventsUntilDate">> => DebugDate,
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
            <<"trackEvents">> => false,
            <<"variations">> => [true,false],
            <<"version">> => 5
        }
    }.

send_await_events(Events, Flush) ->
    send_await_events(Events, Flush, default).

send_await_events(Events, Flush, Tag) ->
    TestPid = self(),
    ErPid = spawn(fun() -> receive ErEvents -> TestPid ! ErEvents end end),
    true = register(eld_test_events, ErPid),
    [ok = eld_event_server:add_event(Tag, E) || E <- Events],
    ok = if Flush -> eld_event_server:flush(Tag); true -> ok end,
    ActualEventsBin = receive
        EventsReceived ->
            EventsReceived
        after 2000 ->
            error
    end,
    true = is_binary(ActualEventsBin),
    ActualEvents = jsx:decode(ActualEventsBin, [return_maps]),
    true = is_list(ActualEvents),
    ActualEvents.

%%====================================================================
%% Tests
%%====================================================================

add_flag_eval_events_flush_with_track(_) ->
    {FlagKey, FlagBin} = get_simple_flag_track(),
    Flag = eld_flag:new(FlagKey, FlagBin),
    Event = eld_event:new_flag_eval(
        5,
        <<"variation-value-5">>,
        <<"default-value">>,
        #{key => <<"12345-track">>},
        target_match,
        Flag
    ),
    Events = [Event],
    ActualEvents = send_await_events(Events, true),
    [
        #{
            <<"features">> := #{
                <<"abc">> := #{
                    <<"counters">> := [
                        #{
                            <<"count">> := 1,
                            <<"unknown">> := false,
                            <<"value">> := <<"variation-value-5">>,
                            <<"variation">> := 5,
                            <<"version">> := 5
                        }
                    ],
                    <<"default">> := <<"default-value">>
                }
            },
            <<"kind">> := <<"summary">>,
            <<"startDate">> := _,
            <<"endDate">> := _
        },
        #{
            <<"kind">> := <<"index">>,
            <<"user">> := #{<<"key">> := <<"12345-track">>},
            <<"creationDate">> := _
        },
        #{
            <<"kind">> := <<"feature">>,
            <<"key">> := <<"abc">>,
            <<"default">> := <<"default-value">>,
            <<"reason">> := #{<<"kind">> := <<"TARGET_MATCH">>},
            <<"userKey">> := <<"12345-track">>,
            <<"value">> := <<"variation-value-5">>,
            <<"variation">> := 5,
            <<"version">> := 5,
            <<"creationDate">> := _
        }
    ] = ActualEvents.

add_flag_eval_events_flush_no_track(_) ->
    {FlagKey, FlagBin} = get_simple_flag_no_track(),
    Flag = eld_flag:new(FlagKey, FlagBin),
    Event = eld_event:new_flag_eval(
        5,
        <<"variation-value-5">>,
        <<"default-value">>,
        #{key => <<"12345-no-track">>},
        target_match,
        Flag
    ),
    Events = [Event],
    ActualEvents = send_await_events(Events, true),
    [
        #{
            <<"features">> := #{
                <<"abc">> := #{
                    <<"counters">> := [
                        #{
                            <<"count">> := 1,
                            <<"unknown">> := false,
                            <<"value">> := <<"variation-value-5">>,
                            <<"variation">> := 5,
                            <<"version">> := 5
                        }
                    ],
                    <<"default">> := <<"default-value">>
                }
            },
            <<"kind">> := <<"summary">>,
            <<"startDate">> := _,
            <<"endDate">> := _
        },
        #{
            <<"kind">> := <<"index">>,
            <<"user">> := #{<<"key">> := <<"12345-no-track">>},
            <<"creationDate">> := _
        }
    ] = ActualEvents,
    % Evaluate flag for same user
    Event2 = eld_event:new_flag_eval(
        5,
        <<"variation-value-5">>,
        <<"default-value">>,
        #{key => <<"12345-no-track">>},
        target_match,
        Flag
    ),
    Events2 = [Event2],
    ActualEvents2 = send_await_events(Events2, true),
    % No index event this time due to users LRU cache
    [
        #{
            <<"features">> := #{
                <<"abc">> := #{
                    <<"counters">> := [
                        #{
                            <<"count">> := 1,
                            <<"unknown">> := false,
                            <<"value">> := <<"variation-value-5">>,
                            <<"variation">> := 5,
                            <<"version">> := 5
                        }
                    ],
                    <<"default">> := <<"default-value">>
                }
            },
            <<"kind">> := <<"summary">>,
            <<"startDate">> := _,
            <<"endDate">> := _
        }
    ] = ActualEvents2.

add_flag_eval_events_with_debug(_) ->
    {FlagKey, FlagBin} = get_simple_flag_debug(),
    Flag = eld_flag:new(FlagKey, FlagBin),
    Event = eld_event:new_flag_eval(
        5,
        <<"variation-value-5">>,
        <<"default-value">>,
        #{key => <<"12345-debug">>},
        target_match,
        Flag
    ),
    Events = [Event],
    ActualEvents = send_await_events(Events, true),
    [
        #{
            <<"features">> := #{
                <<"abc">> := #{
                    <<"counters">> := [
                        #{
                            <<"count">> := 1,
                            <<"unknown">> := false,
                            <<"value">> := <<"variation-value-5">>,
                            <<"variation">> := 5,
                            <<"version">> := 5
                        }
                    ],
                    <<"default">> := <<"default-value">>
                }
            },
            <<"kind">> := <<"summary">>,
            <<"startDate">> := _,
            <<"endDate">> := _
        },
        #{
            <<"kind">> := <<"index">>,
            <<"user">> := #{<<"key">> := <<"12345-debug">>},
            <<"creationDate">> := _
        },
        #{
            <<"kind">> := <<"debug">>,
            <<"key">> := <<"abc">>,
            <<"default">> := <<"default-value">>,
            <<"reason">> := #{<<"kind">> := <<"TARGET_MATCH">>},
            <<"user">> := #{<<"key">> := <<"12345-debug">>},
            <<"value">> := <<"variation-value-5">>,
            <<"variation">> := 5,
            <<"version">> := 5,
            <<"creationDate">> := _
        }
    ] = ActualEvents.

add_identify_events(_) ->
    Event1 = eld_event:new_identify(#{key => <<"12345">>}),
    Event2 = eld_event:new_identify(#{key => <<"abcde">>}),
    Events = [Event1, Event2],
    ActualEvents = send_await_events(Events, true),
    [
        #{
            <<"key">> := <<"12345">>,
            <<"kind">> := <<"identify">>,
            <<"user">> := #{<<"key">> := <<"12345">>},
            <<"creationDate">> := _
        },
        #{
            <<"key">> := <<"abcde">>,
            <<"kind">> := <<"identify">>,
            <<"user">> := #{<<"key">> := <<"abcde">>},
            <<"creationDate">> := _
        }
    ] = ActualEvents.

auto_flush(_) ->
    Event1 = eld_event:new_identify(#{key => <<"12345">>}),
    Event2 = eld_event:new_identify(#{key => <<"abcde">>}),
    Events = [Event1, Event2],
    ActualEvents = send_await_events(Events, false, another1),
    [
        #{
            <<"key">> := <<"12345">>,
            <<"kind">> := <<"identify">>,
            <<"user">> := #{<<"key">> := <<"12345">>},
            <<"creationDate">> := _
        },
        #{
            <<"key">> := <<"abcde">>,
            <<"kind">> := <<"identify">>,
            <<"user">> := #{<<"key">> := <<"abcde">>},
            <<"creationDate">> := _
        }
    ] = ActualEvents.

exceed_capacity(_) ->
    Event1 = eld_event:new_identify(#{key => <<"foo">>}),
    Event2 = eld_event:new_identify(#{key => <<"bar">>}),
    Event3 = eld_event:new_identify(#{key => <<"baz">>}),
    Events = [Event1, Event2, Event3],
    ActualEvents = send_await_events(Events, false, another1),
    [
        #{
            <<"key">> := <<"foo">>,
            <<"kind">> := <<"identify">>,
            <<"user">> := #{<<"key">> := <<"foo">>},
            <<"creationDate">> := _
        },
        #{
            <<"key">> := <<"bar">>,
            <<"kind">> := <<"identify">>,
            <<"user">> := #{<<"key">> := <<"bar">>},
            <<"creationDate">> := _
        }
    ] = ActualEvents.
