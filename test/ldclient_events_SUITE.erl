-module(ldclient_events_SUITE).

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
    add_flag_eval_events_flush_with_track_no_reasons/1,
    add_flag_eval_events_flush_with_track_experimentation_rule/1,
    add_flag_eval_events_flush_with_track_experimentation_fallthrough/1,
    add_flag_eval_events_flush_no_track/1,
    add_flag_eval_events_with_debug/1,
    add_flag_eval_events_with_debug_track_events/1,
    add_identify_events/1,
    add_custom_events/1,
    auto_flush/1,
    exceed_capacity/1,
    fail_and_retry/1,
    payload_id_differs/1,
    add_flag_eval_events_flush_in_experiment_fallthrough/1,
    add_flag_eval_events_flush_in_experiment_rule_match/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        add_flag_eval_events_flush_with_track,
        add_flag_eval_events_flush_with_track_no_reasons,
        add_flag_eval_events_flush_with_track_experimentation_rule,
        add_flag_eval_events_flush_with_track_experimentation_fallthrough,
        add_flag_eval_events_flush_no_track,
        add_flag_eval_events_with_debug,
        add_identify_events,
        add_custom_events,
        auto_flush,
        exceed_capacity,
        fail_and_retry,
        payload_id_differs,
        add_flag_eval_events_flush_in_experiment_fallthrough,
        add_flag_eval_events_flush_in_experiment_rule_match
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ldclient),
    Options = #{
        stream => false,
        events_dispatcher => ldclient_event_dispatch_test,
        polling_update_requestor => ldclient_update_requestor_test
    },
    ldclient:start_instance("", Options),
    Another1Options = Options#{
        events_capacity => 2,
        events_flush_interval => 1000
    },
    ldclient:start_instance("", another1, Another1Options),
    ldclient:start_instance("sdk-key-events-fail", failer, Another1Options),
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

get_simple_flag_track() ->
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
        <<"trackEventsFallthrough">> => false,
        <<"variations">> => [true,false],
        <<"version">> => 5
    }.

get_simple_flag_no_track() ->
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
        <<"trackEventsFallthrough">> => false,
        <<"variations">> => [true,false],
        <<"version">> => 5
    }.

get_simple_flag_experimentation_rule() ->
    #{
        <<"clientSide">> => false,
        <<"debugEventsUntilDate">> => null,
        <<"deleted">> => false,
        <<"fallthrough">> => #{<<"variation">> => 0},
        <<"key">> => <<"abc">>,
        <<"offVariation">> => 1,
        <<"on">> => true,
        <<"prerequisites">> => [],
        <<"rules">> => [#{
            <<"clauses">> => [],
            <<"id">> => <<"12345">>,
            <<"variation">> => 0,
            <<"trackEvents">> => true
        }],
        <<"salt">> => <<"d0888ec5921e45c7af5bc10b47b033ba">>,
        <<"sel">> => <<"8b4d79c59adb4df492ebea0bf65dfd4c">>,
        <<"targets">> => [],
        <<"trackEvents">> => false,
        <<"trackEventsFallthrough">> => false,
        <<"variations">> => [true,false],
        <<"version">> => 5
    }.

get_simple_flag_experimentation_fallthrough() ->
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
        <<"trackEventsFallthrough">> => true,
        <<"variations">> => [true,false],
        <<"version">> => 5
    }.

get_simple_flag_experiment_allocation_v2_fallthrough() ->
    #{
        <<"clientSide">> => false,
        <<"debugEventsUntilDate">> => null,
        <<"deleted">> => false,
        <<"fallthrough">> => #{<<"rollout">> => #{
            <<"kind">> => <<"experiment">>,
            <<"seed">>  => 61,
            <<"variations">> => [
                #{
                    <<"variation">> => 0,
                    <<"weight">> => 10000
                }
            ]
        }},
        <<"key">> => <<"abc">>,
        <<"offVariation">> => 1,
        <<"on">> => true,
        <<"prerequisites">> => [],
        <<"rules">> => [],
        <<"salt">> => <<"d0888ec5921e45c7af5bc10b47b033ba">>,
        <<"sel">> => <<"8b4d79c59adb4df492ebea0bf65dfd4c">>,
        <<"targets">> => [],
        <<"trackEvents">> => false,
        <<"trackEventsFallthrough">> => false,
        <<"variations">> => [true,false],
        <<"version">> => 5
    }.

get_simple_flag_experiment_allocation_v2_rule() ->
    #{
        <<"clientSide">> => false,
        <<"debugEventsUntilDate">> => null,
        <<"deleted">> => false,
        <<"fallthrough">> => #{<<"variation">> => 0},
        <<"key">> => <<"abc">>,
        <<"offVariation">> => 1,
        <<"on">> => true,
        <<"prerequisites">> => [],
        <<"rules">> => [#{
            <<"clauses">> => [],
            <<"id">> => <<"12345">>,
            <<"variation">> => 0,
            <<"trackEvents">> => false,
            <<"rollout">> => #{
                <<"kind">> => <<"experiment">>,
                <<"seed">>  => 61,
                <<"variations">> => [
                    #{
                        <<"variation">> => 0,
                        <<"weight">> => 10000
                    }
                ]
            }
        }],
        <<"salt">> => <<"d0888ec5921e45c7af5bc10b47b033ba">>,
        <<"sel">> => <<"8b4d79c59adb4df492ebea0bf65dfd4c">>,
        <<"targets">> => [],
        <<"trackEvents">> => false,
        <<"trackEventsFallthrough">> => true,
        <<"variations">> => [true,false],
        <<"version">> => 5
    }.

get_simple_flag_debug(TrackEvents) ->
    % Now + 30 secs
    DebugDate = erlang:system_time(milli_seconds) + 30000,
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
        <<"trackEvents">> => TrackEvents,
        <<"trackEventsFallthrough">> => false,
        <<"variations">> => [true,false],
        <<"version">> => 5
    }.

send_await_events(Events, Options) ->
    send_await_events(Events, Options, default).

send_await_events(Events, Options, Tag) ->
    register_event_forwarding_process(),
    IncludeReasons = maps:get(include_reasons, Options, false),
    Flush = maps:get(flush, Options, false),
    [ok = ldclient_event_server:add_event(Tag, E, #{include_reasons => IncludeReasons}) || E <- Events],
    ok = if Flush -> ldclient_event_server:flush(Tag); true -> ok end,
    receive_events().

await_events() ->
    register_event_forwarding_process(),
    receive_events().

receive_events() ->
    {Ok, ActualEventsBin, PayloadId} = receive
        {EventsReceived, PayloadIdReceived} ->
            {ok, EventsReceived, PayloadIdReceived}
        after 2000 ->
            {error, <<>>, <<>>}
    end,
    ok = Ok,
    true = is_binary(ActualEventsBin),
    true = is_binary(PayloadId),
    true = uuid:is_v4(PayloadId),
    ActualEvents = jsx:decode(ActualEventsBin, [return_maps]),
    true = is_list(ActualEvents),
    {ActualEvents, PayloadId}.

register_event_forwarding_process() ->
    TestPid = self(),
    ErPid = spawn(fun() -> receive ErEvents -> TestPid ! ErEvents end end),
    true = register(ldclient_test_events, ErPid).


%%====================================================================
%% Tests
%%====================================================================

add_flag_eval_events_flush_with_track(_) ->
    FlagBin = get_simple_flag_track(),
    Flag = ldclient_flag:new(FlagBin),
    Event = ldclient_event:new_flag_eval(
        5,
        <<"variation-value-5">>,
        <<"default-value">>,
        #{key => <<"12345-track">>, first_name => <<"Tester">>, last_name => <<"Testerson">>},
        target_match,
        Flag
    ),
    Events = [Event],
    {ActualEvents, _} = send_await_events(Events, #{flush => true, include_reasons => true}),
    [
        #{
            <<"features">> := #{
                <<"abc">> := #{
                    <<"counters">> := [
                        #{
                            <<"count">> := 1,
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
            <<"user">> := #{<<"key">> := <<"12345-track">>, <<"firstName">> := <<"Tester">>, <<"lastName">> := <<"Testerson">>},
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

add_flag_eval_events_flush_with_track_no_reasons(_) ->
    FlagBin = get_simple_flag_track(),
    Flag = ldclient_flag:new(FlagBin),
    Event = ldclient_event:new_flag_eval(
        5,
        <<"variation-value-5">>,
        <<"default-value">>,
        #{key => <<"12345-track-no-reasons">>, first_name => <<"Tester">>, last_name => <<"Testerson">>},
        target_match,
        Flag
    ),
    Events = [Event],
    {ActualEvents, _} = send_await_events(Events, #{flush => true, include_reasons => false}),
    [
        #{
            <<"features">> := #{
                <<"abc">> := #{
                    <<"counters">> := [
                        #{
                            <<"count">> := 1,
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
            <<"user">> := #{<<"key">> := <<"12345-track-no-reasons">>, <<"firstName">> := <<"Tester">>, <<"lastName">> := <<"Testerson">>},
            <<"creationDate">> := _
        },
        #{
            <<"kind">> := <<"feature">>,
            <<"key">> := <<"abc">>,
            <<"default">> := <<"default-value">>,
            <<"userKey">> := <<"12345-track-no-reasons">>,
            <<"value">> := <<"variation-value-5">>,
            <<"variation">> := 5,
            <<"version">> := 5,
            <<"creationDate">> := _
        }
    ] = ActualEvents.

add_flag_eval_events_flush_with_track_experimentation_rule(_) ->
    FlagBin = get_simple_flag_experimentation_rule(),
    Flag = ldclient_flag:new(FlagBin),
    Event = ldclient_event:new_flag_eval(
        5,
        <<"variation-value-5">>,
        <<"default-value">>,
        #{key => <<"12345-track-experimentation-rule">>, first_name => <<"Tester">>, last_name => <<"Testerson">>},
        {rule_match, 0, <<"12345">>},
        Flag
    ),
    Events = [Event],
    {ActualEvents, _} = send_await_events(Events, #{flush => true, include_reasons => false}),
    [
        #{
            <<"features">> := #{
                <<"abc">> := #{
                    <<"counters">> := [
                        #{
                            <<"count">> := 1,
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
            <<"user">> := #{<<"key">> := <<"12345-track-experimentation-rule">>, <<"firstName">> := <<"Tester">>, <<"lastName">> := <<"Testerson">>},
            <<"creationDate">> := _
        },
        #{
            <<"kind">> := <<"feature">>,
            <<"key">> := <<"abc">>,
            <<"default">> := <<"default-value">>,
            <<"reason">> := #{<<"kind">> := <<"RULE_MATCH">>, <<"ruleId">> := <<"12345">>, <<"ruleIndex">> := 0},
            <<"userKey">> := <<"12345-track-experimentation-rule">>,
            <<"value">> := <<"variation-value-5">>,
            <<"variation">> := 5,
            <<"version">> := 5,
            <<"creationDate">> := _
        }
    ] = ActualEvents.

add_flag_eval_events_flush_with_track_experimentation_fallthrough(_) ->
    FlagBin = get_simple_flag_experimentation_fallthrough(),
    Flag = ldclient_flag:new(FlagBin),
    Event = ldclient_event:new_flag_eval(
        5,
        <<"variation-value-5">>,
        <<"default-value">>,
        #{key => <<"12345-track-experimentation-fallthrough">>, first_name => <<"Tester">>, last_name => <<"Testerson">>},
        fallthrough,
        Flag
    ),
    Events = [Event],
    {ActualEvents, _} = send_await_events(Events, #{flush => true, include_reasons => false}),
    [
        #{
            <<"features">> := #{
                <<"abc">> := #{
                    <<"counters">> := [
                        #{
                            <<"count">> := 1,
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
            <<"user">> := #{<<"key">> := <<"12345-track-experimentation-fallthrough">>, <<"firstName">> := <<"Tester">>, <<"lastName">> := <<"Testerson">>},
            <<"creationDate">> := _
        },
        #{
            <<"kind">> := <<"feature">>,
            <<"key">> := <<"abc">>,
            <<"default">> := <<"default-value">>,
            <<"reason">> := #{<<"kind">> := <<"FALLTHROUGH">>},
            <<"userKey">> := <<"12345-track-experimentation-fallthrough">>,
            <<"value">> := <<"variation-value-5">>,
            <<"variation">> := 5,
            <<"version">> := 5,
            <<"creationDate">> := _
        }
    ] = ActualEvents.

add_flag_eval_events_flush_no_track(_) ->
    FlagBin = get_simple_flag_no_track(),
    Flag = ldclient_flag:new(FlagBin),
    Event = ldclient_event:new_flag_eval(
        5,
        <<"variation-value-5">>,
        <<"default-value">>,
        #{key => <<"12345-no-track">>, first_name => <<"Tester">>, last_name => <<"Testerson">>},
        target_match,
        Flag
    ),
    Events = [Event],
    {ActualEvents, _} = send_await_events(Events, #{flush => true, include_reasons => true}),
    [
        #{
            <<"features">> := #{
                <<"abc">> := #{
                    <<"counters">> := [
                        #{
                            <<"count">> := 1,
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
            <<"user">> := #{<<"key">> := <<"12345-no-track">>, <<"firstName">> := <<"Tester">>, <<"lastName">> := <<"Testerson">>},
            <<"creationDate">> := _
        }
    ] = ActualEvents,
    % Evaluate flag for same user
    Event2 = ldclient_event:new_flag_eval(
        5,
        <<"variation-value-5">>,
        <<"default-value">>,
        #{key => <<"12345-no-track">>, first_name => <<"Tester">>, last_name => <<"Testerson">>},
        target_match,
        Flag
    ),
    Events2 = [Event2],
    {ActualEvents2, _} = send_await_events(Events2, #{flush => true}),
    % No index event this time due to users LRU cache
    [
        #{
            <<"features">> := #{
                <<"abc">> := #{
                    <<"counters">> := [
                        #{
                            <<"count">> := 1,
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
    FlagBin = get_simple_flag_debug(false),
    Flag = ldclient_flag:new(FlagBin),
    Event = ldclient_event:new_flag_eval(
        5,
        <<"variation-value-5">>,
        <<"default-value">>,
        #{key => <<"12345-debug">>, first_name => <<"Tester">>, last_name => <<"Testerson">>},
        target_match,
        Flag
    ),
    Events = [Event],
    {ActualEvents, _} = send_await_events(Events, #{flush => true}),
    [
        #{
            <<"features">> := #{
                <<"abc">> := #{
                    <<"counters">> := [
                        #{
                            <<"count">> := 1,
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
            <<"user">> := #{<<"key">> := <<"12345-debug">>, <<"firstName">> := <<"Tester">>, <<"lastName">> := <<"Testerson">>},
            <<"creationDate">> := _
        },
        #{
            <<"kind">> := <<"debug">>,
            <<"key">> := <<"abc">>,
            <<"default">> := <<"default-value">>,
            % The reason should only be included with trackEvents (flag or rule), or if with_reason is specified.
            %<<"reason">> := #{<<"kind">> := <<"TARGET_MATCH">>},
            <<"user">> := #{<<"key">> := <<"12345-debug">>},
            <<"value">> := <<"variation-value-5">>,
            <<"variation">> := 5,
            <<"version">> := 5,
            <<"creationDate">> := _
        }
    ] = ActualEvents.

add_flag_eval_events_with_debug_track_events(_) ->
    FlagBin = get_simple_flag_debug(true),
    Flag = ldclient_flag:new(FlagBin),
    Event = ldclient_event:new_flag_eval(
        5,
        <<"variation-value-5">>,
        <<"default-value">>,
        #{key => <<"12345-debug">>, first_name => <<"Tester">>, last_name => <<"Testerson">>},
        target_match,
        Flag
    ),
    Events = [Event],
    {ActualEvents, _} = send_await_events(Events, #{flush => true}),
    [
        #{
            <<"features">> := #{
                <<"abc">> := #{
                    <<"counters">> := [
                        #{
                            <<"count">> := 1,
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
            <<"user">> := #{<<"key">> := <<"12345-debug">>, <<"firstName">> := <<"Tester">>, <<"lastName">> := <<"Testerson">>},
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
    Event1 = ldclient_event:new_identify(#{key => <<"12345">>, first_name => <<"Tester">>, last_name => <<"Testerson">>}),
    Event2 = ldclient_event:new_identify(#{key => <<"abcde">>, first_name => <<"Tester">>, last_name => <<"Testerson">>}),
    Events = [Event1, Event2],
    {ActualEvents, _} = send_await_events(Events, #{flush => true}),
    [
        #{
            <<"key">> := <<"12345">>,
            <<"kind">> := <<"identify">>,
            <<"user">> := #{<<"key">> := <<"12345">>, <<"firstName">> := <<"Tester">>, <<"lastName">> := <<"Testerson">>},
            <<"creationDate">> := _
        },
        #{
            <<"key">> := <<"abcde">>,
            <<"kind">> := <<"identify">>,
            <<"user">> := #{<<"key">> := <<"abcde">>, <<"firstName">> := <<"Tester">>, <<"lastName">> := <<"Testerson">>},
            <<"creationDate">> := _
        }
    ] = ActualEvents.

add_custom_events(_) ->
    Event1 = ldclient_event:new_custom(<<"event-foo">>, #{key => <<"123456">>, first_name => <<"Tester">>, last_name => <<"Testerson">>}, #{k1 => <<"v1">>}),
    Event2 = ldclient_event:new_custom(<<"event-bar">>, #{key => <<"abcdef">>, first_name => <<"Tester">>, last_name => <<"Testerson">>}, #{k2 => <<"v2">>}),
    Event3 = ldclient_event:new_custom(<<"event-baz">>, #{key => <<"987656">>, first_name => <<"Tester">>, last_name => <<"Testerson">>}, #{k3 => <<"v3">>}, 123),
    Events = [Event1, Event2, Event3],
    {ActualEvents, _} = send_await_events(Events, #{flush => true}),
    [
        #{
            <<"kind">> := <<"index">>,
            <<"user">> := #{<<"key">> := <<"123456">>, <<"firstName">> := <<"Tester">>, <<"lastName">> := <<"Testerson">>},
            <<"creationDate">> := _
        },
        #{
            <<"key">> := <<"event-foo">>,
            <<"kind">> := <<"custom">>,
            <<"userKey">> := <<"123456">>,
            <<"data">> := #{<<"k1">> := <<"v1">>},
            <<"creationDate">> := _
        },
        #{
            <<"kind">> := <<"index">>,
            <<"user">> := #{<<"key">> := <<"abcdef">>, <<"firstName">> := <<"Tester">>, <<"lastName">> := <<"Testerson">>},
            <<"creationDate">> := _
        },
        #{
            <<"key">> := <<"event-bar">>,
            <<"kind">> := <<"custom">>,
            <<"userKey">> := <<"abcdef">>,
            <<"data">> := #{<<"k2">> := <<"v2">>},
            <<"creationDate">> := _
        },
        #{
            <<"kind">> := <<"index">>,
            <<"user">> := #{<<"key">> := <<"987656">>, <<"firstName">> := <<"Tester">>, <<"lastName">> := <<"Testerson">>},
            <<"creationDate">> := _
        },
        #{
            <<"key">> := <<"event-baz">>,
            <<"kind">> := <<"custom">>,
            <<"userKey">> := <<"987656">>,
            <<"data">> := #{<<"k3">> := <<"v3">>},
            <<"metricValue">> := 123,
            <<"creationDate">> := _
        }
    ] = ActualEvents.

auto_flush(_) ->
    Event1 = ldclient_event:new_identify(#{key => <<"12345">>, first_name => <<"Tester">>, last_name => <<"Testerson">>}),
    Event2 = ldclient_event:new_identify(#{key => <<"abcde">>, first_name => <<"Tester">>, last_name => <<"Testerson">>}),
    Events = [Event1, Event2],
    {ActualEvents, _} = send_await_events(Events, #{flush => false}, another1),
    [
        #{
            <<"key">> := <<"12345">>,
            <<"kind">> := <<"identify">>,
            <<"user">> := #{<<"key">> := <<"12345">>, <<"firstName">> := <<"Tester">>, <<"lastName">> := <<"Testerson">>},
            <<"creationDate">> := _
        },
        #{
            <<"key">> := <<"abcde">>,
            <<"kind">> := <<"identify">>,
            <<"user">> := #{<<"key">> := <<"abcde">>, <<"firstName">> := <<"Tester">>, <<"lastName">> := <<"Testerson">>},
            <<"creationDate">> := _
        }
    ] = ActualEvents.

exceed_capacity(_) ->
    Event1 = ldclient_event:new_identify(#{key => <<"foo">>, first_name => <<"Tester">>, last_name => <<"Testerson">>}),
    Event2 = ldclient_event:new_identify(#{key => <<"bar">>, first_name => <<"Tester">>, last_name => <<"Testerson">>}),
    Event3 = ldclient_event:new_identify(#{key => <<"baz">>, first_name => <<"Tester">>, last_name => <<"Testerson">>}),
    Events = [Event1, Event2, Event3],
    {ActualEvents, _} = send_await_events(Events, #{flush => true}, another1),
    [
        #{
            <<"key">> := <<"foo">>,
            <<"kind">> := <<"identify">>,
            <<"user">> := #{<<"key">> := <<"foo">>, <<"firstName">> := <<"Tester">>, <<"lastName">> := <<"Testerson">>},
            <<"creationDate">> := _
        },
        #{
            <<"key">> := <<"bar">>,
            <<"kind">> := <<"identify">>,
            <<"user">> := #{<<"key">> := <<"bar">>, <<"firstName">> := <<"Tester">>, <<"lastName">> := <<"Testerson">>},
            <<"creationDate">> := _
        }
    ] = ActualEvents.

fail_and_retry(_) ->
    Event1 = ldclient_event:new_identify(#{key => <<"foo">>, first_name => <<"Tester">>, last_name => <<"Testerson">>}),
    Event2 = ldclient_event:new_identify(#{key => <<"bar">>, first_name => <<"Tester">>, last_name => <<"Testerson">>}),
    Events = [Event1, Event2],
    % Await and match events and payload ID (first try)
    {ActualEvents, PayloadId} = send_await_events(Events, #{flush => true}, failer),
    [
        #{
            <<"key">> := <<"foo">>,
            <<"kind">> := <<"identify">>,
            <<"user">> := #{<<"key">> := <<"foo">>, <<"firstName">> := <<"Tester">>, <<"lastName">> := <<"Testerson">>},
            <<"creationDate">> := _
        },
        #{
            <<"key">> := <<"bar">>,
            <<"kind">> := <<"identify">>,
            <<"user">> := #{<<"key">> := <<"bar">>, <<"firstName">> := <<"Tester">>, <<"lastName">> := <<"Testerson">>},
            <<"creationDate">> := _
        }
    ] = ActualEvents,
    % Await and match same events and payload ID the 2nd time (on retry)
    {ActualEvents, PayloadId} = await_events().

payload_id_differs(_) ->
    Event1 = ldclient_event:new_identify(#{key => <<"foo">>, first_name => <<"Tester">>, last_name => <<"Testerson">>}),
    % Await and match events and payload ID (first try)
    {ActualEvents1, PayloadId1} = send_await_events([Event1], #{flush => true}),
    [
        #{
            <<"key">> := <<"foo">>,
            <<"kind">> := <<"identify">>,
            <<"user">> := #{<<"key">> := <<"foo">>, <<"firstName">> := <<"Tester">>, <<"lastName">> := <<"Testerson">>},
            <<"creationDate">> := _
        }
    ] = ActualEvents1,
    Event2 = ldclient_event:new_identify(#{key => <<"bar">>, first_name => <<"Tester">>, last_name => <<"Testerson">>}),
    {ActualEvents2, PayloadId2} = send_await_events([Event2], #{flush => true}),
    [
        #{
            <<"key">> := <<"bar">>,
            <<"kind">> := <<"identify">>,
            <<"user">> := #{<<"key">> := <<"bar">>, <<"firstName">> := <<"Tester">>, <<"lastName">> := <<"Testerson">>},
            <<"creationDate">> := _
        }
    ] = ActualEvents2,
    false = PayloadId1 =:= PayloadId2.

add_flag_eval_events_flush_in_experiment_fallthrough(_) ->
    FlagBin = get_simple_flag_experiment_allocation_v2_fallthrough(),
    Flag = ldclient_flag:new(FlagBin),
    Event = ldclient_event:new_flag_eval(
        0,
        <<"a">>,
        <<"default-value">>,
        #{key => <<"experiment-fallthrough">>},
        {fallthrough, in_experiment},
        Flag
    ),
    Events = [Event],
    {ActualEvents, _} = send_await_events(Events, #{flush => true, include_reasons => true}),
    [
        #{
            <<"features">> := #{
                <<"abc">> := #{
                    <<"counters">> := [
                        #{
                            <<"count">> := 1,
                            <<"value">> := <<"a">>,
                            <<"variation">> := 0,
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
            <<"user">> := #{<<"key">> := <<"experiment-fallthrough">>},
            <<"creationDate">> := _
        },
        #{
            <<"kind">> := <<"feature">>,
            <<"key">> := <<"abc">>,
            <<"default">> := <<"default-value">>,
            <<"reason">> := #{<<"kind">> := <<"FALLTHROUGH">>, <<"inExperiment">> := true},
            <<"userKey">> := <<"experiment-fallthrough">>,
            <<"value">> := <<"a">>,
            <<"variation">> := 0,
            <<"version">> := 5,
            <<"creationDate">> := _
        }
    ] = ActualEvents.

add_flag_eval_events_flush_in_experiment_rule_match(_) ->
    FlagBin = get_simple_flag_experiment_allocation_v2_rule(),
    Flag = ldclient_flag:new(FlagBin),
    Event = ldclient_event:new_flag_eval(
        0,
        <<"a">>,
        <<"default-value">>,
        #{key => <<"experiment-rule">>},
        {rule_match, 0, <<"id">>, in_experiment},
        Flag
    ),
    Events = [Event],
    {ActualEvents, _} = send_await_events(Events, #{flush => true, include_reasons => true}),
    [
        #{
            <<"features">> := #{
                <<"abc">> := #{
                    <<"counters">> := [
                        #{
                            <<"count">> := 1,
                            <<"value">> := <<"a">>,
                            <<"variation">> := 0,
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
            <<"user">> := #{<<"key">> := <<"experiment-rule">>},
            <<"creationDate">> := _
        },
        #{
            <<"kind">> := <<"feature">>,
            <<"key">> := <<"abc">>,
            <<"default">> := <<"default-value">>,
            <<"reason">> := #{<<"kind">> := <<"RULE_MATCH">>, <<"ruleIndex">> := 0, <<"ruleId">> := <<"id">>, <<"inExperiment">> := true},
            <<"userKey">> := <<"experiment-rule">>,
            <<"value">> := <<"a">>,
            <<"variation">> := 0,
            <<"version">> := 5,
            <<"creationDate">> := _
        }
    ] = ActualEvents.
