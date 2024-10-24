-module(ldclient_parse_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    parse_flag_empty/1,
    parse_flag_key_only/1,
    parse_flag_bare/1,
    parse_flag_full/1,
    parse_flag_ignore_invalid/1,
    parse_flag_invalid_fallthrough/1,
    parse_segment_empty/1,
    parse_segment_key_only/1,
    parse_segment_full/1,
    parse_flag_rollout_kind/1,
    parse_flag_invalid_kind/1,
    parse_flag_experiment_kind/1,
    parse_client_side_availability_both_false/1,
    parse_client_side_availability_using_environment_id/1,
    parse_client_side_availability_using_mobile_key/1,
    parse_client_side_availability_both_true/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        parse_flag_empty,
        parse_flag_key_only,
        parse_flag_bare,
        parse_flag_full,
        parse_flag_ignore_invalid,
        parse_flag_invalid_fallthrough,
        parse_segment_empty,
        parse_segment_key_only,
        parse_segment_full,
        parse_flag_rollout_kind,
        parse_flag_invalid_kind,
        parse_flag_experiment_kind,
        parse_client_side_availability_both_false,
        parse_client_side_availability_using_environment_id,
        parse_client_side_availability_using_mobile_key,
        parse_client_side_availability_both_true
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%%====================================================================
%% Helpers
%%====================================================================

%%====================================================================
%% Tests
%%====================================================================

parse_flag_empty(_) ->
    FlagRaw = #{},
    FlagExpected = #{
        debugEventsUntilDate  => null,
        deleted                  => false,
        fallthrough              => #{
            bucketBy => #{
                binary => <<"key">>,
                components => [<<"key">>],
                valid => true
            },
            contextKind => <<"user">>,
            variations => [],
            seed => null,
            kind => rollout
        },
        key                      => <<>>,
        offVariation             => 0,
        on                       => false,
        prerequisites            => [],
        rules                    => [],
        salt                     => <<>>,
        targets                  => [],
        contextTargets           => [],
        trackEvents              => false,
        trackEventsFallthrough   => false,
        variations               => [],
        version                  => 0,
        clientSideAvailability   => #{
            usingEnvironmentId => false,
            usingMobileKey => false
        }
    },
    FlagExpected = ldclient_flag:new(FlagRaw).

parse_flag_key_only(_) ->
    FlagRaw = #{
        <<"key">> => <<"flag-key-only">>
    },
    FlagExpected = #{
        debugEventsUntilDate  => null,
        deleted                  => false,
        fallthrough              => #{
            bucketBy => #{
                binary => <<"key">>,
                components => [<<"key">>],
                valid => true
            },
            contextKind => <<"user">>,
            variations => [],
            seed => null,
            kind => rollout
        },
        key                      => <<"flag-key-only">>,
        offVariation            => 0,
        on                       => false,
        prerequisites            => [],
        rules                    => [],
        salt                     => <<>>,
        targets                  => [],
        contextTargets           => [],
        trackEvents             => false,
        trackEventsFallthrough => false,
        variations               => [],
        version                  => 0,
        clientSideAvailability   => #{
            usingEnvironmentId => false,
            usingMobileKey => false
        }
    },
    FlagExpected = ldclient_flag:new(FlagRaw).

parse_flag_bare(_) ->
    FlagRaw = #{
        <<"debugEventsUntilDate">> => 12345,
        <<"deleted">> => true,
        <<"fallthrough">> => #{<<"variation">> => 1},
        <<"key">> => <<"flag-bare">>,
        <<"offVariation">> => 1,
        <<"on">> => true,
        <<"prerequisites">> => [],
        <<"rules">> => [],
        <<"salt">> => <<>>,
        <<"targets">> => [],
        <<"trackEvents">> => true,
        <<"trackEventsFallthrough">> => true,
        <<"variations">> => [true, false],
        <<"version">> => 10
    },
    FlagExpected = #{
        debugEventsUntilDate  => 12345,
        deleted                  => true,
        fallthrough              => 1,
        key                      => <<"flag-bare">>,
        offVariation            => 1,
        on                       => true,
        prerequisites            => [],
        rules                    => [],
        salt                     => <<>>,
        targets                  => [],
        contextTargets           => [],
        trackEvents             => true,
        trackEventsFallthrough => true,
        variations               => [true, false],
        version                  => 10,
        clientSideAvailability   => #{
            usingEnvironmentId => false,
            usingMobileKey => false
        }
    },
    FlagExpected = ldclient_flag:new(FlagRaw).

parse_flag_full(_) ->
    FlagRaw = #{
        <<"debugEventsUntilDate">> => 1234567,
        <<"deleted">> => false,
        <<"fallthrough">> => #{
            <<"rollout">> => #{
                <<"kind">> => <<"experiment">>,
                <<"bucketBy">> => <<"foo">>,
                <<"variations">> => [
                    #{<<"variation">> => 0, <<"weight">> => 0, <<"untracked">> => true},
                    #{<<"variation">> => 1, <<"weight">> => 40000, <<"untracked">> => false},
                    #{<<"variation">> => 2, <<"weight">> => 60000}
                ]
            }
        },
        <<"key">> => <<"flag-full">>,
        <<"offVariation">> => 2,
        <<"on">> => true,
        <<"prerequisites">> => [
            #{<<"key">> => <<"flag-foo">>, <<"variation">> => 5},
            #{<<"key">> => <<"flag-bar">>, <<"variation">> => 3}
        ],
        <<"rules">> => [
            #{
                <<"id">> => <<"rule-foo">>,
                <<"trackEvents">> => true,
                <<"variation">> => 3,
                <<"clauses">> => [
                    #{
                        <<"attribute">> => <<"user-attr-foo">>,
                        <<"negate">> => false,
                        <<"op">> => <<"contains">>,
                        <<"values">> => [<<"rule-foo-value1">>, <<"rule-foo-value2">>]
                    }
                ]
            }
        ],
        <<"salt">> => <<"flag-full-salt">>,
        <<"targets">> => [
            #{<<"variation">> => 0, <<"values">> => [<<"user-target1">>, <<"user-target-2">>]},
            #{<<"variation">> => 1, <<"values">> => [<<"user-target3">>, <<"user-target-4">>]},
            #{<<"variation">> => 2, <<"values">> => []}
        ],
        <<"contextTargets">> => [
            #{<<"contextKind">> => <<"other">>, <<"variation">> => 3, <<"values">> => [<<"other-target1">>, <<"other-target-2">>]},
            #{<<"contextKind">> => <<"other">>, <<"variation">> => 4, <<"values">> => [<<"other-target3">>, <<"other-target-4">>]},
            #{<<"contextKind">> => <<"other">>, <<"variation">> => 5, <<"values">> => []}
        ],
        <<"trackEvents">> => true,
        <<"trackEventsFallthrough">> => true,
        <<"variations">> => [<<"A">>, <<"B">>, <<"C">>],
        <<"version">> => 9
    },
    FlagExpected = #{
        debugEventsUntilDate  => 1234567,
        deleted                  => false,
        fallthrough              => #{
            %% For an experiment we discard the specified bucketBy and use <<"key">>
            bucketBy => #{
                binary => <<"key">>,
                components => [<<"key">>],
                valid => true
            },
            contextKind => <<"user">>,
            variations => [
                #{variation => 0, weight => 0, untracked => true},
                #{variation => 1, weight => 40000, untracked => false},
                #{variation => 2, weight => 60000, untracked => false}
            ],
            kind => experiment,
            seed => null
        },
        key                      => <<"flag-full">>,
        offVariation            => 2,
        on                       => true,
        prerequisites            => [
            #{key => <<"flag-foo">>, variation => 5},
            #{key => <<"flag-bar">>, variation => 3}
        ],
        rules                    => [
            #{
                id => <<"rule-foo">>,
                trackEvents => true,
                variationOrRollout => 3,
                clauses => [
                    #{
                        attribute => #{
                            binary => <<"user-attr-foo">>,
                            components => [<<"user-attr-foo">>],
                            valid => true
                        },
                        context_kind => <<"user">>,
                        negate => false,
                        op => contains,
                        values => [<<"rule-foo-value1">>, <<"rule-foo-value2">>]
                    }
                ]
            }
        ],
        salt                     => <<"flag-full-salt">>,
        targets                  => [
            #{variation => 0, values => [<<"user-target1">>, <<"user-target-2">>], contextKind => <<"user">>},
            #{variation => 1, values => [<<"user-target3">>, <<"user-target-4">>], contextKind => <<"user">>},
            #{variation => 2, values => [], contextKind => <<"user">>}
        ],
        contextTargets           => [
            #{variation => 3, values => [<<"other-target1">>, <<"other-target-2">>], contextKind => <<"other">>},
            #{variation => 4, values => [<<"other-target3">>, <<"other-target-4">>], contextKind => <<"other">>},
            #{variation => 5, values => [], contextKind => <<"other">>}
        ],
        trackEvents             => true,
        trackEventsFallthrough => true,
        variations               => [<<"A">>, <<"B">>, <<"C">>],
        version                  => 9,
        clientSideAvailability   => #{
            usingEnvironmentId => false,
            usingMobileKey => false
        }
    },
    FlagExpected = ldclient_flag:new(FlagRaw).

parse_flag_ignore_invalid(_) ->
    FlagRaw = #{
        <<"fallthrough">> => #{
            <<"rollout">> => #{
                <<"bucketBy">> => <<"foo">>,
                <<"variations">> => [
                    #{<<"weight">> => 0},   % Invalid, no variation
                    #{<<"variation">> => 1} % Valid, weight defaults to 0
                ]
            }
        },
        <<"key">> => <<"flag-ignore-invalid">>,
        <<"offVariation">> => 2,
        <<"on">> => true,
        <<"prerequisites">> => [
            #{<<"variation">> => 5},       % Invalid, no key
            #{<<"key">> => <<"flag-bar">>} % Invalid, no variation
        ],
        <<"rules">> => [
            #{<<"id">> => <<"rule-foo">>, <<"trackEvents">> => true, <<"variation">> => 3} % Invalid, no clauses
        ],
        <<"targets">> => [
            #{<<"variation">> => 0},                                     % Invalid, no values
            #{<<"values">> => [<<"user-target3">>, <<"user-target-4">>]} % Invalid, no variation
        ],
        <<"variations">> => [<<"A">>, <<"B">>, <<"C">>]
    },
    FlagExpected = #{
        debugEventsUntilDate  => null,
        deleted                  => false,
        fallthrough              => #{
            kind => rollout,
            seed => null,
            bucketBy => #{
                binary => <<"foo">>,
                components => [<<"foo">>],
                valid => true
            },
            contextKind => <<"user">>,
            variations => [
                #{variation => 1, weight => 0, untracked => false}
            ]
        },
        key                      => <<"flag-ignore-invalid">>,
        offVariation            => 2,
        on                       => true,
        prerequisites            => [],
        rules                    => [],
        salt                     => <<>>,
        targets                  => [],
        contextTargets           => [],
        trackEvents             => false,
        trackEventsFallthrough => false,
        variations               => [<<"A">>, <<"B">>, <<"C">>],
        version                  => 0,
        clientSideAvailability   => #{
            usingEnvironmentId => false,
            usingMobileKey => false
        }
    },
    FlagExpected = ldclient_flag:new(FlagRaw).

parse_flag_invalid_fallthrough(_) ->
    FlagRaw = #{
        <<"key">> => <<"flag-invalid-fallthrough">>,
        <<"fallthrough">> => #{}
    },
    FlagExpected = #{
        debugEventsUntilDate  => null,
        deleted                  => false,
        fallthrough              => #{
            bucketBy => #{
                binary => <<"key">>,
                components => [<<"key">>],
                valid => true
            },
            contextKind => <<"user">>,
            variations => [],
            seed => null,
            kind => rollout
        },
        key                      => <<"flag-invalid-fallthrough">>,
        offVariation            => 0,
        on                       => false,
        prerequisites            => [],
        rules                    => [],
        salt                     => <<>>,
        targets                  => [],
        contextTargets           => [],
        trackEvents             => false,
        trackEventsFallthrough => false,
        variations               => [],
        version                  => 0,
        clientSideAvailability   => #{
            usingEnvironmentId => false,
            usingMobileKey => false
        }
    },
    FlagExpected = ldclient_flag:new(FlagRaw).

parse_segment_empty(_) ->
    SegmentRaw = #{},
    SegmentExpected = #{
        key      => <<>>,
        deleted  => false,
        excluded => [],
        included => [],
        rules    => [],
        salt     => <<>>,
        version  => 0,
        excludedContexts => [],
        includedContexts =>  []
    },
    SegmentExpected = ldclient_segment:new(SegmentRaw).

parse_segment_key_only(_) ->
    SegmentRaw = #{
        <<"key">> => <<"segment-key-only">>
    },
    SegmentExpected = #{
        key      => <<"segment-key-only">>,
        deleted  => false,
        excluded => [],
        included => [],
        rules    => [],
        salt     => <<>>,
        version  => 0,
        excludedContexts => [],
        includedContexts =>  []
    },
    SegmentExpected = ldclient_segment:new(SegmentRaw).

parse_segment_full(_) ->
    SegmentRaw = #{
        <<"key">> => <<"segment-full">>,
        <<"deleted">> => true,
        <<"excluded">> => [<<"123">>, <<"456">>],
        <<"included">> => [<<"789">>],
        <<"rules">> => [
            #{
                <<"id">> => <<"rule-foo">>,
                <<"trackEvents">> => true,
                <<"variation">> => 3,
                <<"clauses">> => [
                    #{
                        <<"attribute">> => <<"user-attr-foo">>,
                        <<"negate">> => false,
                        <<"op">> => <<"contains">>,
                        <<"values">> => [<<"rule-foo-value1">>, <<"rule-foo-value2">>]
                    }
                ]
            }
        ],
        <<"salt">> => <<"segment-full-salt">>,
        <<"version">> => 5,
        <<"includedContexts">> => [
            #{
                <<"contextKind">> => <<"org">>,
                <<"values">> => [<<"org-1">>, <<"org-2">>]
            },
            #{
                <<"contextKind">> => <<"potato">>,
                <<"values">> => [<<"potato-1">>, <<"potato-2">>]
            }
        ],
        <<"excludedContexts">> => [
            #{
                <<"contextKind">> => <<"user">>,
                <<"values">> => [<<"user-1">>, <<"user-2">>]
            }
        ]
    },
    SegmentExpected = #{
        key      => <<"segment-full">>,
        deleted  => true,
        excluded => [<<"123">>, <<"456">>],
        included => [<<"789">>],
        rules    => [
            #{
                bucketBy => #{
                    binary => <<"key">>,
                    components => [<<"key">>],
                    valid => true
                },
                rolloutContextKind => <<"user">>,
                weight => null,
                segmentKey => <<"segment-full">>,
                segmentSalt => <<"segment-full-salt">>,
                clauses => [
                    #{
                        attribute => #{
                            binary => <<"user-attr-foo">>,
                            components => [<<"user-attr-foo">>],
                            valid => true
                        },
                        context_kind => <<"user">>,
                        negate => false,
                        op => contains,
                        values => [<<"rule-foo-value1">>, <<"rule-foo-value2">>]
                    }
                ]
            }
        ],
        salt     => <<"segment-full-salt">>,
        version  => 5,
        excludedContexts => [
            #{
                contextKind => <<"user">>,
                values => [<<"user-1">>, <<"user-2">>]
            }
        ],
        includedContexts =>  [
            #{
                contextKind => <<"org">>,
                values => [<<"org-1">>, <<"org-2">>]
            },
            #{
                contextKind => <<"potato">>,
                values => [<<"potato-1">>, <<"potato-2">>]
            }
        ]
    },
    SegmentExpected = ldclient_segment:new(SegmentRaw).

parse_flag_invalid_kind(_) ->
    FlagRaw = #{
        <<"debugEventsUntilDate">> => 1234567,
        <<"deleted">> => false,
        <<"fallthrough">> => #{
            <<"rollout">> => #{
                <<"kind">> => <<"invalid">>,
                <<"variations">> => []
            }
        },
        <<"key">> => <<"flag-full">>,
        <<"offVariation">> => 2,
        <<"on">> => true,
        <<"salt">> => <<"flag-full-salt">>,
        <<"version">> => 9
    },
    FlagExpected = #{
        debugEventsUntilDate  => 1234567,
        deleted                  => false,
        fallthrough              => #{
            bucketBy => #{
                binary => <<"key">>,
                components => [<<"key">>],
                valid => true
            },
            contextKind => <<"user">>,
            variations => [],
            kind => rollout,
            seed => null
        },
        key                      => <<"flag-full">>,
        offVariation            => 2,
        on                       => true,
        prerequisites            => [],
        rules                    => [],
        salt                     => <<"flag-full-salt">>,
        targets                  => [],
        contextTargets           => [],
        trackEvents             => false,
        trackEventsFallthrough => false,
        variations               => [],
        version                  => 9,
        clientSideAvailability   => #{
            usingEnvironmentId => false,
            usingMobileKey => false
        }
    },
    FlagExpected = ldclient_flag:new(FlagRaw).

parse_flag_rollout_kind(_) ->
    FlagRaw = #{
        <<"debugEventsUntilDate">> => 1234567,
        <<"deleted">> => false,
        <<"fallthrough">> => #{
            <<"rollout">> => #{
                <<"kind">> => <<"rollout">>,
                <<"variations">> => []
            }
        },
        <<"key">> => <<"flag-full">>,
        <<"offVariation">> => 2,
        <<"on">> => true,
        <<"salt">> => <<"flag-full-salt">>,
        <<"version">> => 9
    },
    FlagExpected = #{
        debugEventsUntilDate  => 1234567,
        deleted                  => false,
        fallthrough              => #{
            bucketBy => #{
                binary => <<"key">>,
                components => [<<"key">>],
                valid => true
            },
            contextKind => <<"user">>,
            variations => [],
            kind => rollout,
            seed => null
        },
        key                      => <<"flag-full">>,
        offVariation            => 2,
        on                       => true,
        prerequisites            => [],
        rules                    => [],
        salt                     => <<"flag-full-salt">>,
        targets                  => [],
        contextTargets           => [],
        trackEvents             => false,
        trackEventsFallthrough => false,
        variations               => [],
        version                  => 9,
        clientSideAvailability   => #{
            usingEnvironmentId => false,
            usingMobileKey => false
        }
    },
    FlagExpected = ldclient_flag:new(FlagRaw).

parse_flag_experiment_kind(_) ->
    FlagRaw = #{
        <<"debugEventsUntilDate">> => 1234567,
        <<"deleted">> => false,
        <<"fallthrough">> => #{
            <<"rollout">> => #{
                <<"kind">> => <<"experiment">>,
                <<"variations">> => []
            }
        },
        <<"key">> => <<"flag-full">>,
        <<"offVariation">> => 2,
        <<"on">> => true,
        <<"salt">> => <<"flag-full-salt">>,
        <<"version">> => 9
    },
    FlagExpected = #{
        debugEventsUntilDate  => 1234567,
        deleted                  => false,
        fallthrough              => #{
            bucketBy => #{
                binary => <<"key">>,
                components => [<<"key">>],
                valid => true
            },
            contextKind => <<"user">>,
            variations => [],
            kind => experiment,
            seed => null
        },
        key                      => <<"flag-full">>,
        offVariation            => 2,
        on                       => true,
        prerequisites            => [],
        rules                    => [],
        salt                     => <<"flag-full-salt">>,
        targets                  => [],
        contextTargets           => [],
        trackEvents             => false,
        trackEventsFallthrough => false,
        variations               => [],
        version                  => 9,
        clientSideAvailability   => #{
            usingEnvironmentId => false,
            usingMobileKey => false
        }
    },
    FlagExpected = ldclient_flag:new(FlagRaw).

parse_client_side_availability_both_false(_) ->
    BothFalse = #{
        <<"clientSideAvailability">> => #{
            <<"usingEnvironmentId">> => false,
            <<"usingMobileKey">> => false
        }
    },
    #{
        clientSideAvailability := #{
            usingEnvironmentId := false,
            usingMobileKey := false
        }
    } = ldclient_flag:new(BothFalse).

parse_client_side_availability_using_environment_id(_) ->
    UsingEnvironment = #{
        <<"clientSideAvailability">> => #{
            <<"usingEnvironmentId">> => true,
            <<"usingMobileKey">> => false
        }
    },
    #{
        clientSideAvailability := #{
            usingEnvironmentId := true,
            usingMobileKey := false
        }
    } = ldclient_flag:new(UsingEnvironment).

parse_client_side_availability_using_mobile_key(_) ->
    UsingMobileKey = #{
        <<"clientSideAvailability">> => #{
            <<"usingEnvironmentId">> => false,
            <<"usingMobileKey">> => true
        }
    },
    #{
        clientSideAvailability := #{
            usingEnvironmentId := false,
            usingMobileKey := true
        }
    } = ldclient_flag:new(UsingMobileKey).

parse_client_side_availability_both_true(_) ->
    BothTrue = #{
        <<"clientSideAvailability">> => #{
            <<"usingEnvironmentId">> => true,
            <<"usingMobileKey">> => true
        }
    },
    #{
        clientSideAvailability := #{
            usingEnvironmentId := true,
            usingMobileKey := true
        }
    } =  ldclient_flag:new(BothTrue).
