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
    parse_segment_full/1
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
        parse_segment_full
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
        debug_events_until_date  => null,
        deleted                  => false,
        fallthrough              => #{bucket_by => key, variations => []},
        key                      => <<>>,
        off_variation            => 0,
        on                       => false,
        prerequisites            => [],
        rules                    => [],
        salt                     => <<>>,
        targets                  => [],
        track_events             => false,
        track_events_fallthrough => false,
        variations               => [],
        version                  => 0
    },
    FlagExpected = ldclient_flag:new(FlagRaw).

parse_flag_key_only(_) ->
    FlagRaw = #{
        <<"key">> => <<"flag-key-only">>
    },
    FlagExpected = #{
        debug_events_until_date  => null,
        deleted                  => false,
        fallthrough              => #{bucket_by => key, variations => []},
        key                      => <<"flag-key-only">>,
        off_variation            => 0,
        on                       => false,
        prerequisites            => [],
        rules                    => [],
        salt                     => <<>>,
        targets                  => [],
        track_events             => false,
        track_events_fallthrough => false,
        variations               => [],
        version                  => 0
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
        debug_events_until_date  => 12345,
        deleted                  => true,
        fallthrough              => 1,
        key                      => <<"flag-bare">>,
        off_variation            => 1,
        on                       => true,
        prerequisites            => [],
        rules                    => [],
        salt                     => <<>>,
        targets                  => [],
        track_events             => true,
        track_events_fallthrough => true,
        variations               => [true, false],
        version                  => 10
    },
    FlagExpected = ldclient_flag:new(FlagRaw).

parse_flag_full(_) ->
    FlagRaw = #{
        <<"debugEventsUntilDate">> => 1234567,
        <<"deleted">> => false,
        <<"fallthrough">> => #{
            <<"rollout">> => #{
                <<"bucketBy">> => <<"foo">>,
                <<"variations">> => [
                    #{<<"variation">> => 0, <<"weight">> => 0},
                    #{<<"variation">> => 1, <<"weight">> => 40000},
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
        <<"trackEvents">> => true,
        <<"trackEventsFallthrough">> => true,
        <<"variations">> => [<<"A">>, <<"B">>, <<"C">>],
        <<"version">> => 9
    },
    FlagExpected = #{
        debug_events_until_date  => 1234567,
        deleted                  => false,
        fallthrough              => #{
            bucket_by => <<"foo">>,
            variations => [
                #{variation => 0, weight => 0},
                #{variation => 1, weight => 40000},
                #{variation => 2, weight => 60000}
            ]
        },
        key                      => <<"flag-full">>,
        off_variation            => 2,
        on                       => true,
        prerequisites            => [
            #{key => <<"flag-foo">>, variation => 5},
            #{key => <<"flag-bar">>, variation => 3}
        ],
        rules                    => [
            #{
                id => <<"rule-foo">>,
                track_events => true,
                variation_or_rollout => 3,
                clauses => [
                    #{
                        attribute => <<"user-attr-foo">>,
                        negate => false,
                        op => contains,
                        values => [<<"rule-foo-value1">>, <<"rule-foo-value2">>]
                    }
                ]
            }
        ],
        salt                     => <<"flag-full-salt">>,
        targets                  => [
            #{variation => 0, values => [<<"user-target1">>, <<"user-target-2">>]},
            #{variation => 1, values => [<<"user-target3">>, <<"user-target-4">>]},
            #{variation => 2, values => []}
        ],
        track_events             => true,
        track_events_fallthrough => true,
        variations               => [<<"A">>, <<"B">>, <<"C">>],
        version                  => 9
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
        debug_events_until_date  => null,
        deleted                  => false,
        fallthrough              => #{bucket_by => <<"foo">>, variations => [#{variation => 1, weight => 0}]},
        key                      => <<"flag-ignore-invalid">>,
        off_variation            => 2,
        on                       => true,
        prerequisites            => [],
        rules                    => [],
        salt                     => <<>>,
        targets                  => [],
        track_events             => false,
        track_events_fallthrough => false,
        variations               => [<<"A">>, <<"B">>, <<"C">>],
        version                  => 0
    },
    FlagExpected = ldclient_flag:new(FlagRaw).

parse_flag_invalid_fallthrough(_) ->
    FlagRaw = #{
        <<"key">> => <<"flag-invalid-fallthrough">>,
        <<"fallthrough">> => #{}
    },
    FlagExpected = #{
        debug_events_until_date  => null,
        deleted                  => false,
        fallthrough              => #{bucket_by => key, variations => []},
        key                      => <<"flag-invalid-fallthrough">>,
        off_variation            => 0,
        on                       => false,
        prerequisites            => [],
        rules                    => [],
        salt                     => <<>>,
        targets                  => [],
        track_events             => false,
        track_events_fallthrough => false,
        variations               => [],
        version                  => 0
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
        version  => 0
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
        version  => 0
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
        <<"version">> => 5
    },
    SegmentExpected = #{
        key      => <<"segment-full">>,
        deleted  => true,
        excluded => [<<"123">>, <<"456">>],
        included => [<<"789">>],
        rules    => [
            #{
                bucket_by => key,
                weight => null,
                segment_key => <<"segment-full">>,
                segment_salt => <<"segment-full-salt">>,
                clauses => [
                    #{
                        attribute => <<"user-attr-foo">>,
                        negate => false,
                        op => contains,
                        values => [<<"rule-foo-value1">>, <<"rule-foo-value2">>]
                    }
                ]
            }
        ],
        salt     => <<"segment-full-salt">>,
        version  => 5
    },
    SegmentExpected = ldclient_segment:new(SegmentRaw).
