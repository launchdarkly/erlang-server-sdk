-module(ldclient_segment_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    can_include_user/1,
    can_exclude_user/1,
    can_include_non_user_context/1,
    can_exclude_non_user_context/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        can_include_user,
        can_exclude_user,
        can_include_non_user_context,
        can_exclude_non_user_context
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
%% Helper Functions
%%====================================================================

%%====================================================================
%% Tests
%%====================================================================

%% Tests which do not contain nested segements may pass null for storage
%% and for client tags.

can_include_user(_) ->
    Segment = ldclient_segment:new(#{
        <<"included">> => [<<"user-key">>]
    }),
    match = ldclient_segment:match_context(Segment, ldclient_context:new(<<"user-key">>), null, null, []).

can_exclude_user(_) ->
    Segment = ldclient_segment:new(#{
        <<"excluded">> => [<<"user-key">>],
        <<"rules">> => [#{
            <<"clauses">> => [#{
                <<"attribute">> => <<"attr-1">>,
                <<"op">> => <<"in">>,
                <<"values">> => [<<"yes">>],
                <<"negate">> => false,
                <<"contextKind">> => <<"user">>
            }]
        }]
    }),
    match = ldclient_segment:match_context(Segment,
        ldclient_context:set(<<"attr-1">>, <<"yes">>,
            ldclient_context:new(<<"not-excluded">>)),
        null,
        null,
        []
    ),
    no_match = ldclient_segment:match_context(Segment,
        ldclient_context:set(<<"attr-1">>, <<"yes">>,
            ldclient_context:new(<<"user-key">>)),
        null,
        null,
        []
    ).

can_include_non_user_context(_) ->
    Segment = ldclient_segment:new(#{
        <<"includedContexts">> => [#{
            <<"contextKind">> => <<"org">>,
            <<"values">> => [<<"decoy">>, <<"org-key">>]
        }]
    }),
    match = ldclient_segment:match_context(Segment, ldclient_context:new(<<"org-key">>, <<"org">>), null, null, []),
    no_match = ldclient_segment:match_context(Segment,
        ldclient_context:new(<<"potato-key">>, <<"org">>), null, null, []).

can_exclude_non_user_context(_) ->
    Segment = ldclient_segment:new(#{
        <<"excludedContexts">> => [#{
            <<"contextKind">> => <<"org">>,
            <<"values">> => [<<"org-key">>]
        }],
        <<"rules">> => [#{
            <<"clauses">> => [#{
                <<"attribute">> => <<"attr-1">>,
                <<"op">> => <<"in">>,
                <<"values">> => [<<"yes">>],
                <<"negate">> => false,
                <<"contextKind">> => <<"org">>
            }]
        }]
    }),
    match = ldclient_segment:match_context(Segment,
        ldclient_context:set(<<"attr-1">>, <<"yes">>,
            ldclient_context:new(<<"not-excluded">>, <<"org">>)),
        null,
        null,
        []
    ),
    no_match = ldclient_segment:match_context(Segment,
        ldclient_context:set(<<"attr-1">>, <<"yes">>,
            ldclient_context:new(<<"org-key">>, <<"org">>)),
        null,
        null,
        []
    ).
