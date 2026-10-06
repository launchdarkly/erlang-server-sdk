-module(ldclient_context_cache_SUITE).

%% ct functions
-export([all/0]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    dedups_contexts/1,
    promotes_previous_generation/1,
    drops_oldest_generation/1
]).

-define(TAG, context_cache_test).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        dedups_contexts,
        promotes_previous_generation,
        drops_oldest_generation
    ].

init_per_testcase(_, Config) ->
    ok = ldclient_context_cache:new(?TAG),
    Config.

end_per_testcase(_, Config) ->
    ok = ldclient_context_cache:delete(?TAG),
    Config.

%%====================================================================
%% Tests
%%====================================================================

%% The first time a context is seen notice_context/2 returns false (an index
%% event is needed); afterwards it returns true (no index).
dedups_contexts(_) ->
    Context = context(<<"a">>),
    false = ldclient_context_cache:notice_context(?TAG, Context),
    true = ldclient_context_cache:notice_context(?TAG, Context).

%% A context seen in the previous generation is still recognised (and promoted).
promotes_previous_generation(_) ->
    Context = context(<<"b">>),
    false = ldclient_context_cache:notice_context(?TAG, Context),
    ok = ldclient_context_cache:maybe_rotate(?TAG, 1),
    true = ldclient_context_cache:notice_context(?TAG, Context).

%% After two rotations an old context is forgotten so memory stays bounded.
drops_oldest_generation(_) ->
    Context = context(<<"c">>),
    false = ldclient_context_cache:notice_context(?TAG, Context),
    ok = ldclient_context_cache:maybe_rotate(?TAG, 1),
    false = ldclient_context_cache:notice_context(?TAG, context(<<"d">>)),
    ok = ldclient_context_cache:maybe_rotate(?TAG, 1),
    false = ldclient_context_cache:notice_context(?TAG, Context).

%%====================================================================
%% Helpers
%%====================================================================

context(Key) ->
    ldclient_context:new_from_user(#{key => Key}).
