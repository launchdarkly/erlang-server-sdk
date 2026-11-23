-module(ldclient_context_cache_SUITE).

%% ct functions
-export([all/0]).

%% Tests
-export([
    dedups_contexts/1,
    promotes_previous_generation/1,
    drops_oldest_generation/1,
    bounds_cache_on_ingest/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        dedups_contexts,
        promotes_previous_generation,
        drops_oldest_generation,
        bounds_cache_on_ingest
    ].

%%====================================================================
%% Tests
%%====================================================================

%% The first time a context is seen notice_context/3 returns false (an index
%% event is needed); afterwards it returns true (no index).
dedups_contexts(_) ->
    Cache0 = ldclient_context_cache:new(),
    Context = context(<<"a">>),
    {false, Cache1} = ldclient_context_cache:notice_context(Cache0, Context, 100),
    {true, Cache2} = ldclient_context_cache:notice_context(Cache1, Context, 100),
    ok = ldclient_context_cache:delete(Cache2).

%% A context seen in the previous generation is still recognised (and promoted).
promotes_previous_generation(_) ->
    Cache0 = ldclient_context_cache:new(),
    Context = context(<<"b">>),
    {false, Cache1} = ldclient_context_cache:notice_context(Cache0, Context, 1),
    {true, Cache2} = ldclient_context_cache:notice_context(Cache1, Context, 1),
    ok = ldclient_context_cache:delete(Cache2).

%% After two rotations an old context is forgotten so memory stays bounded.
drops_oldest_generation(_) ->
    Cache0 = ldclient_context_cache:new(),
    {false, Cache1} = ldclient_context_cache:notice_context(Cache0, context(<<"c">>), 1),
    {false, Cache2} = ldclient_context_cache:notice_context(Cache1, context(<<"d">>), 1),
    {false, Cache3} = ldclient_context_cache:notice_context(Cache2, context(<<"c">>), 1),
    ok = ldclient_context_cache:delete(Cache3).

%% The cache must stay bounded on ingest, not only when a rotation is triggered
%% externally: the total across both generations never exceeds twice capacity.
bounds_cache_on_ingest(_) ->
    Capacity = 3,
    Cache0 = ldclient_context_cache:new(),
    Cache = lists:foldl(
        fun(I, Current) ->
            Context = context(integer_to_binary(I)),
            {_Seen, Next} = ldclient_context_cache:notice_context(Current, Context, Capacity),
            true = ldclient_context_cache:size(Next) =< (2 * Capacity),
            Next
        end,
        Cache0,
        lists:seq(1, 50)
    ),
    ok = ldclient_context_cache:delete(Cache).

%%====================================================================
%% Helpers
%%====================================================================

context(Key) ->
    ldclient_context:new_from_user(#{key => Key}).
