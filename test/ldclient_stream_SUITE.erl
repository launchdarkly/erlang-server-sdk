-module(ldclient_stream_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    server_process_event_put_patch/1,
    server_process_event_put_patch_flag_with_extra_property/1,
    server_process_event_put_patch_old_version/1,
    server_process_event_put_delete/1,
    server_process_event_other/1,
    parse_shotgun_event/1,
    parse_shotgun_event_optional_spaces/1,
    split_sse_events/1,
    stream_chunk_processes_complete_events_and_buffers_the_rest/1,
    heartbeat_comment_restarts_read_timer_without_events/1,
    stream_chunk_from_replaced_connection_is_ignored/1,
    read_timeout_closes_the_connection/1,
    stale_read_timeout_is_ignored/1,
    read_timeout_disabled_starts_no_timer/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        server_process_event_put_patch,
        server_process_event_put_patch_flag_with_extra_property,
        server_process_event_put_patch_old_version,
        server_process_event_put_delete,
        server_process_event_other,
        parse_shotgun_event,
        parse_shotgun_event_optional_spaces,
        split_sse_events,
        stream_chunk_processes_complete_events_and_buffers_the_rest,
        heartbeat_comment_restarts_read_timer_without_events,
        stream_chunk_from_replaced_connection_is_ignored,
        read_timeout_closes_the_connection,
        stale_read_timeout_is_ignored,
        read_timeout_disabled_starts_no_timer
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ldclient),
    Options = #{
        stream => false,
        polling_update_requestor => ldclient_update_requestor_test
    },
    ldclient:start_instance("", Options),
    Config.

end_per_suite(_) ->
    ok = application:stop(ldclient),
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok = ldclient_storage_ets:empty(default, features),
    ok.

%%====================================================================
%% Helpers
%%====================================================================

stream_state(Conn) ->
    #{
        conn => Conn,
        feature_store => ldclient_storage_ets,
        storage_tag => default,
        sse_buffer => <<>>,
        read_timer => undefined,
        read_timeout_ms => 60000
    }.

put_event_bin() ->
    {_FlagSimpleKey, FlagSimpleBin, _FlagSimpleMap} = ldclient_test_utils:get_simple_flag(),
    <<"event: put\ndata: {\"path\":\"/\",\"data\":{\"flags\":{", FlagSimpleBin/binary, "},\"segments\":{}}}">>.

stop_timer(#{read_timer := undefined}) -> ok;
stop_timer(#{read_timer := TimerRef}) ->
    _ = erlang:cancel_timer(TimerRef),
    ok.

%%====================================================================
%% Tests
%%====================================================================

server_process_event_put_patch(_) ->
    {FlagSimpleKey, FlagSimpleBin, FlagSimpleMap} = ldclient_test_utils:get_simple_flag(),
    PutData = <<"{\"path\":\"/\",",
        "\"data\":{",
            "\"flags\":{",
                FlagSimpleBin/binary,
            "},",
            "\"segments\":{}",
        "}",
    "}">>,
    ok = ldclient_update_stream_server:process_event(#{event => <<"put">>, data => PutData}, ldclient_storage_ets, default),
    [] = ldclient_storage_ets:all(default, segments),
    ParsedFlagSimpleMap = ldclient_flag:new(FlagSimpleMap),
    [{FlagSimpleKey, ParsedFlagSimpleMap}] = ldclient_storage_ets:all(default, features),
    {FlagSimpleKey, FlagPatchBin, FlagPatchMap} = ldclient_test_utils:get_simple_flag_patch(),
    PatchData = <<"{\"path\":\"/flags/", FlagSimpleKey/binary, "\",", FlagPatchBin/binary, "}">>,
    ok = ldclient_update_stream_server:process_event(#{event => <<"patch">>, data => PatchData}, ldclient_storage_ets, default),
    ParsedFlagPatchMap = ldclient_flag:new(FlagPatchMap),
    [{FlagSimpleKey, ParsedFlagPatchMap}] = ldclient_storage_ets:all(default, features),
    ok.

server_process_event_put_patch_flag_with_extra_property(_) ->
    {FlagSimpleKey, FlagSimpleBin, FlagSimpleMap} = ldclient_test_utils:get_flag_with_extra_property(),
    PutData = <<"{\"path\":\"/\",",
        "\"data\":{",
            "\"flags\":{",
                FlagSimpleBin/binary,
            "},",
            "\"segments\":{}",
        "}",
    "}">>,
    ok = ldclient_update_stream_server:process_event(#{event => <<"put">>, data => PutData}, ldclient_storage_ets, default),
    [] = ldclient_storage_ets:all(default, segments),
    ParsedFlagSimpleMap = ldclient_flag:new(FlagSimpleMap),
    [{FlagSimpleKey, ParsedFlagSimpleMap}] = ldclient_storage_ets:all(default, features),
    {FlagSimpleKey, FlagPatchBin, FlagPatchMap} = ldclient_test_utils:get_simple_flag_patch(),
    PatchData = <<"{\"path\":\"/flags/", FlagSimpleKey/binary, "\",", FlagPatchBin/binary, "}">>,
    ok = ldclient_update_stream_server:process_event(#{event => <<"patch">>, data => PatchData}, ldclient_storage_ets, default),
    ParsedFlagPatchMap = ldclient_flag:new(FlagPatchMap),
    [{FlagSimpleKey, ParsedFlagPatchMap}] = ldclient_storage_ets:all(default, features),
    ok.

server_process_event_put_patch_old_version(_) ->
    {FlagSimpleKey, FlagSimpleBin, FlagSimpleMap} = ldclient_test_utils:get_simple_flag(),
    PutData = <<"{\"path\":\"/\",",
        "\"data\":{",
            "\"flags\":{",
                FlagSimpleBin/binary,
            "},",
            "\"segments\":{}",
        "}",
    "}">>,
    ok = ldclient_update_stream_server:process_event(#{event => <<"put">>, data => PutData}, ldclient_storage_ets, default),
    [] = ldclient_storage_ets:all(default, segments),
    ParsedFlagSimpleMap = ldclient_flag:new(FlagSimpleMap),
    [{FlagSimpleKey, ParsedFlagSimpleMap}] = ldclient_storage_ets:all(default, features),
    {FlagSimpleKey, FlagPatchBin, _FlagPatchMap} = ldclient_test_utils:get_simple_flag_patch_old(),
    PatchData = <<"{\"path\":\"/flags/", FlagSimpleKey/binary, "\",", FlagPatchBin/binary, "}">>,
    ok = ldclient_update_stream_server:process_event(#{event => <<"patch">>, data => PatchData}, ldclient_storage_ets, default),
    [{FlagSimpleKey, ParsedFlagSimpleMap}] = ldclient_storage_ets:all(default, features),
    ok.

server_process_event_put_delete(_) ->
    {FlagSimpleKey, FlagSimpleBin, FlagSimpleMap} = ldclient_test_utils:get_simple_flag(),
    PutData = <<"{\"path\":\"/\",",
        "\"data\":{",
            "\"flags\":{",
                FlagSimpleBin/binary,
            "},",
            "\"segments\":{}",
        "}",
    "}">>,
    ok = ldclient_update_stream_server:process_event(#{event => <<"put">>, data => PutData}, ldclient_storage_ets, default),
    [] = ldclient_storage_ets:all(default, segments),
    ParsedFlagSimpleMap = ldclient_flag:new(FlagSimpleMap),
    [{FlagSimpleKey, ParsedFlagSimpleMap}] = ldclient_storage_ets:all(default, features),
    ok = ldclient_update_stream_server:process_event(#{event => <<"delete">>, data => <<"{\"path\": \"/flags/", FlagSimpleKey/binary,"\", \"version\": 6}">>}, ldclient_storage_ets, default),
    {FlagSimpleKey, _FlagDeleteBin, FlagDeleteMap} = ldclient_test_utils:get_simple_flag_delete(),
    ParsedFlagDeleteMap = ldclient_flag:new(FlagDeleteMap),
    [{FlagSimpleKey, ParsedFlagDeleteMap}] = ldclient_storage_ets:all(default, features),
    ok.

server_process_event_other(_) ->
    ok = ldclient_update_stream_server:process_event(#{event => <<"unsupported-event">>, data => <<"foo">>}, ldclient_storage_ets, default),
    [] = ldclient_storage_ets:all(default, features),
    [] = ldclient_storage_ets:all(default, segments),
    ok.

parse_shotgun_event(_) ->
    EventBin = <<"event:put\ndata:foo">>,
    ExpectedEvent = #{event => <<"put">>, data => <<"foo\n">>},
    ExpectedEvent = ldclient_update_stream_server:parse_shotgun_event(EventBin).

parse_shotgun_event_optional_spaces(_) ->
    EventBin = <<"event: put\ndata: foo">>,
    ExpectedEvent = #{event => <<"put">>, data => <<"foo\n">>},
    ExpectedEvent = ldclient_update_stream_server:parse_shotgun_event(EventBin).

split_sse_events(_) ->
    {[], <<>>} = ldclient_update_stream_server:split_sse_events(<<>>),
    {[], <<>>} = ldclient_update_stream_server:split_sse_events(<<":\n">>),
    {[], <<>>} = ldclient_update_stream_server:split_sse_events(<<":\n:\n:\n">>),
    {[], <<":">>} = ldclient_update_stream_server:split_sse_events(<<":\n:">>),
    {[], <<"event: put\ndata: {">>} = ldclient_update_stream_server:split_sse_events(<<":\nevent: put\n:\ndata: {">>),
    {[<<"event: put\ndata: {}">>], <<>>} =
        ldclient_update_stream_server:split_sse_events(<<"event: put\ndata: {}\n\n">>),
    {[<<":\nevent: put\ndata: {}">>, <<"event: patch\ndata: {}">>], <<"event: del">>} =
        ldclient_update_stream_server:split_sse_events(
            <<":\nevent: put\ndata: {}\n\nevent: patch\ndata: {}\n\nevent: del">>).

stream_chunk_processes_complete_events_and_buffers_the_rest(_) ->
    {FlagSimpleKey, _FlagSimpleBin, FlagSimpleMap} = ldclient_test_utils:get_simple_flag(),
    Conn = spawn(fun() -> receive stop -> ok end end),
    State = stream_state(Conn),
    PutEvent = put_event_bin(),
    {Head, Tail} = split_binary(PutEvent, 10),
    {noreply, State1} = ldclient_update_stream_server:handle_info({stream_chunk, Conn, nofin, Head}, State),
    [] = ldclient_storage_ets:all(default, features),
    Head = maps:get(sse_buffer, State1),
    {noreply, State2} = ldclient_update_stream_server:handle_info(
        {stream_chunk, Conn, nofin, <<Tail/binary, "\n\nevent: pat">>}, State1),
    ParsedFlagSimpleMap = ldclient_flag:new(FlagSimpleMap),
    [{FlagSimpleKey, ParsedFlagSimpleMap}] = ldclient_storage_ets:all(default, features),
    <<"event: pat">> = maps:get(sse_buffer, State2),
    true = is_reference(maps:get(read_timer, State2)),
    true = maps:get(read_timer, State1) =/= maps:get(read_timer, State2),
    ok = stop_timer(State2),
    Conn ! stop,
    ok.

heartbeat_comment_restarts_read_timer_without_events(_) ->
    Conn = spawn(fun() -> receive stop -> ok end end),
    State = stream_state(Conn),
    {noreply, State1} = ldclient_update_stream_server:handle_info({stream_chunk, Conn, nofin, <<":\n">>}, State),
    [] = ldclient_storage_ets:all(default, features),
    <<>> = maps:get(sse_buffer, State1),
    true = is_reference(maps:get(read_timer, State1)),
    {noreply, State2} = ldclient_update_stream_server:handle_info(
        {stream_chunk, Conn, nofin, <<(put_event_bin())/binary, "\n\n">>}, State1),
    [{_Key, _Flag}] = ldclient_storage_ets:all(default, features),
    <<>> = maps:get(sse_buffer, State2),
    ok = stop_timer(State2),
    Conn ! stop,
    ok.

stream_chunk_from_replaced_connection_is_ignored(_) ->
    Conn = spawn(fun() -> receive stop -> ok end end),
    Stale = spawn(fun() -> receive stop -> ok end end),
    State = stream_state(Conn),
    {noreply, State} = ldclient_update_stream_server:handle_info(
        {stream_chunk, Stale, nofin, <<(put_event_bin())/binary, "\n\n">>}, State),
    [] = ldclient_storage_ets:all(default, features),
    Conn ! stop,
    Stale ! stop,
    ok.

read_timeout_closes_the_connection(_) ->
    Conn = spawn(fun() -> receive stop -> ok end end),
    TimerRef = make_ref(),
    State = (stream_state(Conn))#{read_timer => TimerRef},
    ok = meck:new(shotgun, [passthrough]),
    ok = meck:expect(shotgun, close, fun(_Pid) -> ok end),
    try
        {noreply, State1} = ldclient_update_stream_server:handle_info({timeout, TimerRef, read_timeout}, State),
        undefined = maps:get(read_timer, State1),
        true = meck:called(shotgun, close, [Conn])
    after
        meck:unload(shotgun),
        Conn ! stop
    end,
    ok.

stale_read_timeout_is_ignored(_) ->
    Conn = spawn(fun() -> receive stop -> ok end end),
    State = (stream_state(Conn))#{read_timer => make_ref()},
    ok = meck:new(shotgun, [passthrough]),
    ok = meck:expect(shotgun, close, fun(_Pid) -> ok end),
    try
        {noreply, State} = ldclient_update_stream_server:handle_info({timeout, make_ref(), read_timeout}, State),
        false = meck:called(shotgun, close, '_')
    after
        meck:unload(shotgun),
        Conn ! stop
    end,
    ok.

read_timeout_disabled_starts_no_timer(_) ->
    Conn = spawn(fun() -> receive stop -> ok end end),
    State = (stream_state(Conn))#{read_timeout_ms => 0},
    {noreply, State1} = ldclient_update_stream_server:handle_info(
        {stream_chunk, Conn, nofin, <<(put_event_bin())/binary, "\n\n">>}, State),
    undefined = maps:get(read_timer, State1),
    [{_Key, _Flag}] = ldclient_storage_ets:all(default, features),
    {noreply, State2} = ldclient_update_stream_server:handle_info({stream_chunk, Conn, nofin, <<":\n">>}, State1),
    undefined = maps:get(read_timer, State2),
    Conn ! stop,
    ok.
