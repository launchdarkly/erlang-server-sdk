-module(ldclient_event_server_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    add_event_is_cast/1,
    sheds_when_buffer_at_threshold/1,
    pool_uses_multiple_workers/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        add_event_is_cast,
        sheds_when_buffer_at_threshold,
        pool_uses_multiple_workers
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ldclient),
    Options = #{
        stream => false,
        events_dispatcher => ldclient_event_dispatch_test,
        polling_update_requestor => ldclient_update_requestor_test,
        events_capacity => 100,
        events_shed_threshold => 1,
        events_flush_interval => 60000
    },
    ldclient:start_instance("", shedder, Options),
    PoolOptions = #{
        stream => false,
        events_dispatcher => ldclient_event_dispatch_test,
        polling_update_requestor => ldclient_update_requestor_test,
        events_capacity => 100,
        events_shed_threshold => 1000,
        events_flush_interval => 60000,
        events_min_workers => 3,
        events_batch_size => 1
    },
    ldclient:start_instance("", pooler, PoolOptions),
    Config.

end_per_suite(_) ->
    ok = application:stop(ldclient).

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    _ = catch unregister(ldclient_test_events),
    Config.

%%====================================================================
%% Tests
%%====================================================================

%% add_event/3 must not block the caller: it is a cast, so it returns ok even
%% when there is no event server process for the given tag.
add_event_is_cast(_) ->
    Event = identify_event(<<"no-server">>),
    ok = ldclient_event_server:add_event(no_such_tag, Event, #{}).

%% Once the buffered event count reaches the shed threshold, further events are
%% dropped at the caller with a telemetry event instead of being enqueued.
sheds_when_buffer_at_threshold(_) ->
    Tag = shedder,
    HandlerId = {?MODULE, sheds_when_buffer_at_threshold, self()},
    Self = self(),
    ok = telemetry:attach(
        HandlerId,
        [ldclient, events, shed],
        fun(_Event, Measurements, Metadata, _Config) ->
            Self ! {shed, Measurements, Metadata}
        end,
        undefined
    ),
    register_event_forwarding_process(),
    try
        ok = ldclient_event_server:add_event(Tag, identify_event(<<"first">>), #{}),
        wait_for_event_count(Tag, 1),
        ok = ldclient_event_server:add_event(Tag, identify_event(<<"second">>), #{}),
        receive
            {shed, #{count := 1}, #{tag := Tag}} -> ok
        after 1000 ->
            ct:fail("Expected a shed telemetry event")
        end,
        ok = ldclient_event_server:flush(Tag),
        {ActualEvents, _PayloadId} = receive_events(),
        %% Only the first event made it into the buffer.
        1 = length(ActualEvents),
        #{<<"context">> := #{<<"key">> := <<"first">>}} = hd(ActualEvents)
    after
        telemetry:detach(HandlerId)
    end.

%% The pool dispatches batches across multiple workers concurrently. With a
%% batch size of 1 and three workers, a flush of three events fans out into
%% three independent payloads (delivery order is not guaranteed).
pool_uses_multiple_workers(_) ->
    Tag = pooler,
    SupName = ldclient_event_worker_sup:get_sup_name(Tag),
    3 = length(supervisor:which_children(SupName)),
    register_collector(),
    Keys = [<<"a">>, <<"b">>, <<"c">>],
    [ok = ldclient_event_server:add_event(Tag, identify_event(K), #{}) || K <- Keys],
    wait_for_event_count(Tag, 3),
    ok = ldclient_event_server:flush(Tag),
    Payloads = collect_payloads(3),
    GotKeys = lists:sort([K || Payload <- Payloads, #{<<"context">> := #{<<"key">> := K}} <- Payload]),
    Keys = GotKeys.

%%====================================================================
%% Helpers
%%====================================================================

identify_event(Key) ->
    ldclient_event:new_identify(ldclient_context:new_from_user(#{key => Key})).

wait_for_event_count(Tag, Expected) ->
    ServerName = list_to_atom("ldclient_event_server_" ++ atom_to_list(Tag)),
    wait_for_event_count(ServerName, Expected, 50).

wait_for_event_count(_ServerName, _Expected, 0) ->
    ct:fail("Event server did not buffer the expected event");
wait_for_event_count(ServerName, Expected, Retries) ->
    case sys:get_state(ServerName) of
        #{event_count := Count} when Count >= Expected ->
            ok;
        _ ->
            timer:sleep(10),
            wait_for_event_count(ServerName, Expected, Retries - 1)
    end.

register_event_forwarding_process() ->
    TestPid = self(),
    ErPid = spawn(fun() -> receive ErEvents -> TestPid ! ErEvents end end),
    true = register(ldclient_test_events, ErPid).

register_collector() ->
    TestPid = self(),
    _ = catch unregister(ldclient_test_events),
    Collector = spawn(fun() -> collector_loop(TestPid) end),
    true = register(ldclient_test_events, Collector).

collector_loop(TestPid) ->
    receive
        Msg ->
            TestPid ! Msg,
            collector_loop(TestPid)
    end.

collect_payloads(Count) ->
    collect_payloads(Count, []).

collect_payloads(0, Acc) ->
    Acc;
collect_payloads(Count, Acc) ->
    receive
        {EventsBin, _PayloadId} ->
            collect_payloads(Count - 1, [jsx:decode(EventsBin, [return_maps])|Acc])
    after 2000 ->
        ct:fail("Did not receive ~b payloads", [Count])
    end.

receive_events() ->
    receive
        {EventsReceived, PayloadIdReceived} ->
            ActualEvents = jsx:decode(EventsReceived, [return_maps]),
            {ActualEvents, PayloadIdReceived}
    after 2000 ->
        ct:fail("Did not receive events")
    end.
