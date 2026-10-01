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
    pool_uses_multiple_workers/1,
    autoscales_worker_pool/1,
    retries_transient_failures_with_backoff/1,
    emits_published_telemetry/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        add_event_is_cast,
        sheds_when_buffer_at_threshold,
        pool_uses_multiple_workers,
        autoscales_worker_pool,
        retries_transient_failures_with_backoff,
        emits_published_telemetry
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
    ScalerOptions = #{
        stream => false,
        events_dispatcher => ldclient_event_dispatch_test,
        polling_update_requestor => ldclient_update_requestor_test,
        events_capacity => 100,
        events_shed_threshold => 1000,
        events_flush_interval => 60000,
        events_min_workers => 1,
        events_max_workers => 3,
        events_batch_size => 100,
        events_scale_up_threshold => 1,
        events_scale_down_threshold => 0,
        events_scale_interval_ms => 50,
        events_scale_cooldown_ms => 0
    },
    ldclient:start_instance("", scaler, ScalerOptions),
    FailingOptions = #{
        stream => false,
        events_dispatcher => ldclient_event_dispatch_test,
        polling_update_requestor => ldclient_update_requestor_test,
        events_capacity => 100,
        events_shed_threshold => 1000,
        events_flush_interval => 60000
    },
    ldclient:start_instance("sdk-key-events-fail", failing, FailingOptions),
    PublisherOptions = #{
        stream => false,
        events_dispatcher => ldclient_event_dispatch_test,
        polling_update_requestor => ldclient_update_requestor_test,
        events_capacity => 100,
        events_shed_threshold => 1000,
        events_flush_interval => 60000
    },
    ldclient:start_instance("", publisher, PublisherOptions),
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

%% The pool scales workers up as the buffer depth grows and back down once it
%% is drained, staying within [min_workers, max_workers].
autoscales_worker_pool(_) ->
    Tag = scaler,
    SupName = ldclient_event_worker_sup:get_sup_name(Tag),
    HandlerId = {?MODULE, autoscales_worker_pool, self()},
    Self = self(),
    ok = telemetry:attach(
        HandlerId,
        [ldclient, events, pool_size],
        fun(_Event, _Measurements, Metadata, _Config) ->
            Self ! {pool_size, maps:get(direction, Metadata)}
        end,
        undefined
    ),
    register_collector(),
    try
        1 = length(supervisor:which_children(SupName)),
        Keys = [<<"k1">>, <<"k2">>, <<"k3">>, <<"k4">>, <<"k5">>],
        [ok = ldclient_event_server:add_event(Tag, identify_event(K), #{}) || K <- Keys],
        wait_for_event_count(Tag, 5),
        wait_for_worker_count(SupName, 3, 100),
        ok = ldclient_event_server:flush(Tag),
        _ = collect_payloads(1),
        wait_for_worker_count(SupName, 1, 100),
        Directions = collect_scales([]),
        true = lists:member(up, Directions),
        true = lists:member(down, Directions)
    after
        telemetry:detach(HandlerId)
    end.

%% Transient dispatch failures are retried with backoff and reported through
%% telemetry rather than at a fixed interval with no visibility.
retries_transient_failures_with_backoff(_) ->
    Tag = failing,
    HandlerId = {?MODULE, retries_transient_failures_with_backoff, self()},
    Self = self(),
    ok = telemetry:attach(
        HandlerId,
        [ldclient, events, send_error],
        fun(_Event, _Measurements, Metadata, _Config) ->
            Self ! {send_error, maps:get(type, Metadata)}
        end,
        undefined
    ),
    register_collector(),
    try
        ok = ldclient_event_server:add_event(Tag, identify_event(<<"retry">>), #{}),
        wait_for_event_count(Tag, 1),
        ok = ldclient_event_server:flush(Tag),
        receive
            {send_error, temporary} -> ok
        after 1000 ->
            ct:fail("Expected a temporary send_error telemetry event")
        end,
        %% The first attempt and the retry are both dispatched; delivery order
        %% and timing are not asserted, only that a retry happens.
        2 = length(collect_payloads(2))
    after
        telemetry:detach(HandlerId)
    end.

%% Successful dispatches are reported with the number of events delivered, so a
%% "published events" counter metric can be derived from telemetry alone.
emits_published_telemetry(_) ->
    Tag = publisher,
    HandlerId = {?MODULE, emits_published_telemetry, self()},
    Self = self(),
    ok = telemetry:attach(
        HandlerId,
        [ldclient, events, published],
        fun(_Event, Measurements, Metadata, _Config) ->
            Self ! {published, Measurements, Metadata}
        end,
        undefined
    ),
    register_collector(),
    try
        Keys = [<<"pub1">>, <<"pub2">>],
        [ok = ldclient_event_server:add_event(Tag, identify_event(K), #{}) || K <- Keys],
        wait_for_event_count(Tag, 2),
        ok = ldclient_event_server:flush(Tag),
        _ = collect_payloads(1),
        receive
            {published, #{count := 2}, #{tag := Tag}} -> ok
        after 1000 ->
            ct:fail("Expected a published telemetry event with count 2")
        end
    after
        telemetry:detach(HandlerId)
    end.

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

wait_for_worker_count(_SupName, _Expected, 0) ->
    ct:fail("Worker pool did not reach the expected size");
wait_for_worker_count(SupName, Expected, Retries) ->
    case length(supervisor:which_children(SupName)) of
        Expected ->
            ok;
        _ ->
            timer:sleep(10),
            wait_for_worker_count(SupName, Expected, Retries - 1)
    end.

collect_scales(Acc) ->
    receive
        {pool_size, Direction} -> collect_scales([Direction|Acc])
    after 50 ->
        Acc
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
        {EventsBin, _PayloadId} when is_binary(EventsBin) ->
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
