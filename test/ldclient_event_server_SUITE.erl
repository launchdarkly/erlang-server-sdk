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
    retries_transient_failures_once/1,
    emits_published_telemetry/1,
    offline_instance_does_not_send/1,
    preserves_summary_when_shedding/1,
    permanent_failures_are_not_retried/1,
    decommission_waits_for_pending_retries/1,
    flush_not_extended_by_new_events/1,
    flush_does_not_overshoot_window/1,
    flush_returns_without_waiting_for_delivery/1,
    default_pool_bounds/1,
    decommission_delivers_all_pending_retries/1,
    feature_requests_shed_when_inbox_full/1,
    flush_sends_one_payload_by_default/1,
    worker_exit_mid_batch_redispatches/1,
    invalid_options_fall_back_to_defaults/1,
    capacity_drops_reported_once_per_flush/1,
    unencodable_events_do_not_lose_the_batch/1
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
        retries_transient_failures_once,
        emits_published_telemetry,
        offline_instance_does_not_send,
        preserves_summary_when_shedding,
        permanent_failures_are_not_retried,
        decommission_waits_for_pending_retries,
        flush_not_extended_by_new_events,
        flush_does_not_overshoot_window,
        flush_returns_without_waiting_for_delivery,
        default_pool_bounds,
        decommission_delivers_all_pending_retries,
        feature_requests_shed_when_inbox_full,
        flush_sends_one_payload_by_default,
        worker_exit_mid_batch_redispatches,
        invalid_options_fall_back_to_defaults,
        capacity_drops_reported_once_per_flush,
        unencodable_events_do_not_lose_the_batch
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
        events_dispatcher => ldclient_event_dispatch_slow,
        polling_update_requestor => ldclient_update_requestor_test,
        events_capacity => 100,
        events_shed_threshold => 1000,
        events_flush_interval => 60000,
        events_min_workers => 1,
        events_max_workers => 3,
        events_batch_size => 1,
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
        events_flush_interval => 60000,
        events_min_workers => 1,
        events_max_workers => 1
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
    OfflineOptions = #{
        stream => false,
        offline => true,
        events_dispatcher => ldclient_event_dispatch_test,
        polling_update_requestor => ldclient_update_requestor_test,
        events_capacity => 100,
        events_shed_threshold => 1000,
        events_flush_interval => 60000
    },
    ldclient:start_instance("", offline_events, OfflineOptions),
    SummaryOptions = #{
        stream => false,
        events_dispatcher => ldclient_event_dispatch_test,
        polling_update_requestor => ldclient_update_requestor_test,
        events_capacity => 1,
        events_shed_threshold => 1,
        events_inbox_capacity => 1000,
        events_flush_interval => 60000
    },
    ldclient:start_instance("", summary_shedder, SummaryOptions),
    PermanentOptions = #{
        stream => false,
        events_dispatcher => ldclient_event_dispatch_permanent,
        polling_update_requestor => ldclient_update_requestor_test,
        events_capacity => 100,
        events_shed_threshold => 1000,
        events_flush_interval => 60000
    },
    ldclient:start_instance("", permanent_failing, PermanentOptions),
    DecommissionOptions = #{
        stream => false,
        events_dispatcher => ldclient_event_dispatch_test,
        polling_update_requestor => ldclient_update_requestor_test,
        events_capacity => 100,
        events_shed_threshold => 1000,
        events_flush_interval => 60000,
        events_min_workers => 1,
        events_max_workers => 1
    },
    ldclient:start_instance("sdk-key-events-fail", decommission_test, DecommissionOptions),
    SlowFlushOptions = #{
        stream => false,
        events_dispatcher => ldclient_event_dispatch_slow,
        polling_update_requestor => ldclient_update_requestor_test,
        events_capacity => 100,
        events_shed_threshold => 1000,
        events_flush_interval => 60000,
        events_batch_size => 1,
        events_min_workers => 1,
        events_max_workers => 1
    },
    ldclient:start_instance("", slow_flush, SlowFlushOptions),
    SlowBatchOptions = #{
        stream => false,
        events_dispatcher => ldclient_event_dispatch_slow,
        polling_update_requestor => ldclient_update_requestor_test,
        events_capacity => 100,
        events_shed_threshold => 1000,
        events_flush_interval => 60000,
        events_batch_size => 2,
        events_min_workers => 1,
        events_max_workers => 1
    },
    ldclient:start_instance("", slow_flush_batch, SlowBatchOptions),
    DecommissionMultiOptions = #{
        stream => false,
        events_dispatcher => ldclient_event_dispatch_test,
        polling_update_requestor => ldclient_update_requestor_test,
        events_capacity => 100,
        events_shed_threshold => 1000,
        events_flush_interval => 60000,
        events_min_workers => 1,
        events_max_workers => 1
    },
    ldclient:start_instance("sdk-key-events-fail", decommission_multi, DecommissionMultiOptions),
    InboxOptions = #{
        stream => false,
        events_dispatcher => ldclient_event_dispatch_test,
        polling_update_requestor => ldclient_update_requestor_test,
        events_capacity => 100,
        events_shed_threshold => 1000,
        events_inbox_capacity => 5,
        events_flush_interval => 60000,
        %% keep the scale tick (which also resyncs the queue counter) out of the
        %% test's suspend window
        events_scale_interval_ms => 60000
    },
    ldclient:start_instance("", inbox_bound, InboxOptions),
    SinglePayloadOptions = #{
        stream => false,
        events_dispatcher => ldclient_event_dispatch_test,
        polling_update_requestor => ldclient_update_requestor_test,
        events_capacity => 500,
        events_shed_threshold => 1000,
        events_flush_interval => 60000
    },
    ldclient:start_instance("", single_payload, SinglePayloadOptions),
    CrashOnceOptions = #{
        stream => false,
        events_dispatcher => ldclient_event_dispatch_crash_once,
        polling_update_requestor => ldclient_update_requestor_test,
        events_capacity => 100,
        events_shed_threshold => 1000,
        events_flush_interval => 60000,
        events_scale_interval_ms => 50,
        events_min_workers => 1,
        events_max_workers => 1
    },
    ldclient:start_instance("", crash_once, CrashOnceOptions),
    BadOptions = #{
        stream => false,
        events_dispatcher => ldclient_event_dispatch_test,
        polling_update_requestor => ldclient_update_requestor_test,
        events_capacity => 0,
        events_flush_interval => -1,
        events_min_workers => -1,
        events_max_workers => 0.5,
        events_batch_size => 0,
        events_shed_threshold => 0,
        events_inbox_capacity => foo,
        events_scale_interval_ms => 0,
        events_scale_cooldown_ms => -5,
        events_request_timeout => 0,
        context_keys_capacity => -3
    },
    ldclient:start_instance("", bad_options, BadOptions),
    MaxOnlyOptions = #{
        stream => false,
        events_dispatcher => ldclient_event_dispatch_test,
        polling_update_requestor => ldclient_update_requestor_test,
        events_capacity => 100,
        events_flush_interval => 60000,
        events_max_workers => 2
    },
    ldclient:start_instance("", max_only, MaxOnlyOptions),
    DefaultsOptions = #{
        stream => false,
        events_dispatcher => ldclient_event_dispatch_test,
        polling_update_requestor => ldclient_update_requestor_test,
        events_capacity => 100,
        events_shed_threshold => 1000,
        events_flush_interval => 60000
    },
    ldclient:start_instance("", defaults, DefaultsOptions),
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

%% A flush that finds every worker busy adds workers on demand up to
%% max_workers; once the buffer has drained the pool shrinks back to
%% min_workers.
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
        %% Five batches of one against a 200 ms dispatcher: the first takes the
        %% only worker and the rest find the pool busy, so it grows to max.
        ok = ldclient_event_server:flush(Tag),
        wait_for_worker_count(SupName, 3, 100),
        _ = collect_payloads(5),
        wait_for_worker_count(SupName, 1, 300),
        Directions = collect_scales([]),
        true = lists:member(up, Directions),
        true = lists:member(down, Directions)
    after
        telemetry:detach(HandlerId)
    end.

%% Transient dispatch failures are retried exactly once and reported through
%% telemetry.
retries_transient_failures_once(_) ->
    Tag = failing,
    HandlerId = {?MODULE, retries_transient_failures_once, self()},
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
        telemetry:detach(HandlerId),
        %% This instance fails its sends forever and would keep retrying into
        %% later tests, so stop it once the assertion is done.
        _ = ldclient:stop_instance(failing)
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

%% Offline instances must not buffer or send events, even though ingestion is
%% now a cast handled asynchronously.
offline_instance_does_not_send(_) ->
    Tag = offline_events,
    register_collector(),
    ok = ldclient_event_server:add_event(Tag, identify_event(<<"offline">>), #{}),
    ok = ldclient_event_server:flush(Tag),
    receive
        {EventsBin, _PayloadId} when is_binary(EventsBin) ->
            ct:fail("Offline instance sent events")
    after 500 ->
        ok
    end.

%% Shedding must not lose summary analytics: full-fidelity feature events can be
%% dropped at capacity, but every evaluation still counts toward the summary.
preserves_summary_when_shedding(_) ->
    Tag = summary_shedder,
    register_collector(),
    {_Key, _Json, FlagMap} = ldclient_test_utils:get_simple_flag(),
    Flag = ldclient_flag:new(FlagMap),
    Events = [
        ldclient_event:new_flag_eval(
            5,
            <<"variation-value-5">>,
            <<"default-value">>,
            ldclient_context:new_from_user(#{key => Key}),
            target_match,
            Flag
        )
     || Key <- [<<"s1">>, <<"s2">>, <<"s3">>]
    ],
    [ok = ldclient_event_server:add_event(Tag, E, #{include_reasons => true}) || E <- Events],
    ok = ldclient_event_server:flush(Tag),
    Payloads = collect_payloads(1),
    [Summary|_] = [E || E <- hd(Payloads), maps:get(<<"kind">>, E) =:= <<"summary">>],
    #{<<"features">> := #{<<"abc">> := #{<<"counters">> := [Counter]}}} = Summary,
    3 = maps:get(<<"count">>, Counter).

%% Permanent failures (for example 401/403) are dropped rather than retried
%% forever, matching the previous SDK behaviour and avoiding an unbounded
%% accumulation of retry timers.
permanent_failures_are_not_retried(_) ->
    Tag = permanent_failing,
    HandlerId = {?MODULE, permanent_failures_are_not_retried, self()},
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
        ok = ldclient_event_server:add_event(Tag, identify_event(<<"perm">>), #{}),
        wait_for_event_count(Tag, 1),
        ok = ldclient_event_server:flush(Tag),
        receive
            {send_error, permanent} -> ok
        after 1000 ->
            ct:fail("Expected a permanent send_error telemetry event")
        end,
        _ = collect_payload_with_key(<<"perm">>, 2000),
        %% No retry of this event should be attempted.
        ok = wait_for_no_event(<<"perm">>, 1500)
    after
        telemetry:detach(HandlerId)
    end.

%% A worker that is decommissioned while it has a scheduled retry must not be
%% killed, or the events it is retrying would be lost.
decommission_waits_for_pending_retries(_) ->
    Tag = decommission_test,
    register_collector(),
    try
        ok = ldclient_event_server:add_event(Tag, identify_event(<<"dc">>), #{}),
        wait_for_event_count(Tag, 1),
        ok = ldclient_event_server:flush(Tag),
        SupName = ldclient_event_worker_sup:get_sup_name(Tag),
        [Worker] = [Pid || {_Id, Pid, _Type, _Modules} <- supervisor:which_children(SupName), is_pid(Pid)],
        ok = ldclient_event_process_server:decommission(Worker),
        timer:sleep(100),
        %% The worker is mid-backoff, so it must still be alive.
        true = is_process_alive(Worker),
        %% Once the in-flight retry attempt resolves it must exit, even though
        %% the endpoint keeps failing.
        wait_for_dead(Worker, 3000)
    after
        _ = ldclient:stop_instance(Tag)
    end.

%% A flush only covers the events buffered when it started. Events that arrive
%% while it is dispatching must not extend it, or a flush under sustained
%% evaluations could block indefinitely.
flush_not_extended_by_new_events(_) ->
    Tag = slow_flush,
    register_collector(),
    [ok = ldclient_event_server:add_event(Tag, identify_event(K), #{}) || K <- [<<"f1">>, <<"f2">>, <<"f3">>]],
    Self = self(),
    _Flusher = spawn(fun() ->
        ok = ldclient_event_server:flush(Tag),
        Self ! flush_done
    end),
    wait_until_flushing(Tag),
    %% These arrive after the flush started and belong to the next window.
    [ok = ldclient_event_server:add_event(Tag, identify_event(K), #{}) || K <- [<<"f4">>, <<"f5">>, <<"f6">>]],
    receive
        flush_done -> ok
    after 5000 ->
        ct:fail("Flush was extended by events that arrived after it started")
    end,
    Payloads = collect_payloads(3),
    GotKeys = lists:sort([K || P <- Payloads, #{<<"context">> := #{<<"key">> := K}} <- P]),
    [<<"f1">>, <<"f2">>, <<"f3">>] = GotKeys,
    ok = wait_for_no_event(<<"f4">>, 500).

%% A flush must not pull more than the events captured at the start of the
%% window, even when a batch would otherwise span into events that arrived
%% after the window began.
flush_does_not_overshoot_window(_) ->
    Tag = slow_flush_batch,
    register_collector(),
    [ok = ldclient_event_server:add_event(Tag, identify_event(K), #{}) || K <- [<<"g1">>, <<"g2">>, <<"g3">>]],
    Self = self(),
    _Flusher = spawn(fun() ->
        ok = ldclient_event_server:flush(Tag),
        Self ! flush_done
    end),
    wait_until_flushing(Tag),
    %% These arrive after the window started; the second batch would otherwise
    %% be filled from here.
    [ok = ldclient_event_server:add_event(Tag, identify_event(K), #{}) || K <- [<<"g4">>, <<"g5">>]],
    receive
        flush_done -> ok
    after 5000 ->
        ct:fail("Flush did not complete")
    end,
    Payloads = collect_payloads(2),
    GotKeys = lists:sort([K || P <- Payloads, #{<<"context">> := #{<<"key">> := K}} <- P]),
    [<<"g1">>, <<"g2">>, <<"g3">>] = GotKeys,
    ok = wait_for_no_event(<<"g4">>, 500).

%% The default reporter pool matches the other server SDKs: 5 workers at rest,
%% growing on demand up to 10.
default_pool_bounds(_) ->
    SupName = ldclient_event_worker_sup:get_sup_name(defaults),
    5 = length(supervisor:which_children(SupName)),
    5 = ldclient_config:get_value(defaults, events_min_workers),
    10 = ldclient_config:get_value(defaults, events_max_workers).

%% flush/1 opens the window and returns; it must not wait for the HTTP requests
%% of the window to complete (the pre-pool contract), so a slow endpoint cannot
%% block the caller.
flush_returns_without_waiting_for_delivery(_) ->
    Tag = slow_flush,
    register_collector(),
    [ok = ldclient_event_server:add_event(Tag, identify_event(K), #{}) || K <- [<<"w1">>, <<"w2">>, <<"w3">>]],
    wait_for_event_count(Tag, 3),
    T0 = erlang:monotonic_time(millisecond),
    ok = ldclient_event_server:flush(Tag),
    Elapsed = erlang:monotonic_time(millisecond) - T0,
    %% Three batches of one at 200 ms each would take ~600 ms if flush waited.
    true = Elapsed < 150,
    %% The instance is shared with the window tests, so other keys may be
    %% delivered too; only require that ours arrive.
    [_ = collect_payload_with_key(K, 3000) || K <- [<<"w1">>, <<"w2">>, <<"w3">>]],
    ok.

%% A decommissioned worker holding more than one scheduled retry must attempt
%% every one of them before exiting.
decommission_delivers_all_pending_retries(_) ->
    Tag = decommission_multi,
    register_collector(),
    try
        ok = ldclient_event_server:add_event(Tag, identify_event(<<"m1">>), #{}),
        wait_for_event_count(Tag, 1),
        ok = ldclient_event_server:flush(Tag),
        _ = collect_payload_with_key(<<"m1">>, 2000),
        ok = ldclient_event_server:add_event(Tag, identify_event(<<"m2">>), #{}),
        wait_for_event_count(Tag, 1),
        ok = ldclient_event_server:flush(Tag),
        _ = collect_payload_with_key(<<"m2">>, 2000),
        SupName = ldclient_event_worker_sup:get_sup_name(Tag),
        [Worker] = [Pid || {_Id, Pid, _Type, _Modules} <- supervisor:which_children(SupName), is_pid(Pid)],
        #{pending := 2} = sys:get_state(Worker),
        ok = ldclient_event_process_server:decommission(Worker),
        %% Both retries fire about 1 s after their first attempt, in either
        %% order, so collect two payloads and compare the set of keys.
        Payloads = collect_payloads(2),
        [<<"m1">>, <<"m2">>] = lists:sort([K || P <- Payloads, #{<<"context">> := #{<<"key">> := K}} <- P]),
        wait_for_dead(Worker, 3000)
    after
        _ = ldclient:stop_instance(Tag)
    end.

%% Feature requests are admitted while the event server keeps up, but once the
%% number of queued casts reaches events_inbox_capacity they are shed too, so
%% the mailbox is bounded under ingress overload.
feature_requests_shed_when_inbox_full(_) ->
    Tag = inbox_bound,
    HandlerId = {?MODULE, feature_requests_shed_when_inbox_full, self()},
    Self = self(),
    ok = telemetry:attach(
        HandlerId,
        [ldclient, events, shed],
        fun(_Event, Measurements, Metadata, _Config) ->
            Self ! {shed, Measurements, Metadata}
        end,
        undefined
    ),
    register_collector(),
    ServerName = list_to_atom("ldclient_event_server_" ++ atom_to_list(Tag)),
    try
        {_Key, _Json, FlagMap} = ldclient_test_utils:get_simple_flag(),
        Flag = ldclient_flag:new(FlagMap),
        Eval = fun(Key) ->
            ldclient_event:new_flag_eval(5, <<"v">>, <<"d">>, ldclient_context:new_from_user(#{key => Key}), target_match, Flag)
        end,
        %% Freeze the server so nothing is dequeued while we cast.
        ok = sys:suspend(ServerName),
        [ok = ldclient_event_server:add_event(Tag, Eval(<<"i", (integer_to_binary(N))/binary>>), #{}) || N <- lists:seq(1, 10)],
        {message_queue_len, Queued} = process_info(whereis(ServerName), message_queue_len),
        Shed = count_shed(0),
        ok = sys:resume(ServerName),
        5 = Queued,
        5 = Shed,
        ok = ldclient_event_server:flush(Tag),
        Payloads = collect_payloads(1),
        [Summary|_] = [E || E <- hd(Payloads), maps:get(<<"kind">>, E) =:= <<"summary">>],
        #{<<"features">> := #{<<"abc">> := #{<<"counters">> := [Counter]}}} = Summary,
        5 = maps:get(<<"count">>, Counter)
    after
        telemetry:detach(HandlerId)
    end.

count_shed(Acc) ->
    receive
        {shed, #{count := 1}, #{tag := inbox_bound, kind := feature_request}} -> count_shed(Acc + 1)
    after 100 ->
        Acc
    end.

%% With the default batch size a flush is one request, as before the pool.
flush_sends_one_payload_by_default(_) ->
    Tag = single_payload,
    register_collector(),
    Keys = [<<"sp", (integer_to_binary(N))/binary>> || N <- lists:seq(1, 250)],
    [ok = ldclient_event_server:add_event(Tag, identify_event(K), #{}) || K <- Keys],
    wait_for_event_count(Tag, 250),
    ok = ldclient_event_server:flush(Tag),
    [Payload] = collect_payloads(1),
    250 = length(Payload),
    receive
        {EventsBin, _} when is_binary(EventsBin) -> ct:fail("Flush was split into more than one request")
    after 500 ->
        ok
    end.

%% A batch whose worker crashes mid-send is handed to a replacement worker once
%% instead of being lost.
worker_exit_mid_batch_redispatches(_) ->
    Tag = crash_once,
    register_collector(),
    ok = ldclient_event_server:add_event(Tag, identify_event(<<"crash">>), #{}),
    wait_for_event_count(Tag, 1),
    ok = ldclient_event_server:flush(Tag),
    %% First attempt: the dispatcher forwards the payload and the worker dies.
    {_, PayloadId1} = collect_payload_and_id_with_key(<<"crash">>, 3000),
    %% Second attempt from a replacement worker reuses the same payload id, so
    %% the service can deduplicate if the first request did get through.
    {_, PayloadId2} = collect_payload_and_id_with_key(<<"crash">>, 3000),
    PayloadId1 = PayloadId2,
    SupName = ldclient_event_worker_sup:get_sup_name(Tag),
    wait_for_worker_count(SupName, 1, 100).

%% Invalid pipeline options are replaced by their defaults instead of being
%% passed through (a negative worker count used to spawn workers forever).
invalid_options_fall_back_to_defaults(_) ->
    Tag = bad_options,
    10000 = ldclient_config:get_value(Tag, events_capacity),
    30000 = ldclient_config:get_value(Tag, events_flush_interval),
    5 = ldclient_config:get_value(Tag, events_min_workers),
    10 = ldclient_config:get_value(Tag, events_max_workers),
    10000 = ldclient_config:get_value(Tag, events_batch_size),
    10000 = ldclient_config:get_value(Tag, events_shed_threshold),
    10000 = ldclient_config:get_value(Tag, events_inbox_capacity),
    1000 = ldclient_config:get_value(Tag, events_scale_interval_ms),
    1000 = ldclient_config:get_value(Tag, events_scale_cooldown_ms),
    30000 = ldclient_config:get_value(Tag, events_request_timeout),
    1000 = ldclient_config:get_value(Tag, context_keys_capacity),
    %% Setting only a maximum below the default minimum lowers the minimum.
    2 = ldclient_config:get_value(max_only, events_min_workers),
    2 = ldclient_config:get_value(max_only, events_max_workers),
    2 = length(supervisor:which_children(ldclient_event_worker_sup:get_sup_name(max_only))),
    SupName = ldclient_event_worker_sup:get_sup_name(Tag),
    5 = length(supervisor:which_children(SupName)).

%% Events dropped at capacity are reported with a single telemetry event (and a
%% single log line) per flush window instead of one warning per event.
capacity_drops_reported_once_per_flush(_) ->
    Tag = summary_shedder,
    HandlerId = {?MODULE, capacity_drops_reported_once_per_flush, self()},
    Self = self(),
    ok = telemetry:attach(
        HandlerId,
        [ldclient, events, dropped],
        fun(_Event, Measurements, Metadata, _Config) ->
            Self ! {dropped, Measurements, Metadata}
        end,
        undefined
    ),
    register_collector(),
    try
        %% Feature requests are not shed at the caller (below the inbox bound),
        %% so with capacity 1 their index/feature payloads are dropped inside
        %% the server.
        {_Key, _Json, FlagMap} = ldclient_test_utils:get_simple_flag(),
        Flag = ldclient_flag:new(FlagMap),
        Events = [
            ldclient_event:new_flag_eval(5, <<"v">>, <<"d">>, ldclient_context:new_from_user(#{key => Key}), target_match, Flag)
         || Key <- [<<"d1">>, <<"d2">>, <<"d3">>, <<"d4">>, <<"d5">>]
        ],
        [ok = ldclient_event_server:add_event(Tag, E, #{}) || E <- Events],
        wait_for_event_count(Tag, 1),
        ok = ldclient_event_server:flush(Tag),
        _ = collect_payloads(1),
        receive
            {dropped, #{count := Count}, #{tag := Tag}} when Count >= 1 -> ok
        after 1000 ->
            ct:fail("Expected a dropped telemetry event")
        end,
        receive
            {dropped, _, _} -> ct:fail("Drops were reported more than once for one window")
        after 200 ->
            ok
        end
    after
        telemetry:detach(HandlerId)
    end.

%% An event whose data cannot be encoded as JSON is dropped (and reported) on
%% its own; the rest of the batch and the summary are still delivered.
unencodable_events_do_not_lose_the_batch(_) ->
    Tag = publisher,
    HandlerId = {?MODULE, unencodable_events_do_not_lose_the_batch, self()},
    Self = self(),
    ok = telemetry:attach(
        HandlerId,
        [ldclient, events, dropped],
        fun(_Event, Measurements, Metadata, _Config) ->
            Self ! {dropped, Measurements, Metadata}
        end,
        undefined
    ),
    register_collector(),
    try
        Ctx = ldclient_context:new_from_user(#{key => <<"enc-bad">>}),
        Bad = ldclient_event:new_custom(<<"bad-data">>, Ctx, #{<<"v">> => {not_json, 1}}),
        ok = ldclient_event_server:add_event(Tag, identify_event(<<"enc-ok">>), #{}),
        ok = ldclient_event_server:add_event(Tag, Bad, #{}),
        wait_for_event_count(Tag, 3),
        ok = ldclient_event_server:flush(Tag),
        Payload = collect_payload_with_key(<<"enc-ok">>, 3000),
        [] = [E || #{<<"kind">> := <<"custom">>} = E <- Payload],
        receive
            {dropped, #{count := 1}, #{tag := Tag, reason := unencodable}} -> ok
        after 1000 ->
            ct:fail("Expected a dropped telemetry event for the unencodable custom event")
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

wait_until_flushing(Tag) ->
    ServerName = list_to_atom("ldclient_event_server_" ++ atom_to_list(Tag)),
    wait_until_flushing(ServerName, 200).

wait_until_flushing(_ServerName, 0) ->
    ct:fail("Event server never entered a flush window");
wait_until_flushing(ServerName, Retries) ->
    case sys:get_state(ServerName) of
        #{flushing := true} ->
            ok;
        _ ->
            timer:sleep(5),
            wait_until_flushing(ServerName, Retries - 1)
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

wait_for_dead(_Pid, 0) ->
    ct:fail("Worker did not exit");
wait_for_dead(Pid, Retries) ->
    case is_process_alive(Pid) of
        false ->
            ok;
        true ->
            timer:sleep(25),
            wait_for_dead(Pid, Retries - 1)
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

%% Wait for a payload containing an event for `Key', ignoring payloads from
%% other instances/tests.
collect_payload_with_key(Key, Timeout) ->
    Deadline = erlang:monotonic_time(millisecond) + Timeout,
    collect_payload_with_key(Key, Deadline, Timeout).

collect_payload_with_key(Key, Deadline, Timeout) ->
    Remaining = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {EventsBin, _PayloadId} when is_binary(EventsBin) ->
            Payload = jsx:decode(EventsBin, [return_maps]),
            case lists:any(fun(E) -> event_context_key(E) =:= Key end, Payload) of
                true -> Payload;
                false -> collect_payload_with_key(Key, Deadline, Timeout)
            end
    after Remaining ->
        ct:fail("Did not receive a payload for key ~p within ~bms", [Key, Timeout])
    end.

%% Like collect_payload_with_key/2 but also returns the payload id.
collect_payload_and_id_with_key(Key, Timeout) ->
    Deadline = erlang:monotonic_time(millisecond) + Timeout,
    collect_payload_and_id_with_key(Key, Deadline, Timeout).

collect_payload_and_id_with_key(Key, Deadline, Timeout) ->
    Remaining = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {EventsBin, PayloadId} when is_binary(EventsBin) ->
            Payload = jsx:decode(EventsBin, [return_maps]),
            case lists:any(fun(E) -> event_context_key(E) =:= Key end, Payload) of
                true -> {Payload, PayloadId};
                false -> collect_payload_and_id_with_key(Key, Deadline, Timeout)
            end
    after Remaining ->
        ct:fail("Did not receive a payload for key ~p within ~bms", [Key, Timeout])
    end.

wait_for_no_event(Key, Timeout) ->
    Deadline = erlang:monotonic_time(millisecond) + Timeout,
    wait_for_no_event_loop(Key, Deadline).

wait_for_no_event_loop(Key, Deadline) ->
    Remaining = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {EventsBin, _PayloadId} when is_binary(EventsBin) ->
            Payload = jsx:decode(EventsBin, [return_maps]),
            case lists:any(fun(E) -> event_context_key(E) =:= Key end, Payload) of
                true -> ct:fail("Event ~p was retried", [Key]);
                false -> wait_for_no_event_loop(Key, Deadline)
            end
    after Remaining ->
        ok
    end.

event_context_key(#{<<"context">> := #{<<"key">> := Key}}) -> Key;
event_context_key(_) -> undefined.

receive_events() ->
    receive
        {EventsReceived, PayloadIdReceived} ->
            ActualEvents = jsx:decode(EventsReceived, [return_maps]),
            {ActualEvents, PayloadIdReceived}
    after 2000 ->
        ct:fail("Did not receive events")
    end.
