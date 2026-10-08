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
    flush_while_every_worker_is_busy_is_deferred/1,
    retries_transient_failures_once/1,
    emits_published_telemetry/1,
    offline_instance_does_not_send/1,
    preserves_summary_when_shedding/1,
    permanent_failures_are_not_retried/1,
    flush_not_extended_by_new_events/1,
    flush_does_not_overshoot_window/1,
    flush_returns_without_waiting_for_delivery/1,
    default_pool_size/1,
    feature_requests_shed_when_inbox_full/1,
    flush_sends_one_payload_by_default/1,
    worker_exit_mid_batch_redispatches/1,
    invalid_options_fall_back_to_defaults/1,
    capacity_drops_reported_once_per_flush/1,
    unencodable_events_do_not_lose_the_batch/1,
    emits_flush_telemetry_on_success/1,
    emits_flush_telemetry_once_after_retry/1,
    function_clause_terms_do_not_lose_the_batch/1,
    unencodable_default_keeps_the_summary/1,
    gate_is_closed_while_the_pool_starts/1,
    gate_closes_on_crash_and_counters_are_erased_on_stop/1,
    pool_never_exceeds_its_size/1,
    capacity_drops_are_reported_when_the_server_restarts/1,
    published_counts_only_sent_events/1,
    unencodable_drop_reported_once_across_retry/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        add_event_is_cast,
        sheds_when_buffer_at_threshold,
        pool_uses_multiple_workers,
        flush_while_every_worker_is_busy_is_deferred,
        retries_transient_failures_once,
        emits_published_telemetry,
        offline_instance_does_not_send,
        preserves_summary_when_shedding,
        permanent_failures_are_not_retried,
        flush_not_extended_by_new_events,
        flush_does_not_overshoot_window,
        flush_returns_without_waiting_for_delivery,
        default_pool_size,
        feature_requests_shed_when_inbox_full,
        flush_sends_one_payload_by_default,
        worker_exit_mid_batch_redispatches,
        invalid_options_fall_back_to_defaults,
        capacity_drops_reported_once_per_flush,
        unencodable_events_do_not_lose_the_batch,
        emits_flush_telemetry_on_success,
        emits_flush_telemetry_once_after_retry,
        function_clause_terms_do_not_lose_the_batch,
        unencodable_default_keeps_the_summary,
        gate_is_closed_while_the_pool_starts,
        gate_closes_on_crash_and_counters_are_erased_on_stop,
        pool_never_exceeds_its_size,
        capacity_drops_are_reported_when_the_server_restarts,
        published_counts_only_sent_events,
        unencodable_drop_reported_once_across_retry
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
        events_flush_workers => 3,
        events_batch_size => 1
    },
    ldclient:start_instance("", pooler, PoolOptions),
    BusyPoolOptions = #{
        stream => false,
        events_dispatcher => ldclient_event_dispatch_slow,
        polling_update_requestor => ldclient_update_requestor_test,
        events_capacity => 100,
        events_shed_threshold => 1000,
        events_flush_interval => 60000,
        events_flush_workers => 1,
        events_batch_size => 1
    },
    ldclient:start_instance("", busy_pool, BusyPoolOptions),
    FailingOptions = #{
        stream => false,
        events_dispatcher => ldclient_event_dispatch_test,
        polling_update_requestor => ldclient_update_requestor_test,
        events_capacity => 100,
        events_shed_threshold => 1000,
        events_flush_interval => 60000,
        events_flush_workers => 1
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
    FlushFailingOptions = #{
        stream => false,
        events_dispatcher => ldclient_event_dispatch_test,
        polling_update_requestor => ldclient_update_requestor_test,
        events_capacity => 100,
        events_shed_threshold => 1000,
        events_flush_interval => 60000,
        events_flush_workers => 1
    },
    ldclient:start_instance("sdk-key-events-fail", flush_failing, FlushFailingOptions),
    SlowFlushOptions = #{
        stream => false,
        events_dispatcher => ldclient_event_dispatch_slow,
        polling_update_requestor => ldclient_update_requestor_test,
        events_capacity => 100,
        events_shed_threshold => 1000,
        events_flush_interval => 60000,
        events_batch_size => 1,
        events_flush_workers => 1
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
        events_flush_workers => 1
    },
    ldclient:start_instance("", slow_flush_batch, SlowBatchOptions),
    InboxOptions = #{
        stream => false,
        events_dispatcher => ldclient_event_dispatch_test,
        polling_update_requestor => ldclient_update_requestor_test,
        events_capacity => 100,
        events_shed_threshold => 1000,
        events_inbox_capacity => 5,
        events_flush_interval => 60000,
        %% keep the housekeeping tick (which also resyncs the queue counter) out
        %% of the test's suspend window
        events_housekeeping_interval_ms => 60000
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
        events_housekeeping_interval_ms => 50,
        events_flush_workers => 1
    },
    ldclient:start_instance("", crash_once, CrashOnceOptions),
    BadOptions = #{
        stream => false,
        events_dispatcher => ldclient_event_dispatch_test,
        polling_update_requestor => ldclient_update_requestor_test,
        events_capacity => 0,
        events_flush_interval => 1 bsl 52,
        events_flush_workers => 100000,
        events_batch_size => 0,
        events_shed_threshold => 0,
        events_inbox_capacity => foo,
        events_housekeeping_interval_ms => 0,
        events_request_timeout => 1 bsl 50,
        context_keys_capacity => -3
    },
    ldclient:start_instance("", bad_options, BadOptions),
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

%% The pool has a fixed size. A flush that finds every worker busy is deferred
%% (reported as flush_skipped) and runs as soon as a worker is free, so the
%% events stay buffered meanwhile and no worker is ever added. A flush with
%% nothing to send is a no-op and is not reported.
flush_while_every_worker_is_busy_is_deferred(_) ->
    Tag = busy_pool,
    SupName = ldclient_event_worker_sup:get_sup_name(Tag),
    ServerName = list_to_atom("ldclient_event_server_" ++ atom_to_list(Tag)),
    HandlerId = {?MODULE, flush_while_every_worker_is_busy_is_deferred, self()},
    Self = self(),
    ok = telemetry:attach(
        HandlerId,
        [ldclient, events, flush_skipped],
        fun(_Event, Measurements, Metadata, _Config) ->
            Self ! {flush_skipped, Measurements, Metadata}
        end,
        undefined
    ),
    register_collector(),
    try
        1 = length(supervisor:which_children(SupName)),
        ok = ldclient_event_server:add_event(Tag, identify_event(<<"busy1">>), #{}),
        wait_for_event_count(Tag, 1),
        %% The only worker takes this window (200 ms dispatcher).
        ok = ldclient_event_server:flush(Tag),
        %% Nothing to send: a no-op, not a skip.
        ok = ldclient_event_server:flush(Tag),
        receive
            {flush_skipped, _, _} -> ct:fail("A flush with nothing to send must not be reported as skipped")
        after 100 ->
            ok
        end,
        ok = ldclient_event_server:add_event(Tag, identify_event(<<"busy2">>), #{}),
        wait_for_event_count(Tag, 1),
        %% Every worker is busy: this flush is deferred and busy2 stays buffered.
        ok = ldclient_event_server:flush(Tag),
        receive
            {flush_skipped, #{count := 1}, #{tag := Tag}} -> ok
        after 1000 ->
            ct:fail("Expected the flush to be deferred while the only worker was busy")
        end,
        #{event_count := 1, deferred_flush := true} = sys:get_state(ServerName),
        1 = length(supervisor:which_children(SupName)),
        _ = collect_payload_with_key(<<"busy1">>, 3000),
        %% The worker is free again: the deferred flush runs by itself.
        _ = collect_payload_with_key(<<"busy2">>, 3000),
        #{deferred_flush := false} = sys:get_state(ServerName),
        1 = length(supervisor:which_children(SupName))
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

%% Each delivered batch emits one flush event with count, size and duration, so
%% a recorder can derive flush count, batch size, flush duration and sent
%% metrics from telemetry alone.
emits_flush_telemetry_on_success(_) ->
    Tag = publisher,
    HandlerId = {?MODULE, emits_flush_telemetry_on_success, self()},
    Self = self(),
    ok = telemetry:attach(
        HandlerId,
        [ldclient, events, flush],
        fun(_Event, Measurements, Metadata, _Config) ->
            Self ! {flush_event, Measurements, Metadata}
        end,
        undefined
    ),
    register_collector(),
    try
        Keys = [<<"fl1">>, <<"fl2">>],
        [ok = ldclient_event_server:add_event(Tag, identify_event(K), #{}) || K <- Keys],
        wait_for_event_count(Tag, 2),
        ok = ldclient_event_server:flush(Tag),
        _ = collect_payloads(1),
        receive
            {flush_event, Measurements, #{tag := Tag, outcome := accepted}} ->
                2 = maps:get(count, Measurements),
                true = (maps:get(size, Measurements) > 0),
                true = (maps:get(duration, Measurements) >= 0)
        after 1000 ->
            ct:fail("Expected an accepted [ldclient, events, flush] telemetry event")
        end
    after
        telemetry:detach(HandlerId)
    end.

%% A batch that fails after its retry emits exactly one flush event, whose
%% duration covers both attempts.
emits_flush_telemetry_once_after_retry(_) ->
    Tag = flush_failing,
    HandlerId = {?MODULE, emits_flush_telemetry_once_after_retry, self()},
    Self = self(),
    ok = telemetry:attach(
        HandlerId,
        [ldclient, events, flush],
        fun(_Event, Measurements, Metadata, _Config) ->
            Self ! {flush_event, Measurements, Metadata}
        end,
        undefined
    ),
    register_collector(),
    try
        ok = ldclient_event_server:add_event(Tag, identify_event(<<"flfail">>), #{}),
        wait_for_event_count(Tag, 1),
        ok = ldclient_event_server:flush(Tag),
        %% Two dispatch attempts, but only one flush event for the batch.
        _ = collect_payloads(2),
        receive
            {flush_event, Measurements, #{tag := Tag, outcome := failed}} ->
                1 = maps:get(count, Measurements),
                true = (maps:get(duration, Measurements) >= 0)
        after 2000 ->
            ct:fail("Expected a failed [ldclient, events, flush] telemetry event")
        end,
        receive
            {flush_event, _M, #{tag := Tag}} ->
                ct:fail("flush event emitted more than once for a batch")
        after 500 ->
            ok
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

%% The default reporter pool matches the other server SDKs: 5 workers, fixed.
default_pool_size(_) ->
    SupName = ldclient_event_worker_sup:get_sup_name(defaults),
    5 = length(supervisor:which_children(SupName)),
    5 = ldclient_config:get_value(defaults, events_flush_workers).

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
        %% Five casts were admitted; a flush or housekeeping timer may also be queued.
        true = Queued >= 5,
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
    5 = ldclient_config:get_value(Tag, events_flush_workers),
    10000 = ldclient_config:get_value(Tag, events_batch_size),
    10000 = ldclient_config:get_value(Tag, events_shed_threshold),
    10000 = ldclient_config:get_value(Tag, events_inbox_capacity),
    1000 = ldclient_config:get_value(Tag, events_housekeeping_interval_ms),
    30000 = ldclient_config:get_value(Tag, events_request_timeout),
    1000 = ldclient_config:get_value(Tag, context_keys_capacity),
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

%% jsx raises function_clause, not badarg, for a map whose key is a string, a
%% float or a tuple, and for an improper list. Those must be dropped per event
%% exactly like badarg terms.
function_clause_terms_do_not_lose_the_batch(_) ->
    Tag = publisher,
    HandlerId = {?MODULE, function_clause_terms_do_not_lose_the_batch, self()},
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
        Ctx = ldclient_context:new_from_user(#{key => <<"fc-bad">>}),
        Bad = ldclient_event:new_custom(<<"bad-data">>, Ctx, #{"plan" => <<"pro">>}),
        ok = ldclient_event_server:add_event(Tag, identify_event(<<"fc-ok">>), #{}),
        ok = ldclient_event_server:add_event(Tag, Bad, #{}),
        wait_for_event_count(Tag, 3),
        ok = ldclient_event_server:flush(Tag),
        Payload = collect_payload_with_key(<<"fc-ok">>, 3000),
        [] = [E || #{<<"kind">> := <<"custom">>} = E <- Payload],
        receive
            {dropped, #{count := 1}, #{tag := Tag, reason := unencodable}} -> ok
        after 1000 ->
            ct:fail("Expected a dropped telemetry event for the unencodable custom event")
        end
    after
        telemetry:detach(HandlerId)
    end.

%% The summary is one output event for every flag in the window. An
%% application-supplied default that is not JSON must not take all of those
%% counters with it: it is sent as null instead.
unencodable_default_keeps_the_summary(_) ->
    Tag = publisher,
    HandlerId = {?MODULE, unencodable_default_keeps_the_summary, self()},
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
        Ctx = ldclient_context:new_from_user(#{key => <<"def-ok">>}),
        BadDefault = #{"a" => 1},
        Eval = ldclient_event:new_for_unknown_flag(<<"abc">>, Ctx, BadDefault, {error, flag_not_found}),
        ok = ldclient_event_server:add_event(Tag, Eval, #{}),
        ok = ldclient_event_server:add_event(Tag, identify_event(<<"def-ok">>), #{}),
        wait_for_event_count(Tag, 2),
        ok = ldclient_event_server:flush(Tag),
        Payload = collect_payload_with_key(<<"def-ok">>, 3000),
        [#{<<"features">> := #{<<"abc">> := Feature}}] = [E || #{<<"kind">> := <<"summary">>} = E <- Payload],
        #{<<"default">> := null, <<"counters">> := [#{<<"value">> := null, <<"count">> := 1}]} = Feature,
        receive
            {dropped, _, #{reason := unencodable}} -> ct:fail("Nothing should have been dropped")
        after 300 ->
            ok
        end
    after
        telemetry:detach(HandlerId)
    end.

%% The server is registered before init/1 runs, and callers admit without
%% counting while no counters are published. The counters are therefore
%% published first with the gate closed, so a cast arriving while the pool
%% starts is shed instead of queueing uncounted; the gate opens once a worker
%% exists.
gate_is_closed_while_the_pool_starts(_) ->
    Tag = slow_init,
    Key = {ldclient_event_server_counters, Tag},
    HandlerId = {?MODULE, gate_is_closed_while_the_pool_starts, self()},
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
    _Starter = spawn_link(fun() ->
        Options = instance_options(#{events_dispatcher => ldclient_event_dispatch_slow_init, events_inbox_capacity => 50}),
        Self ! {started, ldclient:start_instance("", Tag, Options)}
    end),
    try
        {Ref, _Threshold, 50} = wait_for_counters(Key, 300),
        50 = counters:get(Ref, 2),
        ok = ldclient_event_server:add_event(Tag, identify_event(<<"during-init">>), #{}),
        receive
            {shed, #{count := 1}, #{tag := Tag, kind := identify}} -> ok
        after 1000 ->
            ct:fail("Expected the cast made during init to be shed")
        end,
        receive
            {started, ok} -> ok
        after 5000 ->
            ct:fail("start_instance did not complete")
        end,
        true = counters:get(Ref, 2) < 50,
        ok = ldclient_event_server:add_event(Tag, identify_event(<<"after-init">>), #{}),
        wait_for_event_count(Tag, 1),
        ok = ldclient_event_server:flush(Tag),
        _ = collect_payload_with_key(<<"after-init">>, 3000)
    after
        telemetry:detach(HandlerId),
        _ = (catch ldclient:stop_instance(Tag))
    end.

gate_closes_on_crash_and_counters_are_erased_on_stop(_) ->
    Tag = gate,
    Key = {ldclient_event_server_counters, Tag},
    ServerName = list_to_atom("ldclient_event_server_" ++ atom_to_list(Tag)),
    ok = ldclient:start_instance("", Tag, instance_options(#{events_inbox_capacity => 100, events_housekeeping_interval_ms => 50})),
    register_collector(),
    try
        {Ref1, _, 100} = persistent_term:get(Key),
        Pid1 = whereis(ServerName),
        %% An event without a context crashes the handler.
        ok = gen_server:cast(ServerName, {add_event, #{type => identify}, Tag, #{}}),
        _Pid2 = wait_new_pid(ServerName, Pid1, 300),
        %% The crashed incarnation left its gate closed, so callers holding the
        %% old counters shed; the new incarnation published fresh, open ones.
        100 = counters:get(Ref1, 2),
        {Ref2, _, 100} = persistent_term:get(Key),
        true = Ref1 =/= Ref2,
        true = counters:get(Ref2, 2) < 100,
        %% Reservations leaked by callers that died between their add and their
        %% cast are clamped back to the real mailbox length on the next tick.
        counters:add(Ref2, 2, 7),
        timer:sleep(200),
        true = counters:get(Ref2, 2) =< 1,
        ok = ldclient_event_server:add_event(Tag, identify_event(<<"gate-user">>), #{}),
        wait_for_event_count(Tag, 1),
        ok = ldclient_event_server:flush(Tag),
        _ = collect_payload_with_key(<<"gate-user">>, 3000)
    after
        ok = ldclient:stop_instance(Tag)
    end,
    undefined = persistent_term:get(Key, undefined).

%% The pool never has more live workers than events_flush_workers, even while
%% workers are busy with slow requests and holding retries.
pool_never_exceeds_its_size(_) ->
    Tag = pool_bound,
    Size = 2,
    ok = ldclient:start_instance("", Tag, instance_options(#{
        events_dispatcher => ldclient_event_dispatch_slow_fail,
        events_batch_size => 1,
        events_flush_workers => Size,
        events_housekeeping_interval_ms => 20
    })),
    register_collector(),
    SupName = ldclient_event_worker_sup:get_sup_name(Tag),
    Self = self(),
    Sampler = spawn_link(fun() -> live_sampler(SupName, 0, Self) end),
    try
        lists:foreach(fun(I) ->
            ok = ldclient_event_server:add_event(Tag, identify_event(<<"wa", (integer_to_binary(I))/binary>>), #{}),
            wait_for_event_count(Tag, 1),
            ok = ldclient_event_server:flush(Tag),
            timer:sleep(10),
            ok = ldclient_event_server:add_event(Tag, identify_event(<<"wb", (integer_to_binary(I))/binary>>), #{}),
            timer:sleep(10),
            ok = ldclient_event_server:flush(Tag),
            timer:sleep(700)
        end, lists:seq(1, 4)),
        Sampler ! {stop, self()},
        MaxLive = receive {live, Live} -> Live after 2000 -> ct:fail("sampler did not report") end,
        ct:pal("events_flush_workers=~b; most live worker processes observed: ~b", [Size, MaxLive]),
        true = MaxLive =< Size
    after
        ok = ldclient:stop_instance(Tag)
    end.

%% Drops are reported once per flush window. A window that ends with the
%% server crashing instead of flushing must still report them.
capacity_drops_are_reported_when_the_server_restarts(_) ->
    Tag = drop_restart,
    ServerName = list_to_atom("ldclient_event_server_" ++ atom_to_list(Tag)),
    HandlerId = {?MODULE, capacity_drops_are_reported_when_the_server_restarts, self()},
    Self = self(),
    ok = telemetry:attach(
        HandlerId,
        [ldclient, events, dropped],
        fun(_Event, Measurements, Metadata, _Config) ->
            Self ! {dropped, Measurements, Metadata}
        end,
        undefined
    ),
    ok = ldclient:start_instance("", Tag, instance_options(#{events_capacity => 2, events_housekeeping_interval_ms => 50})),
    try
        {_Key, _Json, FlagMap} = ldclient_test_utils:get_simple_flag(),
        Flag = ldclient_flag:new(FlagMap),
        lists:foreach(fun(I) ->
            Ctx = ldclient_context:new_from_user(#{key => <<"x", (integer_to_binary(I))/binary>>}),
            ok = ldclient_event_server:add_event(Tag, ldclient_event:new_flag_eval(5, <<"v">>, <<"d">>, Ctx, target_match, Flag), #{})
        end, lists:seq(1, 10)),
        ok = wait_until(fun() -> maps:get(dropped, sys:get_state(ServerName)) > 0 end, 100),
        Dropped = maps:get(dropped, sys:get_state(ServerName)),
        Pid1 = whereis(ServerName),
        ok = gen_server:cast(ServerName, {add_event, #{type => identify}, Tag, #{}}),
        _ = wait_new_pid(ServerName, Pid1, 300),
        receive
            {dropped, #{count := Dropped}, #{tag := Tag, reason := capacity}} -> ok
        after 1000 ->
            ct:fail("~b drops counted before the restart were never reported", [Dropped])
        end
    after
        telemetry:detach(HandlerId),
        ok = ldclient:stop_instance(Tag)
    end.

%% `published` counts the events that were actually in the request, after
%% unencodable ones were dropped; a batch with nothing left to send publishes
%% nothing.
published_counts_only_sent_events(_) ->
    Tag = publisher,
    HandlerId = {?MODULE, published_counts_only_sent_events, self()},
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
        Ctx = ldclient_context:new_from_user(#{key => <<"pub-bad">>}),
        Bad = ldclient_event:new_custom(<<"bad-data">>, Ctx, #{<<"v">> => {not_json, 1}}),
        ok = ldclient_event_server:add_event(Tag, identify_event(<<"pub-ok">>), #{}),
        ok = ldclient_event_server:add_event(Tag, Bad, #{}),
        %% identify + custom + the custom event's index event
        wait_for_event_count(Tag, 3),
        ok = ldclient_event_server:flush(Tag),
        Payload = collect_payload_with_key(<<"pub-ok">>, 3000),
        Sent = length(Payload),
        receive
            {published, #{count := Sent}, #{tag := Tag}} -> ok
        after 1000 ->
            ct:fail("Expected a published count of ~b", [Sent])
        end,
        %% The context has been seen, so this batch holds only the bad event.
        ok = ldclient_event_server:add_event(Tag, Bad, #{}),
        wait_for_event_count(Tag, 1),
        ok = ldclient_event_server:flush(Tag),
        receive
            {published, Measurements, _} -> ct:fail("Nothing was sent, but published reported ~p", [Measurements])
        after 500 ->
            ok
        end
    after
        telemetry:detach(HandlerId)
    end.

%% The retry resends the bytes of the first attempt, so an unencodable event
%% is dropped and reported exactly once per batch.
unencodable_drop_reported_once_across_retry(_) ->
    Tag = retry_once,
    HandlerId = {?MODULE, unencodable_drop_reported_once_across_retry, self()},
    Self = self(),
    ok = telemetry:attach(
        HandlerId,
        [ldclient, events, dropped],
        fun(_Event, Measurements, Metadata, _Config) ->
            Self ! {dropped, Measurements, Metadata}
        end,
        undefined
    ),
    ok = ldclient:start_instance("sdk-key-events-fail", Tag, instance_options(#{})),
    register_collector(),
    try
        Ctx = ldclient_context:new_from_user(#{key => <<"retry-bad">>}),
        Bad = ldclient_event:new_custom(<<"bad-data">>, Ctx, #{"plan" => <<"pro">>}),
        ok = ldclient_event_server:add_event(Tag, identify_event(<<"retry-ok">>), #{}),
        ok = ldclient_event_server:add_event(Tag, Bad, #{}),
        wait_for_event_count(Tag, 3),
        ok = ldclient_event_server:flush(Tag),
        %% Both the attempt and the retry (one second later) fail.
        _ = collect_payload_with_key(<<"retry-ok">>, 3000),
        _ = collect_payload_with_key(<<"retry-ok">>, 3000),
        timer:sleep(200),
        1 = count_dropped_reports(0)
    after
        telemetry:detach(HandlerId),
        ok = ldclient:stop_instance(Tag)
    end.

count_dropped_reports(Acc) ->
    receive
        {dropped, _, #{reason := unencodable}} -> count_dropped_reports(Acc + 1)
    after 0 ->
        Acc
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

instance_options(Extra) ->
    maps:merge(#{
        stream => false,
        events_dispatcher => ldclient_event_dispatch_test,
        polling_update_requestor => ldclient_update_requestor_test,
        events_capacity => 100,
        events_shed_threshold => 1000,
        events_inbox_capacity => 1000,
        events_flush_interval => 60000,
        events_flush_workers => 1
    }, Extra).

wait_for_counters(_Key, 0) ->
    ct:fail("The event server did not publish its counters");
wait_for_counters(Key, Retries) ->
    case persistent_term:get(Key, undefined) of
        undefined -> timer:sleep(10), wait_for_counters(Key, Retries - 1);
        Counters -> Counters
    end.

wait_new_pid(_Name, _Old, 0) ->
    ct:fail("The event server was not restarted");
wait_new_pid(Name, Old, Retries) ->
    case whereis(Name) of
        Pid when is_pid(Pid), Pid =/= Old -> Pid;
        _ -> timer:sleep(10), wait_new_pid(Name, Old, Retries - 1)
    end.

wait_until(_Fun, 0) ->
    ct:fail("Condition not met in time");
wait_until(Fun, Retries) ->
    case Fun() of
        true -> ok;
        false -> timer:sleep(10), wait_until(Fun, Retries - 1)
    end.

live_sampler(SupName, MaxLive, Parent) ->
    receive
        {stop, From} -> From ! {live, MaxLive}
    after 5 ->
        Live = proplists:get_value(active, supervisor:count_children(SupName)),
        live_sampler(SupName, max(MaxLive, Live), Parent)
    end.
