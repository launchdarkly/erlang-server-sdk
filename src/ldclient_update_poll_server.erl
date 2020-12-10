%%-------------------------------------------------------------------
%% @doc Polling server
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_update_poll_server).

-behaviour(gen_server).

%% Supervision
-export([start_link/1, init/1]).

%% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-type state() :: #{
    sdk_key := string(),
    storage_backend := atom(),
    storage_tag := atom(),
    requestor := atom(),
    requestor_state := any(),
    poll_uri := string(),
    poll_interval := pos_integer(),
    timer_ref => timer:tref()
}.

%% Constants
-define(LATEST_ALL_PATH, "/sdk/latest-all").

-ifdef(TEST).
-compile(export_all).
-endif.

%%===================================================================
%% Supervision
%%===================================================================

%% @doc Starts the server
%%
%% @end
-spec start_link(Tag :: atom()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Tag) ->
    error_logger:info_msg("Starting polling update server for ~p", [Tag]),
    gen_server:start_link(?MODULE, [Tag], []).

-spec init(Args :: term()) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init([Tag]) ->
    SdkKey = ldclient_settings:get_value(Tag, sdk_key),
    StorageBackend = ldclient_settings:get_value(Tag, storage_backend),
    Requestor = ldclient_settings:get_value(Tag, polling_update_requestor),
    PollUri = ldclient_settings:get_value(Tag, base_uri) ++ ?LATEST_ALL_PATH,
    PollInterval = ldclient_settings:get_value(Tag, polling_interval) * 1000,
    % Need to trap exit so supervisor:terminate_child calls terminate callback
    process_flag(trap_exit, true),
    State = #{
        sdk_key => SdkKey,
        storage_backend => StorageBackend,
        storage_tag => Tag,
        requestor => Requestor,
        requestor_state => Requestor:init(),
        poll_uri => PollUri,
        poll_interval => PollInterval
    },
    self() ! {listen},
    {ok, State}.

%%===================================================================
%% Behavior callbacks
%%===================================================================

-type from() :: {pid(), term()}.
-spec handle_call(Request :: term(), From :: from(), State :: state()) ->
    {reply, Reply :: term(), NewState :: state()} |
    {stop, normal, {error, atom(), term()}, state()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({listen}, #{poll_interval := PollInterval} = State) ->
    case maps:find(timer_ref, State) of
        {ok, _ExistingTimer} ->
            {noreply, ok, State};
        error ->
            {ok, TimerRef} = timer:send_interval(PollInterval, {poll}),
            {noreply, poll(State#{timer_ref => TimerRef})}
    end;
handle_info({poll}, State) ->
    {noreply, poll(State)};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: state()) -> term().
terminate(Reason, #{timer_ref := TimerRef} = _State) ->
    error_logger:info_msg("Terminating polling, reason: ~p", [Reason]),
    _ = timer:cancel(TimerRef),
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%===================================================================
%% Internal functions
%%===================================================================

-spec poll(state()) -> state().
poll(#{ sdk_key := SdkKey,
        storage_backend := StorageBackend,
        requestor := Requestor,
        requestor_state := RequestorState,
        poll_uri := Uri,
        storage_tag := Tag } = State) ->
    {Result, NewRequestorState} = Requestor:all(Uri, SdkKey, RequestorState),
    ok = process_response(Result, StorageBackend, Tag, Uri),
    true = ldclient_update_processor_state:set_initialized_state(Tag, true),
    State#{requestor_state := NewRequestorState}.

-spec process_response(ldclient_update_requestor:response(), atom(), atom(), string()) -> ok.
process_response({error, {bad_status, 401, _Reason}}, _, _, Uri) ->
    error_logger:warning_msg("Invalid SDK key when when polling for updates at URL ~p. Verify that your SDK key is correct.", [Uri]),
    ok;
process_response({error, {bad_status, 404, _Reason}}, _, _, Uri) ->
    error_logger:warning_msg("Resource not found when polling for updates at URL ~p.", [Uri]),
    ok;
process_response({error, {bad_status, StatusCode, Reason}}, _, _, Uri) when StatusCode >= 300 ->
    error_logger:warning_msg("Unexpected response code: ~p when polling for updates at URL ~p: ~p.", [StatusCode, Uri, Reason]),
    ok;
process_response({error, network_error}, _, _, Uri) ->
    error_logger:warning_msg("Failed to connect to update server at: %p", [Uri]),
    ok;
process_response({ok, not_modified}, _, _, _) -> ok;
process_response({ok, ResponseBody}, StorageBackend, Tag, _) ->
    StorageDown = ldclient_update_processor_state:get_storage_initialized_state(Tag),
    case StorageDown of
        false -> ldclient_update_processor_state:set_storage_initialized_state(Tag, reload);
        _ -> ok
    end,
    process_response_body(ResponseBody, StorageBackend, Tag).

-spec process_response_body(binary(), atom(), atom()) -> ok.
process_response_body(ResponseBody, StorageBackend, Tag) ->
    Data = jsx:decode(ResponseBody, [return_maps]),
    [Flags, Segments] = get_put_items(Data),
    ok = StorageBackend:put_clean(Tag, flags, Flags),
    ok = StorageBackend:put_clean(Tag, segments, Segments),
    ok.

-spec get_put_items(Data :: map()) -> [map()].
get_put_items(#{<<"flags">> := Flags, <<"segments">> := Segments}) ->
    [Flags, Segments].
