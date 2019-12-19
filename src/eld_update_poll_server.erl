%%-------------------------------------------------------------------
%% @doc Polling server
%%
%% @end
%%-------------------------------------------------------------------

-module(eld_update_poll_server).

-behaviour(gen_server).
-behaviour(eld_update_server).

%% Supervision
-export([start_link/1, init/1]).

%% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% API
-export([listen/1]).

-type state() :: #{
    sdk_key := string(),
    storage_backend := atom(),
    storage_tag := atom(),
    requestor := atom(),
    poll_uri := string(),
    poll_interval := pos_integer(),
    last_response_hash := binary(),
    timer_ref => reference()
}.

%% Constants
-define(LATEST_ALL_PATH, "/sdk/latest-all").

-ifdef(TEST).
-compile(export_all).
-endif.

%%===================================================================
%% API
%%===================================================================

%% @doc Start listening to streaming events
%%
%% @end
-spec listen(Pid :: pid()) ->
    ok.
listen(Pid) ->
    gen_server:call(Pid, {listen}).

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
    SdkKey = eld_settings:get_value(Tag, sdk_key),
    StorageBackend = eld_settings:get_value(Tag, storage_backend),
    Requestor = eld_settings:get_value(Tag, polling_update_requestor),
    PollUri = eld_settings:get_value(Tag, base_uri) ++ ?LATEST_ALL_PATH,
    PollInterval = eld_settings:get_value(Tag, polling_interval) * 1000,
    % Need to trap exit so supervisor:terminate_child calls terminate callback
    process_flag(trap_exit, true),
    State = #{
        sdk_key => SdkKey,
        storage_backend => StorageBackend,
        storage_tag => Tag,
        requestor => Requestor,
        poll_uri => PollUri,
        poll_interval => PollInterval,
        last_response_hash => <<>>
    },
    {ok, State}.

%%===================================================================
%% Behavior callbacks
%%===================================================================

-type from() :: {pid(), term()}.
-spec handle_call(Request :: term(), From :: from(), State :: state()) ->
    {reply, Reply :: term(), NewState :: state()} |
    {stop, normal, {error, atom(), term()}, state()}.
handle_call({listen}, _From,
    #{
        sdk_key := SdkKey,
        storage_backend := StorageBackend,
        storage_tag := Tag,
        requestor := Requestor,
        poll_uri := Uri,
        poll_interval := PollInterval,
        last_response_hash := LastHash
    } = State) ->
    {ok, NewHash} = poll(SdkKey, StorageBackend, Requestor, Uri, LastHash, Tag),
    TimerRef = erlang:send_after(PollInterval, self(), {poll}),
    NewState = State#{timer_ref => TimerRef, last_response_hash := NewHash},
    {reply, ok, NewState}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({poll},
    #{
        sdk_key := SdkKey,
        storage_backend := StorageBackend,
        storage_tag := Tag,
        requestor := Requestor,
        poll_uri := Uri,
        poll_interval := PollInterval,
        last_response_hash := LastHash
    } = State) ->
    {ok, NewHash} = poll(SdkKey, StorageBackend, Requestor, Uri, LastHash, Tag),
    TimerRef = erlang:send_after(PollInterval, self(), {poll}),
    {noreply, State#{timer_ref := TimerRef, last_response_hash := NewHash}};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: state()) -> term().
terminate(Reason, #{timer_ref := TimerRef} = _State) ->
    error_logger:info_msg("Terminating polling, reason: ~p", [Reason]),
    _ = erlang:cancel_timer(TimerRef),
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%===================================================================
%% Internal functions
%%===================================================================

-spec poll(string(), atom(), atom(), string(), binary(), atom()) -> {ok, binary()}.
poll(SdkKey, StorageBackend, Requestor, Uri, LastHash, Tag) ->
    process_response(Requestor:all(Uri, SdkKey), StorageBackend, LastHash, Tag, Uri).

-spec process_response(eld_update_requestor:response(), atom(), binary(), atom(), string()) -> {ok, binary()}.
process_response({error, 401, _Reason}, _, LastHash, _, Uri) ->
    error_logger:warning_msg("Invalid SDK key when when polling for updates at URL ~p. Verify that your SDK key is correct.", [Uri]),
    {ok, LastHash};
process_response({error, 404, _Reason}, _, LastHash, _, Uri) ->
    error_logger:warning_msg("Resource not found when polling for updates at URL ~p.", [Uri]),
    {ok, LastHash};
process_response({error, StatusCode, Reason}, _, LastHash, _, Uri) when StatusCode >= 300 ->
    error_logger:warning_msg("Unexpected response code: ~p when polling for updates at URL ~p: ~p.", [StatusCode, Uri, Reason]),
    {ok, LastHash};
process_response({ok, ResponseBody}, StorageBackend, LastHash, Tag, _) ->
    NewHash = crypto:hash(sha, ResponseBody),
    process_response_body_last_hash(ResponseBody, StorageBackend, LastHash, NewHash, Tag).

-spec process_response_body_last_hash(binary(), atom(), binary(), binary(), atom()) -> {ok, binary()}.
process_response_body_last_hash(_ResponseBody, _StorageBackend, Hash, Hash, _Tag) -> {ok, Hash};
process_response_body_last_hash(ResponseBody, StorageBackend, _, NewHash, Tag) ->
    process_response_body(ResponseBody, StorageBackend, NewHash, Tag).

-spec process_response_body(binary(), atom(), binary(), atom()) -> {ok, binary()}.
process_response_body(ResponseBody, StorageBackend, NewHash, Tag) ->
    Data = jsx:decode(ResponseBody, [return_maps]),
    [Flags, Segments] = get_put_items(Data),
    ok = StorageBackend:put(Tag, flags, Flags),
    ok = StorageBackend:put(Tag, segments, Segments),
    {ok, NewHash}.

-spec get_put_items(Data :: map()) -> [map()].
get_put_items(#{<<"flags">> := Flags, <<"segments">> := Segments}) ->
    [Flags, Segments].
