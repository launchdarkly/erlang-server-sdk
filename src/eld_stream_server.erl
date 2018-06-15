%%%-------------------------------------------------------------------
%%% @doc Stream server
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(eld_stream_server).

-behaviour(gen_server).

%% Supervision
-export([start_link/1, init/1]).

%% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% API
-export([listen/2]).

-type state() :: #{conn => pid() | undefined, sdkkey => string()}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start listening to streaming events
%%
%% @end
-spec listen(Pid :: pid(), Uri :: http_uri:uri()) -> ok | {error, atom(), term()}.
listen(Pid, Uri) ->
    gen_server:call(Pid, {listen, Uri}).

%% @doc Starts the server
%%
%% @end
-spec start_link(SdkKey :: string()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(SdkKey) ->
    gen_server:start_link(?MODULE, [SdkKey], []).

-spec init(Args :: term()) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init([SdkKey]) ->
    % Need to trap exit so supervisor:terminate_child calls terminate callback
    process_flag(trap_exit, true),
    {ok, #{conn => undefined, sdkkey => SdkKey}}.

%%%===================================================================
%%% Behavior callbacks
%%%===================================================================

-type from() :: {pid(), term()}.
-spec handle_call(Request :: term(), From :: from(), State :: state()) ->
    {reply, Reply :: term(), NewState :: state()} |
    {stop, normal, {error, atom(), term()}, state()}.
handle_call({listen, Uri}, _From, #{sdkkey := SdkKey} = State) ->
    io:format("~nStarting: ~p", [Uri]),
    {ok, {Scheme, _UserInfo, Host, Port, Path, Query}} = http_uri:parse(Uri),
    case shotgun:open(Host, Port, Scheme) of
        {error, gun_open_failed} ->
            {stop, normal, {error, gun_open_failed, "Could not open connection to host"}, State};
        {error, gun_open_timeout} ->
            {stop, normal, {error, gun_open_timeout, "Connection timeout"}, State};
        {ok, ShotgunPid} ->
            F = fun(nofin, _Ref, Bin) ->
                    process_event(shotgun:parse_event(Bin));
                (fin, _Ref, Bin) ->
                    % TODO need to do something here
                    io:format("~nGot a fin message with data ~p", [Bin])
                end,
            Options = #{async => true, async_mode => sse, handle_event => F},
            case shotgun:get(ShotgunPid, Path ++ Query, #{"Authorization" => SdkKey}, Options) of
                {error, Reason} ->
                    {stop, normal, {error, get_req_failed, Reason}};
                {ok, _Ref} ->
                    {reply, ok, State#{conn := ShotgunPid}}
            end
    end.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: state()) -> term().
terminate(Reason, #{conn := ShotgunPid} = _State) ->
    io:format("~nTerminating, reason: ~p; Pid ~p", [Reason, ShotgunPid]),
    ok = shotgun:close(ShotgunPid).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Processes server-sent event received from shotgun
%%
%% @end
-spec process_event(shotgun:event()) -> ok.
process_event(#{event := Event, data := Data}) ->
    % Return data as maps because it's a preferred storage method for flags and segments
    DecodedData = jsx:decode(Data, [return_maps]),
    EventOperation = get_event_operation(Event),
    io:format("~nReceived event ~p with data ~p", [EventOperation, DecodedData]),
    ok = eld_storage_server:process_events(EventOperation, [DecodedData]).

-spec get_event_operation(Event :: binary()) -> eld_storage_server:event_operation().
get_event_operation(<<"put">>) -> put;
get_event_operation(<<"patch">>) -> patch.
