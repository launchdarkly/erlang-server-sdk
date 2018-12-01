%%-------------------------------------------------------------------
%% @doc Stream server
%%
%% @end
%%-------------------------------------------------------------------

-module(eld_stream_server).

-behaviour(gen_server).

%% Supervision
-export([start_link/1, init/1]).

%% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% API
-export([listen/1]).

-type state() :: #{
    key => atom(),
    conn => pid() | undefined,
    sdk_key => string(),
    storage_backend => atom(),
    storage_tag => atom(),
    stream_uri => string()
}.

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
    ok
    | {error, gun_open_failed, term()}
    | {error, gun_open_timeout, term()}
    | {error, get_req_failed, term()}.
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
    io:format("~nStarting with tag: ~p", [Tag]),
    gen_server:start_link(?MODULE, [Tag], []).

-spec init(Args :: term()) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init([Tag]) ->
    SdkKey = eld_settings:get_value(Tag, sdk_key),
    StreamUri = eld_settings:get_value(Tag, stream_uri),
    StorageBackend = eld_settings:get_value(Tag, storage_backend),
    % Need to trap exit so supervisor:terminate_child calls terminate callback
    process_flag(trap_exit, true),
    State = #{
        conn => undefined,
        sdk_key => SdkKey,
        storage_backend => StorageBackend,
        storage_tag => Tag,
        stream_uri => StreamUri
    },
    {ok, State}.

%%===================================================================
%% Behavior callbacks
%%===================================================================

-type from() :: {pid(), term()}.
-spec handle_call(Request :: term(), From :: from(), State :: state()) ->
    {reply, Reply :: term(), NewState :: state()} |
    {stop, normal, {error, atom(), term()}, state()}.
handle_call({listen}, _From, #{sdk_key := SdkKey, storage_backend := StorageBackend, storage_tag := Tag, stream_uri := Uri} = State) ->
    io:format("~nStarting: ~p", [Uri]),
    {ok, {Scheme, _UserInfo, Host, Port, Path, Query}} = http_uri:parse(Uri),
    case shotgun:open(Host, Port, Scheme) of
        {error, gun_open_failed} ->
            {stop, normal, {error, gun_open_failed, "Could not open connection to host"}, State};
        {error, gun_open_timeout} ->
            {stop, normal, {error, gun_open_timeout, "Connection timeout"}, State};
        {ok, ShotgunPid} ->
            F = fun(nofin, _Ref, Bin) ->
                    process_event(shotgun:parse_event(Bin), StorageBackend, Tag);
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
terminate(Reason, #{conn := undefined} = _State) ->
    io:format("~nTerminating, reason: ~p; Pid none", [Reason]),
    ok;
terminate(Reason, #{conn := ShotgunPid} = _State) ->
    io:format("~nTerminating, reason: ~p; Pid ~p", [Reason, ShotgunPid]),
    ok = shotgun:close(ShotgunPid).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%===================================================================
%% Internal functions
%%===================================================================

%% @doc Processes server-sent event received from shotgun
%%
%% @end
-spec process_event(shotgun:event(), StorageBackend :: atom(), Tag :: atom()) -> ok.
process_event(#{event := Event, data := Data}, StorageBackend, Tag) ->
    % Return data as maps because it's a preferred storage method for flags and segments
    DecodedData = jsx:decode(Data, [return_maps]),
    EventOperation = get_event_operation(Event),
    %io:format("~nReceived event ~p with data ~p", [EventOperation, Data]),
    %io:format("~nReceived event ~p with data ~p", [EventOperation, DecodedData]),
    ok = process_items(EventOperation, DecodedData, StorageBackend, Tag).

-spec get_event_operation(Event :: binary()) -> eld_storage_engine:event_operation().
get_event_operation(<<"put">>) -> put;
get_event_operation(<<"patch">>) -> patch.

%% @doc Process a list of put or patch items
%%
%% @end
-spec process_items(EventOperation :: eld_storage_engine:event_operation(), Data :: map(), StorageBackend :: atom(), Tag :: atom()) -> ok.
process_items(put, Data, StorageBackend, Tag) ->
    [Flags, Segments] = get_put_items(Data),
    io:format("Path is: ~p, Tag is: ~p~n", [<<"/">>, Tag]),
    %io:format("~nSegments are: ~p", [length(Segments)]),
    %io:format("~nFlags are: ~p", [length(Flags)]),
    ok = StorageBackend:put(Tag, flags, Flags),
    ok = StorageBackend:put(Tag, segments, Segments);
process_items(patch, Data, StorageBackend, Tag) ->
    {Bucket, Item} = get_patch_item(Data),
    io:format("~nPatching ~p: ~p", [Bucket, Item]),
    ok = StorageBackend:put(Tag, Bucket, Item).

-spec get_put_items(Data :: map()) -> [map()].
get_put_items(#{<<"path">> := <<"/">>, <<"data">> := #{<<"flags">> := Flags, <<"segments">> := Segments}}) ->
    [Flags, Segments].

-spec get_patch_item(Data :: map()) -> {Bucket :: flags|segments, #{Key :: binary() => map()}}.
get_patch_item(#{<<"path">> := <<"/flags/",FlagKey/binary>>, <<"data">> := FlagMap}) ->
    {flags, #{FlagKey => FlagMap}};
get_patch_item(#{<<"path">> := <<"/segments/",SegmentKey/binary>>, <<"data">> := SegmentMap}) ->
    {segments, #{SegmentKey => SegmentMap}}.
