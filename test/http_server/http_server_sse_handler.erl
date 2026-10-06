-module(http_server_sse_handler).

%% cowboy
-export([init/2]).
-export([info/3]).

%% test helpers
-export([reset_connections/1]).

-define(HEARTBEAT_INTERVAL_MS, 250).

%% cowboy

init(Req0, Opts) ->
    Authorization = cowboy_req:header(<<"authorization">>, Req0, <<>>),
    Connection = count_connection(Authorization),
    ok = notify_connected(Authorization),
    PutData = sse_data(Authorization, Connection),
    Req = cowboy_req:stream_reply(200, #{<<"content-type">> => <<"text/event-stream">>}, Req0),
    cowboy_req:stream_events(#{
        event => <<"put">>,
        data => PutData
    }, nofin, Req),
    ok = maybe_start_heartbeats(Authorization),
    {cowboy_loop, Req, Opts}.

info(heartbeat, Req, State) ->
    ok = cowboy_req:stream_body(<<":\n">>, nofin, Req),
    _ = erlang:send_after(?HEARTBEAT_INTERVAL_MS, self(), heartbeat),
    {ok, Req, State};
info(_Msg, Req, State) ->
    {ok, Req, State}.

%% test helpers

reset_connections(SdkKey) ->
    _ = persistent_term:erase({?MODULE, connections, SdkKey}),
    ok.

%% internal

count_connection(SdkKey) ->
    Key = {?MODULE, connections, SdkKey},
    Connection = persistent_term:get(Key, 0) + 1,
    persistent_term:put(Key, Connection),
    Connection.

%% The read-timeout tests register themselves to observe each connection.
notify_connected(SdkKey) when SdkKey =:= <<"sdk-read-timeout">>; SdkKey =:= <<"sdk-heartbeat">> ->
    case whereis(read_timeout_test) of
        undefined -> ok;
        Pid -> Pid ! {stream_connected, self()}, ok
    end;
notify_connected(_SdkKey) -> ok.

maybe_start_heartbeats(<<"sdk-heartbeat">>) ->
    _ = erlang:send_after(?HEARTBEAT_INTERVAL_MS, self(), heartbeat),
    ok;
maybe_start_heartbeats(_SdkKey) -> ok.

%% "sdk-read-timeout": a reconnection is served the updated flag. "sdk-heartbeat": heartbeats only.
sse_data(<<"sdk-read-timeout">>, 1) -> sse_simple_flag();
sse_data(<<"sdk-read-timeout">>, _Reconnection) -> sse_updated_flag();
sse_data(<<"sdk-heartbeat">>, _Connection) -> sse_simple_flag();
sse_data(SdkKey, _Connection) -> sse_data(SdkKey).

sse_data(<<"sdk-empty">>) -> sse_empty();
sse_data(<<"sdk-simple-flag">>) -> sse_simple_flag();
sse_data(<<"sdk-put-no-path">>) ->sse_put_no_path();
sse_data(<<"sdk-timeout">>) -> sse_timeout_delayed_reponse();
sse_data(_SdkKey) -> sse_empty().

sse_empty() ->
    <<"{",
        "\"path\":\"/\",",
        "\"data\":{",
            "\"flags\":{},",
            "\"segments\":{}",
        "}",
    "}">>.

sse_simple_flag() ->
    FlagBin = simple_flag(),
    <<"{",
        "\"path\":\"/\",",
        "\"data\":{",
            "\"flags\":{",
                FlagBin/binary,
            "},",
            "\"segments\":{}",
        "}",
    "}">>.

sse_updated_flag() ->
    FlagBin = updated_flag(),
    <<"{",
        "\"path\":\"/\",",
        "\"data\":{",
            "\"flags\":{",
                FlagBin/binary,
            "},",
            "\"segments\":{}",
        "}",
    "}">>.

sse_put_no_path() ->
    FlagBin = simple_flag(),
    <<"{",
        "\"data\":{",
            "\"flags\":{",
                FlagBin/binary,
            "},",
            "\"segments\":{}",
        "}",
    "}">>.

simple_flag() ->
    <<"\"abc\":{",
        "\"clientSide\":false,",
        "\"debugEventsUntilDate\":null,",
        "\"deleted\":false,",
        "\"fallthrough\":{\"variation\":0},",
        "\"key\":\"abc\",",
        "\"offVariation\":1,",
        "\"on\":true,",
        "\"prerequisites\":[],",
        "\"rules\":[],",
        "\"salt\":\"d0888ec5921e45c7af5bc10b47b033ba\",",
        "\"sel\":\"8b4d79c59adb4df492ebea0bf65dfd4c\",",
        "\"targets\":[],",
        "\"trackEvents\":true,",
        "\"variations\":[true,false],",
        "\"version\":5",
    "}">>.

updated_flag() ->
    <<"\"abc\":{",
        "\"clientSide\":false,",
        "\"debugEventsUntilDate\":null,",
        "\"deleted\":false,",
        "\"fallthrough\":{\"variation\":1},",
        "\"key\":\"abc\",",
        "\"offVariation\":1,",
        "\"on\":true,",
        "\"prerequisites\":[],",
        "\"rules\":[],",
        "\"salt\":\"d0888ec5921e45c7af5bc10b47b033ba\",",
        "\"sel\":\"8b4d79c59adb4df492ebea0bf65dfd4c\",",
        "\"targets\":[],",
        "\"trackEvents\":true,",
        "\"variations\":[true,false],",
        "\"version\":6",
    "}">>.

sse_timeout_delayed_reponse() ->
    timeout_once ! self(),
    Timeout = receive
        T when is_integer(T) -> T;
        _ -> 0
    end,
    timer:sleep(Timeout),
    FlagBin = simple_flag(),
    <<"{",
        "\"path\":\"/\",",
        "\"data\":{",
            "\"flags\":{",
                FlagBin/binary,
            "},",
            "\"segments\":{}",
        "}",
    "}">>.
