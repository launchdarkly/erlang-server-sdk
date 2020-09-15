-module(http_server_sse_handler).

%% cowboy
-export([init/2]).

%% cowboy

init(Req0, Opts) ->
    Authorization = cowboy_req:header(<<"authorization">>, Req0, <<>>),
    PutData = sse_data(Authorization),
    Req = cowboy_req:stream_reply(200, #{<<"content-type">> => <<"text/event-stream">>}, Req0),
    cowboy_req:stream_events(#{
        event => <<"put">>,
        data => PutData
    }, nofin, Req),
    {cowboy_loop, Req, Opts}.

%% internal

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
