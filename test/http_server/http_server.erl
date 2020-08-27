-module(http_server).

-export([start/2]).
-export([stop/1]).
-export([start_phase/3]).

%% @private
start(_StartType, _StartArgs) ->
    http_server_sup:start_link().

%% @private
stop(_State) ->
    ok = cowboy:stop_listener(http_server).

-spec start_phase(atom(), application:start_type(), []) -> ok | {error, term()}.
start_phase(start_cowboy_http, _StartType, []) ->
    Port = application:get_env(http_server, http_port, 8888),
    _ListenerCount = application:get_env(http_server, http_listener_count, 10),
    Routes = [
        {
            '_',
            [
                {"/all", http_server_sse_handler, []}
            ]
        }
    ],
    Dispatch = cowboy_router:compile(Routes),
    TransportOptions = [{port, Port}],
    ProtocolOptions = #{env => #{dispatch => Dispatch}},
    {ok, _} =
        cowboy:start_clear(http_server, TransportOptions, ProtocolOptions),
    TimeoutOncePid = spawn_link(http_server_timeout_once, start, [6000]),
    true = register(timeout_once, TimeoutOncePid),
    ok.
