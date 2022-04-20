%%-------------------------------------------------------------------
%% @doc `ts_server' module
%%
%% Server application for contract test requests.
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ts_server).

-export([start/2]).
-export([stop/1]).
-export([start_phase/3]).

%% @private
start(_StartType, _StartArgs) ->
  ts_server_sup:start_link().

%% @private
stop(_State) ->
  ok = cowboy:stop_listener(server).

-spec start_phase(atom(), application:start_type(), []) -> ok | {error, term()}.
start_phase(start_cowboy_http, _StartType, []) ->
  Port = application:get_env(ts_server, ts_port, 8000),
  Routes = [
    {
      '_',
      [
        {"/", ts_service_request_handler, #{}},
        {"/client/:client_tag", ts_command_request_handler, #{}}
      ]
    }
  ],
  Dispatch = cowboy_router:compile(Routes),
  TransportOptions = [{port, Port}],
  ProtocolOptions = #{env => #{dispatch => Dispatch}},
  {ok, _} =
    cowboy:start_clear(ts_server, TransportOptions, ProtocolOptions),
  ok.
