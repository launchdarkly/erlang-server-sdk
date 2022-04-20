%-------------------------------------------------------------------
%% @doc `ts_command_request_handler' module
%%
%% Handles requests from the test harness.
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ts_command_request_handler).

%% Cowboy callbacks
-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    delete_resource/2
]).

%%
%% Additional callbacks
-export([
    handle_command/2
]).

-spec init(Request :: cowboy_req:req(), State :: map()) -> {cowboy_rest, cowboy_req:req(), map()}.
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

-spec allowed_methods(Request :: cowboy_req:req(), State :: map()) -> {[binary()], cowboy_req:req(), map()}.
allowed_methods(Req, State) ->
    Methods = [<<"POST">>, <<"DELETE">>],
    {Methods, Req, State}.

-spec content_types_accepted(Request :: cowboy_req:req(), State :: map()) -> {list(), cowboy_req:req(), map()}.
content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, handle_command}
    ], Req, State}.

-spec delete_resource(Request :: cowboy_req:req(), State :: map()) -> {atom(), cowboy_req:req(), map()}.
delete_resource(Req, State) ->
    ClientTag = cowboy_req:binding(client_tag, Req),
    ok = ts_client_handler:delete_client(binary_to_atom(ClientTag, utf8)),
    Req0 = cowboy_req:reply(204, Req),
    {true, Req0, State}.

-spec handle_command(Request :: cowboy_req:req(), State :: map()) -> {atom(), cowboy_req:req(), map()}.
handle_command(Req, State) ->
    ClientTag = cowboy_req:binding(client_tag, Req),
    {ok, Data, Req0} = cowboy_req:read_body(Req),
    DecodedJson = jsx:decode(Data, [return_maps]),
    Command = ts_command_params:parse_command(DecodedJson),
    ResponseBody = ts_command_handler:handle_command(binary_to_atom(ClientTag, utf8), Command),
    Req1 = cowboy_req:set_resp_body(jsx:encode(ResponseBody), Req0),
    {true, Req1, State}.