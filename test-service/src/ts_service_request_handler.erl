%-------------------------------------------------------------------
%% @doc `ts_service_request_handler' module
%%
%% Handles root level request from the test harness.
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ts_service_request_handler).

%% Cowboy callbacks
-export([
  init/2,
  allowed_methods/2,
  content_types_provided/2,
  content_types_accepted/2,
  delete_resource/2
]).

%%
%% Additional callbacks
-export([
  get_service_detail/2,
  create_client/2
]).

-spec init(Request :: cowboy_req:req(), State :: map()) -> {cowboy_rest, cowboy_req:req(), map()}.
init(Req, Opt) ->
  {cowboy_rest, Req, Opt}.

-spec allowed_methods(Request :: cowboy_req:req(), State :: map()) -> {[binary()], cowboy_req:req(), map()}.
allowed_methods(Req, State) ->
  Methods = [<<"GET">>, <<"POST">>, <<"DELETE">>],
  {Methods, Req, State}.

-spec content_types_provided(Request :: cowboy_req:req(), State :: map()) -> {list(), cowboy_req:req(), map()}.
content_types_provided(Req, State) ->
  {[
    {<<"application/json">>, get_service_detail}
  ], Req, State}.

-spec content_types_accepted(Request :: cowboy_req:req(), State :: map()) -> {list(), cowboy_req:req(), map()}.
content_types_accepted(Req, State) ->
  {[
    {<<"application/json">>, create_client}
  ], Req, State}.

-spec delete_resource(Request :: cowboy_req:req(), State :: map()) -> {atom(), cowboy_req:req(), map()}.
delete_resource(Req, State) ->
  init:stop("Exit of service requested."),
  {true, Req, State}.

-spec create_client(Request :: cowboy_req:req(), State :: map()) -> {true, cowboy_req:req(), map()}.
create_client(Req, State) ->
  {ok, Data, Req0} = cowboy_req:read_body(Req),
  DecodedJson = jsx:decode(Data, [return_maps]),
  ParsedParams = ts_service_params:parse_create_instance_params(DecodedJson),
  {BinaryTag, Tag} = tag_from_integer(erlang:unique_integer([positive, monotonic])),
  ok = ts_client_handler:create_client(Tag, ParsedParams),
  ClientPath = <<"/client/">>,
  Headers = #{
    <<"Location">> => <<ClientPath/binary, BinaryTag/binary>>
  },
  Req1 = cowboy_req:set_resp_headers(Headers, Req0),
  {true, Req1, State}.

-spec get_service_detail(Req :: cowboy_req:req(), State :: map()) -> {binary(), cowboy_req:req(), map()}.
get_service_detail(Req, State) ->
  Body = jsx:encode(#{
    <<"name">> => <<"erlang-server-sdk">>,
    <<"capabilities">> => [
      <<"server-side">>,
      <<"all-flags-with-reasons">>,
      <<"tags">>,
      <<"server-side-polling">>,
      <<"user-type">>,
      <<"inline-context">>
    ],
    <<"clientVersion">> => ldclient_config:get_version()
  }),
  {Body, Req, State}.

-spec tag_from_integer(Int :: number()) -> {binary(), atom()}.
tag_from_integer(Int) ->
  BinaryInt = integer_to_binary(Int),
  Label = <<"c">>,
  BinaryTag = <<Label/binary, BinaryInt/binary>>,
  Tag = binary_to_atom(BinaryTag, utf8),
  {BinaryTag, Tag}.
