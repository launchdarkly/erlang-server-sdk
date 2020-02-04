%%-------------------------------------------------------------------
%% @doc Polling update requestor for testing
%%
%% @end
%%-------------------------------------------------------------------

-module(eld_update_requestor_test).

-behaviour(eld_update_requestor).

%% API
-export([get_simple_flag/0]).
-export([get_simple_segment/0]).

%% Behavior callbacks
-export([init/0, all/3]).

%%===================================================================
%% API
%%===================================================================

get_simple_flag() ->
    {
        <<"abc">>,
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
        "}">>,
        #{
            <<"clientSide">> => false,
            <<"debugEventsUntilDate">> => null,
            <<"deleted">> => false,
            <<"fallthrough">> => #{<<"variation">> => 0},
            <<"key">> => <<"abc">>,
            <<"offVariation">> => 1,
            <<"on">> => true,
            <<"prerequisites">> => [],
            <<"rules">> => [],
            <<"salt">> => <<"d0888ec5921e45c7af5bc10b47b033ba">>,
            <<"sel">> => <<"8b4d79c59adb4df492ebea0bf65dfd4c">>,
            <<"targets">> => [],
            <<"trackEvents">> => true,
            <<"variations">> => [true,false],
            <<"version">> => 5
        }
    }.

get_simple_segment() ->
    {
        <<"xyz">>,
        <<"\"xyz\": {",
            "\"deleted\":false,",
            "\"excluded\":[],",
            "\"included\":[],",
            "\"key\":\"xyz\",",
            "\"rules\":[],",
            "\"salt\":\"b2ba88c74ad34c288ec10ba78e150afd\",",
            "\"version\":8",
        "}">>,
        #{
            <<"deleted">> => false,
            <<"excluded">> => [],
            <<"included">> => [],
            <<"key">> => <<"xyz">>,
            <<"rules">> => [],
            <<"salt">> => <<"b2ba88c74ad34c288ec10ba78e150afd">>,
            <<"version">> => 8
        }
    }.

%%===================================================================
%% Behavior callbacks
%%===================================================================

%% Empty state for test update requestor
-spec init() -> atom().
init() -> ok.

%% @doc Return static values mocking the polling service
%%
%% @end
-spec all(Uri :: string(), SdkKey :: string(), State :: any()) -> eld_update_requestor:response().
all(_Uri, SdkKey, State) ->
    Result = case SdkKey of
        "sdk-key-unauthorized" ->
            {error, 401, ""};
        "sdk-key-not-found" ->
            {error, 404, ""};
        "sdk-key-internal-error" ->
            {error, 500, ""};
        "sdk-key-flags-segments" ->
            {_, FlagBin, _} = get_simple_flag(),
            {_, SegmentBin, _} = get_simple_segment(),
            {ok, <<"{\"flags\":{",FlagBin/binary,"},\"segments\":{",SegmentBin/binary,"}}">>};
        "sdk-key-events-fail" ->
            {ok, <<"{\"flags\":{},\"segments\":{}}">>};
        "" ->
            {ok, <<"{\"flags\":{},\"segments\":{}}">>}
    end,
    {Result, State}.
