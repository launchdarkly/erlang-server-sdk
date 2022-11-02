-module(ldclient_test_utils).

-export([get_simple_flag/0]).
-export([get_simple_flag_patch/0]).
-export([get_simple_flag_patch_old/0]).
-export([get_simple_flag_delete/0]).
-export([get_flag_with_extra_property/0]).

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

get_simple_flag_patch() ->
    {
        <<"abc">>,
        <<"\"data\":{",
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
            "}">>,
        #{
            <<"clientSide">> => false,
            <<"debugEventsUntilDate">> => null,
            <<"deleted">> => false,
            <<"fallthrough">> => #{<<"variation">> => 1},
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
            <<"version">> => 6
        }
    }.

get_simple_flag_patch_old() ->
    {
        <<"abc">>,
        <<"\"data\":{",
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
            "\"version\":4",
            "}">>,
        #{
            <<"clientSide">> => false,
            <<"debugEventsUntilDate">> => null,
            <<"deleted">> => false,
            <<"fallthrough">> => #{<<"variation">> => 1},
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
            <<"version">> => 4
        }
    }.

get_simple_flag_delete() ->
    {
        <<"abc">>,
        <<"\"abc\":{",
            "\"clientSide\":false,",
            "\"debugEventsUntilDate\":null,",
            "\"deleted\":true,",
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
            "\"version\":6",
            "}">>,
        #{
            <<"clientSide">> => false,
            <<"debugEventsUntilDate">> => null,
            <<"deleted">> => true,
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
            <<"version">> => 6
        }
    }.

get_flag_with_extra_property() ->
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
            "\"foo\":\"bar\",",
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
            <<"foo">> => <<"bar">>,
            <<"version">> => 5
        }
    }.
