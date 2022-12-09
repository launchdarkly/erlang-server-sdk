-module(ldclient_attribute_reference_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    can_unescape_components/1,
    cannot_unescape_invalid_components/1,
    can_create_attribute_references/1,
    handles_bad_attribute_references/1,
    can_create_attribute_references_from_legacy_names/1,
    handles_invalid_legacy_name/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        can_unescape_components,
        cannot_unescape_invalid_components,
        can_create_attribute_references,
        handles_bad_attribute_references,
        can_create_attribute_references_from_legacy_names,
        handles_invalid_legacy_name
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%%====================================================================
%% Helpers
%%====================================================================

%%====================================================================
%% Tests
%%====================================================================

can_unescape_components(_) ->
    <<"~">> = ldclient_attribute_reference:unescape(<<"~0">>),
    <<"/">> = ldclient_attribute_reference:unescape(<<"~1">>),
    <<"~1">> = ldclient_attribute_reference:unescape(<<"~01">>).

cannot_unescape_invalid_components(_) ->
    error = ldclient_attribute_reference:unescape(<<"~">>),
    error = ldclient_attribute_reference:unescape(<<"~3">>),
    error = ldclient_attribute_reference:unescape(<<"a~a">>).

can_create_attribute_references(_) ->
    #{valid := true, components := [<<"potato">>], binary := <<"potato">>} =
        ldclient_attribute_reference:new(<<"potato">>),
    #{valid := true, components := [<<"potato">>], binary := <<"/potato">>} =
        ldclient_attribute_reference:new(<<"/potato">>),
    #{valid := true, components := [<<"potato">>, <<"yukon">>], binary := <<"/potato/yukon">>} =
        ldclient_attribute_reference:new(<<"/potato/yukon">>),
    #{valid := true, components := [<<"potato~/">>, <<"yukon/~">>], binary := <<"/potato~0~1/yukon~1~0">>} =
        ldclient_attribute_reference:new(<<"/potato~0~1/yukon~1~0">>).

handles_bad_attribute_references(_) ->
    #{valid := false} = ldclient_attribute_reference:new(<<"/potato/">>),
    #{valid := false} = ldclient_attribute_reference:new(<<"/potato//yukon">>),
    #{valid := false} = ldclient_attribute_reference:new(<<"">>),
    #{valid := false} = ldclient_attribute_reference:new(<<"/potato~3">>),
    #{valid := false} = ldclient_attribute_reference:new(<<"/">>),
    #{valid := false} = ldclient_attribute_reference:new(false).

can_create_attribute_references_from_legacy_names(_) ->
    #{valid := true, components := [<<"potato">>], binary := <<"potato">>} =
        ldclient_attribute_reference:new_from_legacy(<<"potato">>),
    #{valid := true, components := [<<"/potato">>], binary := <<"/~1potato">>} =
        ldclient_attribute_reference:new_from_legacy(<<"/potato">>),
    #{valid := true, components := [<<"/~/potato">>], binary := <<"/~1~0~1potato">>} =
        ldclient_attribute_reference:new_from_legacy(<<"/~/potato">>),
    #{valid := true, components := [<<"pot//ato">>], binary := <<"pot//ato">>} =
    ldclient_attribute_reference:new_from_legacy(<<"pot//ato">>).

handles_invalid_legacy_name(_) ->
    #{valid := false} = ldclient_attribute_reference:new_from_legacy(<<"">>).
