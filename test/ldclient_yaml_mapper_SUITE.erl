-module(ldclient_yaml_mapper_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
  load_and_convert_yaml/1,
  load_yaml_and_json/1,
  load_yaml_and_json_complex/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
  [
    load_and_convert_yaml,
    load_yaml_and_json,
    load_yaml_and_json_complex
  ].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(ldclient),
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

load_and_convert_yaml(_) ->
  DataFileSimpleFlagsYaml = code:priv_dir(ldclient) ++ "/flags-from-file-simple.yaml",
  Loaded = yamerl_constr:file(DataFileSimpleFlagsYaml, [{detailed_constr, true}]),
  Converted = ldclient_yaml_mapper:to_map_docs(Loaded, []),
  Expected = [#{<<"flagValues">> =>
  #{<<"my-boolean-flag-key">> => true, <<"my-integer-flag-key">> => 3,
    <<"my-string-flag-key">> => <<"value-1">>}}],
  Expected = Converted.

load_yaml_and_json(_) ->
  DataFileSimpleFlagsYaml = code:priv_dir(ldclient) ++ "/flags-from-file-simple.yaml",
  Loaded = yamerl_constr:file(DataFileSimpleFlagsYaml, [{detailed_constr, true}]),
  [YamlConverted | _Rest] = ldclient_yaml_mapper:to_map_docs(Loaded, []),
  DataFileSimpleFlags = code:priv_dir(ldclient) ++ "/flags-from-file-simple.json",
  {ok, JsonData} = file:read_file(DataFileSimpleFlags),
  JsonConverted = jsx:decode(JsonData, [return_maps]),
  YamlConverted = JsonConverted.

load_yaml_and_json_complex(_) ->
  DataFileYaml = code:priv_dir(ldclient) ++ "/flags-all-properties.yaml",
  Loaded = yamerl_constr:file(DataFileYaml, [{detailed_constr, true}]),
  YamlConverted = ldclient_yaml_mapper:to_map_docs(Loaded, []),
  Expected = [#{<<"flagValues">> =>
  #{<<"my-boolean-flag-key">> => true, <<"my-integer-flag-key">> => 3,
    <<"my-string-flag-key">> => <<"value-1">>},
    <<"flags">> =>
    #{<<"flag1">> =>
    #{<<"fallthrough">> => #{<<"variation">> => 2},
      <<"key">> => <<"flag1">>, <<"on">> => true,
      <<"variations">> => [<<"fall">>, <<"off">>, <<"on">>],
      <<"version">> => 1}},
    <<"segments">> =>
    #{<<"seg1">> =>
    #{<<"include">> => [<<"user1">>],
      <<"key">> => <<"seg1">>, <<"version">> => 1}}}],
  YamlConverted = Expected.
