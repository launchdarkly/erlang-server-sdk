-module(ldclient_ssl_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    default_instance_without_ssl_options/1,
    default_instance_with_ssl_options/1,
    named_instance_with_ssl_options/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        default_instance_without_ssl_options,
        default_instance_with_ssl_options,
        named_instance_with_ssl_options
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ldclient),
    Config.

end_per_suite(_) ->
    ok = application:stop(ldclient),
    ok.

init_per_testcase(_, Config) ->
    ok = ldclient:stop_all_instances(),
    Config.

end_per_testcase(_, _Config) ->
    ok.

%%====================================================================
%% Helpers
%%====================================================================

%%====================================================================
%% Tests
%%====================================================================

default_instance_without_ssl_options(_) ->
    ok = ldclient:start_instance(""),
    [] = ldclient_config:get_value(default, ssl_options).

default_instance_with_ssl_options(_) ->
    SslOptions = ssl_options(),
    ok = ldclient:start_instance("", instance_options(SslOptions)),
    SslOptions = ldclient_config:get_value(default, ssl_options).
    
named_instance_with_ssl_options(_) ->
    SslOptions = ssl_options(),
    ok = ldclient:start_instance("", other, instance_options(SslOptions)),
    SslOptions = ldclient_config:get_value(other, ssl_options).
    
ssl_options() -> [{verify, verify_peer}].
instance_options(SslOptions) -> #{ssl_options => SslOptions}.
