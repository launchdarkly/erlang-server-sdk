-module(ldclient_key_redaction_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

%% Tests
-export([
    format_httpc_error_atom/1,
    format_httpc_error_integer/1,
    format_httpc_error_tuple/1,
    format_httpc_error_unknown/1,
    format_shotgun_error_atom/1,
    format_shotgun_error_integer/1,
    format_shotgun_error_unknown/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        format_httpc_error_atom,
        format_httpc_error_integer,
        format_httpc_error_tuple,
        format_httpc_error_unknown,
        format_shotgun_error_atom,
        format_shotgun_error_integer,
        format_shotgun_error_unknown
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_) ->
    ok.

%%====================================================================
%% Tests
%%====================================================================

format_httpc_error_atom(_) ->
    % Known atom errors should be formatted safely
    "timeout opening connection" = ldclient_key_redaction:format_httpc_error(timeout),
    "connection refused" = ldclient_key_redaction:format_httpc_error(econnrefused),
    "connection timeout" = ldclient_key_redaction:format_httpc_error(etimedout).

format_httpc_error_integer(_) ->
    % HTTP status codes should be formatted as integers
    "404" = ldclient_key_redaction:format_httpc_error(404),
    "500" = ldclient_key_redaction:format_httpc_error(500),
    "200" = ldclient_key_redaction:format_httpc_error(200).

format_httpc_error_tuple(_) ->
    % Known tuple errors should be formatted safely
    "failed to connect" = ldclient_key_redaction:format_httpc_error({failed_connect, []}),
    "connection closed" = ldclient_key_redaction:format_httpc_error(closed).

format_httpc_error_unknown(_) ->
    % Unknown error types, including those that might contain SDK keys, should be redacted
    "unknown error" = ldclient_key_redaction:format_httpc_error({http_error, "sdk-12345"}),
    "unknown error" = ldclient_key_redaction:format_httpc_error(["Authorization: sdk-test\n"]),
    "unknown error" = ldclient_key_redaction:format_httpc_error({request, [{headers, [{"Authorization", "sdk-key-with-newline\n"}]}]}).

format_shotgun_error_atom(_) ->
    % Known atom errors should be formatted safely
    "connection timeout" = ldclient_key_redaction:format_shotgun_error(timeout),
    "connection failed to open" = ldclient_key_redaction:format_shotgun_error(gun_open_failed),
    "timeout opening connection" = ldclient_key_redaction:format_shotgun_error(gun_open_timeout),
    "connection refused" = ldclient_key_redaction:format_shotgun_error(econnrefused).

format_shotgun_error_integer(_) ->
    % HTTP status codes should be formatted as integers
    "401" = ldclient_key_redaction:format_shotgun_error(401),
    "500" = ldclient_key_redaction:format_shotgun_error(500).

format_shotgun_error_unknown(_) ->
    % Unknown error types, including those that might contain SDK keys, should be redacted
    "unknown error" = ldclient_key_redaction:format_shotgun_error({gun_error, "sdk-12345"}),
    "unknown error" = ldclient_key_redaction:format_shotgun_error(["Headers: sdk-test\n"]),
    "unknown error" = ldclient_key_redaction:format_shotgun_error({connection_error, [{auth, "sdk-malformed\nkey"}]}).
