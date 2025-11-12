%%-------------------------------------------------------------------
%% @doc SDK key redaction utilities
%% @private
%% Provides functions to safely format error reasons and other data
%% to prevent SDK keys from appearing in logs.
%% @end
%%-------------------------------------------------------------------

-module(ldclient_key_redaction).

%% API
-export([format_httpc_error/1]).
-export([format_shotgun_error/1]).

%%===================================================================
%% API
%%===================================================================

%% @doc Format httpc error for safe logging
%%
%% This function provides an abstract representation of httpc error reasons
%% that is safe to log. It only formats known error types. Unknown
%% error types are represented as "unknown error" to prevent accidental
%% exposure of SDK keys, especially in cases where the SDK key might
%% contain special characters like newlines.
%% @end
-spec format_httpc_error(Reason :: term()) -> string().
%% HTTP status codes or other integers are safe to log
format_httpc_error(StatusCode) when is_integer(StatusCode) ->
    lists:flatten(io_lib:format("~b", [StatusCode]));
%% Common httpc connection errors
format_httpc_error({failed_connect, _}) ->
    "failed to connect";
format_httpc_error(timeout) ->
    "timeout opening connection";
format_httpc_error(etimedout) ->
    "connection timeout";
format_httpc_error(econnrefused) ->
    "connection refused";
format_httpc_error(enetunreach) ->
    "network unreachable";
format_httpc_error(ehostunreach) ->
    "host unreachable";
format_httpc_error(nxdomain) ->
    "domain name not found";
format_httpc_error({tls_alert, {_, Description}}) when is_atom(Description) ->
    lists:flatten(io_lib:format("tls_alert: ~p", [Description]));
format_httpc_error({tls_alert, Alert}) ->
    lists:flatten(io_lib:format("tls_alert: ~p", [Alert]));
%% Socket errors
format_httpc_error({socket_closed_remotely, _, _}) ->
    "socket closed remotely";
format_httpc_error(closed) ->
    "connection closed";
format_httpc_error(enotconn) ->
    "socket not connected";
%% Known atom errors
format_httpc_error(Reason) when is_atom(Reason) ->
    atom_to_list(Reason);
%% For any unknown error type, do not expose details
format_httpc_error(_Reason) ->
    "unknown error".

%% @doc Format shotgun/gun error for safe logging
%%
%% This function provides an abstract representation of shotgun error reasons
%% that is safe to log. Shotgun uses gun underneath, so errors can come from gun.
%% Unknown error types are represented as "unknown error".
%% @end
-spec format_shotgun_error(Reason :: term()) -> string().
%% HTTP status codes or other integers are safe to log
format_shotgun_error(StatusCode) when is_integer(StatusCode) ->
    lists:flatten(io_lib:format("~b", [StatusCode]));
%% Known shotgun errors from open
format_shotgun_error(gun_open_failed) ->
    "connection failed to open";
format_shotgun_error(gun_open_timeout) ->
    "timeout opening connection";
%% Gun/socket errors
format_shotgun_error(timeout) ->
    "connection timeout";
format_shotgun_error(econnrefused) ->
    "connection refused";
format_shotgun_error(enetunreach) ->
    "network unreachable";
format_shotgun_error(ehostunreach) ->
    "host unreachable";
format_shotgun_error(nxdomain) ->
    "domain name not found";
format_shotgun_error({shutdown, _}) ->
    "connection shutdown";
format_shotgun_error(normal) ->
    "connection closed normally";
format_shotgun_error(closed) ->
    "connection closed";
%% Known atom errors
format_shotgun_error(Reason) when is_atom(Reason) ->
    atom_to_list(Reason);
%% For any unknown error type, do not expose details
format_shotgun_error(_Reason) ->
    "unknown error".
