-module(ldclient_http_options_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    httpc_parse_http_options_with_connection_timeout/1,
    httpc_parse_http_options_with_tls_options/1,
    httpc_append_custom_headers/1,
    httpc_parse_http_options_with_connection_undefined_timeout/1,
    gun_parse_http_options_with_connection_timeout/1,
    gun_parse_http_options_with_tls_options/1,
    gun_append_custom_headers/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        httpc_parse_http_options_with_connection_timeout,
        httpc_parse_http_options_with_tls_options,
        httpc_append_custom_headers,
        httpc_parse_http_options_with_connection_undefined_timeout,
        gun_parse_http_options_with_connection_timeout,
        gun_parse_http_options_with_tls_options,
        gun_append_custom_headers
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

httpc_parse_http_options_with_connection_timeout(_) ->
    Settings = ldclient_config:parse_options("sdk-key", #{
        http_options => #{
            connect_timeout => 1234
        }
    }),
    [{connect_timeout, 1234}] = ldclient_http_options:httpc_parse_http_options(maps:get(http_options, Settings, undefined)).

httpc_parse_http_options_with_connection_undefined_timeout(_) ->
    Settings = ldclient_config:parse_options("sdk-key", #{
        http_options => #{
            connect_timeout => undefined
        }
    }),
    [] = ldclient_http_options:httpc_parse_http_options(maps:get(http_options, Settings, undefined)).

httpc_parse_http_options_with_tls_options(_) ->
    Settings = ldclient_config:parse_options("sdk-key", #{
        http_options => #{
            tls_options => [{verify, verify_peer}]
        }
    }),
    [{connect_timeout, 2000}, {ssl, [{verify, verify_peer}]}] = ldclient_http_options:httpc_parse_http_options(maps:get(http_options, Settings, undefined)).

httpc_append_custom_headers(_) ->
    Settings = ldclient_config:parse_options("sdk-key", #{
        http_options => #{
            custom_headers => [
                {"Basic-String-Header", "String"},
                {"Binary-String-Header", "Binary"}
            ]
        }
    }),
    [
        {"initial-header", "initial-header-value"},
        {"Basic-String-Header", "String"},
        {"Binary-String-Header", "Binary"}
    ] = ldclient_http_options:httpc_append_custom_headers([{"initial-header", "initial-header-value"}], maps:get(http_options, Settings, undefined)).

gun_parse_http_options_with_connection_timeout(_) ->
    Settings = ldclient_config:parse_options("sdk-key", #{
        http_options => #{
            connect_timeout => 1234
        }
    }),
    #{
        protocols := [http],
        retry := 0,
        connect_timeout := 1234
    } = ldclient_http_options:gun_parse_http_options(maps:get(http_options, Settings, undefined)).

gun_parse_http_options_with_tls_options(_) ->
    Settings = ldclient_config:parse_options("sdk-key", #{
        http_options => #{
            tls_options => [{verify, verify_peer}]
        }
    }),
    #{
        protocols := [http],
        retry := 0,
        connect_timeout := 2000,
        tls_opts := [{verify, verify_peer}]
    } = ldclient_http_options:gun_parse_http_options(maps:get(http_options, Settings, undefined)).

gun_append_custom_headers(_) ->
    Settings = ldclient_config:parse_options("sdk-key", #{
        http_options => #{
            custom_headers => [
                {"Basic-String-Header", "String"},
                {"Binary-String-Header", "Binary"}
            ]
        }
    }),
    #{
        "initial-header" := "initial-header-value",
        "Basic-String-Header" := "String",
        "Binary-String-Header" := "Binary"
    } = ldclient_http_options:gun_append_custom_headers(#{"initial-header" => "initial-header-value"}, maps:get(http_options, Settings, undefined)).
