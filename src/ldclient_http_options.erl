%%-------------------------------------------------------------------
%% @doc Http option parsing for http and gun.
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_http_options).

%% API
-export([httpc_parse_http_options/1]).
-export([httpc_parse_http_options/2]).
-export([httpc_append_custom_headers/2]).

-export([gun_parse_http_options/1]).
-export([gun_parse_http_options/2]).
-export([gun_append_custom_headers/2]).

%% Implementation for httpc.

-spec httpc_parse_http_options(Options :: ldclient_config:http_options(), BaseOptions :: list()) -> list().
httpc_parse_http_options(Options, BaseOptions) ->
    OptionsWithTls = httpc_parse_tls_options(BaseOptions, Options),
    httpc_parse_connect_timeout(OptionsWithTls, Options).

-spec httpc_parse_http_options(Options :: ldclient_config:http_options()) -> list().
httpc_parse_http_options(Options) ->
    httpc_parse_http_options(Options, []).

-spec httpc_parse_tls_options(HttpOptions :: list(), ConfigOptions :: ldclient_config:http_options()) -> list().
httpc_parse_tls_options(HttpOptions, #{tls_options := undefined}) -> HttpOptions;
httpc_parse_tls_options(HttpOptions, #{tls_options := TlsOptions}) ->
    [{ssl, TlsOptions} | HttpOptions].

-spec httpc_parse_connect_timeout(HttpOptions :: list(), ConfigOptions :: ldclient_config:http_options()) -> list().
httpc_parse_connect_timeout(HttpOptions, #{connect_timeout := undefined}) -> HttpOptions;
httpc_parse_connect_timeout(HttpOptions, #{connect_timeout := TimeoutTime}) ->
    [{connect_timeout, TimeoutTime} | HttpOptions].

-spec httpc_append_custom_headers(Headers :: list(), ConfigOptions :: ldclient_config:http_options()) -> list().
httpc_append_custom_headers(Headers, #{custom_headers := undefined}) -> Headers;
httpc_append_custom_headers(Headers, #{custom_headers := CustomHeaders}) -> lists:append(Headers, CustomHeaders).

%% Implementation for gun

-spec gun_parse_http_options(Options :: ldclient_config:http_options()) -> map().
gun_parse_http_options(undefined) ->
    #{retry => 0, protocols => [http]};
gun_parse_http_options(HttpOptions) ->
    gun_parse_http_options(HttpOptions, #{
        retry => 0, protocols => [http]
    }).

-spec gun_parse_http_options(Options :: ldclient_config:http_options(), GunOptions :: map()) -> map().
gun_parse_http_options(HttpOptions, GunOptions) ->
    OptionsWithTls = gun_parse_tls_options(HttpOptions, GunOptions),
    gun_parse_connect_timeout(HttpOptions, OptionsWithTls).

-spec gun_parse_tls_options(ConfigOptions :: ldclient_config:http_options(), GunOptions :: map()) -> map().
gun_parse_tls_options(#{tls_options := undefined}, GunOptions) -> GunOptions;
gun_parse_tls_options(#{tls_options := TlsOptions}, GunOptions) ->
    GunOptions#{
        tls_opts => TlsOptions
    }.

-spec gun_append_custom_headers(Headers :: map(), ConfigOptions :: ldclient_config:http_options()) -> map().
gun_append_custom_headers(Headers, #{custom_headers := undefined}) -> Headers;
gun_append_custom_headers(Headers, #{custom_headers := CustomHeaders}) ->
    maps:merge(Headers, maps:from_list(CustomHeaders)).

-spec gun_parse_connect_timeout(ConfigOptions :: ldclient_config:http_options(), GunOptions :: map()) -> map().
gun_parse_connect_timeout(#{connect_timeout := undefined}, GunOptions) -> GunOptions;
gun_parse_connect_timeout(#{connect_timeout := TimeoutTime}, GunOptions) ->
    GunOptions#{
        connect_timeout => TimeoutTime
    }.
