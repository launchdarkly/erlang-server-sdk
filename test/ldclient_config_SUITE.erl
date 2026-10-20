-module(ldclient_config_SUITE).

-include_lib("common_test/include/ct.hrl").

%% emSign ECC Root CA - G3, one of the roots removed from the bundled certifi store.
-define(EMSIGN_ECC_ROOT_CA_G3_PEM, <<"
-----BEGIN CERTIFICATE-----
MIICTjCCAdOgAwIBAgIKPPYHqWhwDtqLhDAKBggqhkjOPQQDAzBrMQswCQYDVQQG
EwJJTjETMBEGA1UECxMKZW1TaWduIFBLSTElMCMGA1UEChMcZU11ZGhyYSBUZWNo
bm9sb2dpZXMgTGltaXRlZDEgMB4GA1UEAxMXZW1TaWduIEVDQyBSb290IENBIC0g
RzMwHhcNMTgwMjE4MTgzMDAwWhcNNDMwMjE4MTgzMDAwWjBrMQswCQYDVQQGEwJJ
TjETMBEGA1UECxMKZW1TaWduIFBLSTElMCMGA1UEChMcZU11ZGhyYSBUZWNobm9s
b2dpZXMgTGltaXRlZDEgMB4GA1UEAxMXZW1TaWduIEVDQyBSb290IENBIC0gRzMw
djAQBgcqhkjOPQIBBgUrgQQAIgNiAAQjpQy4LRL1KPOxst3iAhKAnjlfSU2fySU0
WXTsuwYc58Byr+iuL+FBVIcUqEqy6HyC5ltqtdyzdc6LBtCGI79G1Y4PPwT01xyS
fvalY8L1X44uT6EYGQIrMgqCZH0Wk9GjQjBAMB0GA1UdDgQWBBR8XQKEE9TMipuB
zhccLikenEhjQjAOBgNVHQ8BAf8EBAMCAQYwDwYDVR0TAQH/BAUwAwEB/zAKBggq
hkjOPQQDAwNpADBmAjEAvvNhzwIQHWSVB7gYboiFBS+DCBeQyh+KTOgNG3qxrdWB
CUfvO6wIBHxcmbHtRwfSAjEAnbpV/KlK6O3t5nYBQnvI+GDZjVGLVTv7jHvrZQnD
+JbNR6iC8hZVdyR+EhCVBCyj
-----END CERTIFICATE-----
">>).

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    get_http_options_from_default_config/1,
    get_http_options_from_empty_specified_options/1,
    get_http_options_from_only_connect_timeout_specified/1,
    get_http_options_custom_headers/1,
    get_http_options_tls_options/1,
    get_http_options_multiple_options/1,
    tls_basic_options/1,
    tls_with_ca_certfile_options/1,
    tls_basic_linux_options/1,
    with_tls_revocation/1,
    tls_basic_certifi_options_excludes_distrusted_roots/1,
    app_info_options/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        get_http_options_from_default_config,
        get_http_options_from_empty_specified_options,
        get_http_options_from_only_connect_timeout_specified,
        get_http_options_custom_headers,
        get_http_options_tls_options,
        get_http_options_multiple_options,
        tls_basic_options,
        tls_with_ca_certfile_options,
        tls_basic_linux_options,
        with_tls_revocation,
        tls_basic_certifi_options_excludes_distrusted_roots,
        app_info_options
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(ldclient),
    Config.

end_per_suite(_) ->
    ok = application:stop(ldclient).

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%%====================================================================
%% Helpers
%%====================================================================

emsign_ecc_root_ca_g3_der() ->
    [{'Certificate', Der, not_encrypted}] = public_key:pem_decode(?EMSIGN_ECC_ROOT_CA_G3_PEM),
    Der.

%%====================================================================
%% Tests
%%====================================================================

get_http_options_from_default_config(_) ->
    Settings = ldclient_config:parse_options("sdk-key", #{}),
    #{
        tls_options := undefined,
        connect_timeout := 2000,
        custom_headers := undefined
    } = maps:get(http_options, Settings, undefined).

get_http_options_from_empty_specified_options(_) ->
    Settings = ldclient_config:parse_options("sdk-key", #{
        http_options => #{}
    }),
    #{
        tls_options := undefined,
        connect_timeout := 2000,
        custom_headers := undefined
    } = maps:get(http_options, Settings, undefined).

get_http_options_from_only_connect_timeout_specified(_) ->
    Settings = ldclient_config:parse_options("sdk-key", #{
        http_options => #{
            connect_timeout => 12345
        }
    }),
    #{
        tls_options := undefined,
        connect_timeout := 12345,
        custom_headers := undefined
    } = maps:get(http_options, Settings, undefined).

get_http_options_custom_headers(_) ->
    Settings = ldclient_config:parse_options("sdk-key", #{
        http_options => #{
            custom_headers => [{"example-header-1", "heaver-value"}]
        }
    }),
    #{
        tls_options := undefined,
        connect_timeout := 2000,
        custom_headers := [{"example-header-1", "heaver-value"}]
    } = maps:get(http_options, Settings, undefined).

get_http_options_tls_options(_) ->
    Settings = ldclient_config:parse_options("sdk-key", #{
        http_options => #{
            tls_options => [{verify, verify_peer}]
        }
    }),
    #{
        tls_options := [{verify, verify_peer}],
        connect_timeout := 2000,
        custom_headers := undefined
    } = maps:get(http_options, Settings, undefined).

get_http_options_multiple_options(_) ->
    Settings = ldclient_config:parse_options("sdk-key", #{
        http_options => #{
            tls_options => [{verify, verify_peer}],
            connect_timeout => 12345,
            custom_headers => [{"example-header-1", "heaver-value"}]
        }
    }),
    #{
        tls_options := [{verify, verify_peer}],
        connect_timeout := 12345,
        custom_headers := [{"example-header-1", "heaver-value"}]
    } = maps:get(http_options, Settings, undefined).

tls_basic_options(_) ->
    case erlang:list_to_integer(erlang:system_info(otp_release)) >= 25 of
    true -> 
        BasicOptions = ldclient_config:tls_basic_options(),
        CaCerts = public_key:cacerts_get(),
        [{cacerts, CaCerts},
        {verify, verify_peer},
        {ciphers, Ciphers},
        {depth, 3},
        {versions, _Versions},
        {customize_hostname_check, _}] = BasicOptions,
        true = (length(Ciphers) =/= 0);
    false -> 
        BasicOptions = ldclient_config:tls_basic_options(),
        case os:type() of
            {unix, linux} ->
                [
                    {cacertfile, "/etc/ssl/certs/ca-certificates.crt"},
                    {verify, verify_peer},
                    {ciphers, Ciphers},
                    {depth, 3},
                    {versions, _Versions},
                    {customize_hostname_check, _}] = BasicOptions,
                true = (length(Ciphers) =/= 0);
            {_, _} ->
                [
                    {cacerts, _},
                    {verify, verify_peer},
                    {ciphers, Ciphers},
                    {depth, 3},
                    {versions, _Versions},
                    {customize_hostname_check, _}] = BasicOptions,
                true = (length(Ciphers) =/= 0)
        end
    end.

tls_with_ca_certfile_options(_) ->
    [
        {cacertfile, "imaginary/path/to/certfile.crt"},
        {verify, verify_peer},
        {ciphers, Ciphers},
        {depth, 3},
        {versions, _Versions},
        {customize_hostname_check, _}] = ldclient_config:tls_ca_certfile_options("imaginary/path/to/certfile.crt"),
    true = (length(Ciphers) =/= 0).

tls_basic_linux_options(_) ->
    [
        {cacertfile, "/etc/ssl/certs/ca-certificates.crt"},
        {verify, verify_peer},
        {ciphers, Ciphers},
        {depth, 3},
        {versions, _Versions},
        {customize_hostname_check, _}] = ldclient_config:tls_basic_linux_options(),
    true = (length(Ciphers) =/= 0).

with_tls_revocation(_) ->
    [
        {crl_check, true},
        {crl_cache,
            {ssl_crl_cache,
                {internal, [{http, 1000}]}
            }
        },
        {verify, verify_peer}
    ] = ldclient_config:with_tls_revocation([{verify, verify_peer}]).

tls_basic_certifi_options_excludes_distrusted_roots(_) ->
    Certifi = certifi:cacerts(),
    {cacerts, CaCerts} = lists:keyfind(cacerts, 1, ldclient_config:tls_basic_certifi_options()),
    false = lists:member(emsign_ecc_root_ca_g3_der(), CaCerts),
    %% Only the four distrusted roots can be removed; every other certifi root is kept.
    true = lists:all(fun(Der) -> lists:member(Der, Certifi) end, CaCerts),
    true = (length(Certifi) - length(CaCerts) =< 4).

app_info_options(_) ->
    #{application := undefined} = ldclient_config:parse_options("sdk-key", #{}),
    #{application := #{id := <<"the-id">>}} = ldclient_config:parse_options("sdk-key",
        #{application => #{id => <<"the-id">>}}),
    #{application := #{version := <<"the-version">>}} = ldclient_config:parse_options("sdk-key",
        #{application => #{version => <<"the-version">>}}),
    #{application := #{version := <<"the-version">>, id := <<"the-id">>}} = ldclient_config:parse_options("sdk-key",
        #{application => #{version => <<"the-version">>, id => <<"the-id">>}}),
    %% Contains invalid characters
    #{application := #{version := <<"the-version">>}} = ldclient_config:parse_options("sdk-key",
        #{application => #{version => <<"the-version">>, id => <<"the-id#$%*&#%">>}}),
    %% Contains a string that is too long.
    #{application := ApplicationWithoutId} = ldclient_config:parse_options("sdk-key", #{application => #{version => <<"the-version">>,
        id => list_to_binary(lists:duplicate(65, $A))}}),
    undefined = maps:get(id, ApplicationWithoutId, undefined),
    %% Contains a string that is the max length.
    MaxLengthId = list_to_binary(lists:duplicate(64, $A)),
    #{application := #{version := <<"the-version">>, id := MaxLengthId}} = ldclient_config:parse_options("sdk-key",
        #{application => #{version => <<"the-version">>,
        id => MaxLengthId}}).

