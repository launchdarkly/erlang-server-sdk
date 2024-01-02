-module(ldclient_config_SUITE).

-include_lib("common_test/include/ct.hrl").

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
                    {customize_hostname_check, _}] = BasicOptions,
                true = (length(Ciphers) =/= 0);
            {_, _} ->
                [
                    {cacerts, _},
                    {verify, verify_peer},
                    {ciphers, Ciphers},
                    {depth, 3},
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
        {customize_hostname_check, _}] = ldclient_config:tls_ca_certfile_options("imaginary/path/to/certfile.crt"),
    true = (length(Ciphers) =/= 0).

tls_basic_linux_options(_) ->
    [
        {cacertfile, "/etc/ssl/certs/ca-certificates.crt"},
        {verify, verify_peer},
        {ciphers, Ciphers},
        {depth, 3},
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
