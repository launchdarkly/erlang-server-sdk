-module(ldclient_tls_options_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    check_secure_default_httpc_valid/1,
    check_secure_default_httpc_invalid/1,
    check_secure_default_httpc_interception/1,
    check_secure_default_httpc_broken_crypto/1,
    check_secure_default_httpc_revocation/1,
    check_secure_default_httpc_legacy/1,
    check_secure_default_shotgun_stream_endpoint/1,
    check_secure_default_httpc_poll_endpoint/1,
    check_secure_default_httpc_events_endpoint/1,
    check_secure_default_shotgun_invalid/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        check_secure_default_httpc_valid,
        check_secure_default_httpc_invalid,
        check_secure_default_httpc_interception,
        check_secure_default_httpc_broken_crypto,
        check_secure_default_httpc_revocation,
        check_secure_default_httpc_legacy,
        check_secure_default_shotgun_stream_endpoint,
        check_secure_default_httpc_poll_endpoint,
        check_secure_default_httpc_events_endpoint,
        check_secure_default_shotgun_invalid
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

open_stream(Uri, HttpOptions) ->
    GunOpts = ldclient_http_options:gun_parse_http_options(HttpOptions),
    Opts = #{gun_opts => GunOpts},
    {ok, {Scheme, _UserInfo, Host, Port, Path, Query}} = http_uri:parse(Uri),
    case shotgun:open(Host, Port, Scheme, Opts) of
        {error, gun_open_failed} ->
            {error, gun_open_failed, "Could not open connection to host"};
        {error, gun_open_timeout} ->
            {error, gun_open_timeout, "Connection timeout"};
        {ok, Pid} ->
            _ = monitor(process, Pid),
            F = fun(nofin, _Ref, Bin) ->
                try
                    shotgun:parse_event(Bin)
                catch Code:Reason ->
                    % Exception when processing event, log error, close connection
                    error_logger:warning_msg("Invalid SSE event error (~p): ~p", [Code, Reason]),
                    shotgun:close(Pid)
                end;
                (fin, _Ref, _Bin) ->
                    % Connection ended, close monitored shotgun client pid, so we can reconnect
                    error_logger:warning_msg("Streaming connection ended"),
                    shotgun:close(Pid)
                end,
            Options = #{async => true, async_mode => sse, handle_event => F},
            Headers = #{
                "authorization" => os:getenv("LD_SDK_KEY"),
                <<"user-agent">> => ldclient_config:get_user_agent()
            },
            case shotgun:get(Pid, Path ++ Query, Headers, Options) of
                {error, Reason} ->
                    shotgun:close(Pid),
                    {error, get_request_failed, Reason};
                {ok, _Response} ->
                    {ok, Pid}
            end
    end.

%%====================================================================
%% Tests
%%====================================================================

check_secure_default_shotgun_stream_endpoint(_) ->
    {ok, _} = open_stream("https://stream.launchdarkly.com/all", #{
        tls_options => ldclient_config:tls_basic_options(),
        connect_timeout => undefined,
        custom_headers => undefined
    }).

check_secure_default_httpc_poll_endpoint(_) ->
    Options = [{ssl, ldclient_config:tls_basic_options()}],
    {ok, _} = httpc:request(get, {"https://sdk.launchdarkly.com/sdk/latest-all", [
        {"Authorization", os:getenv("LD_SDK_KEY")},
        {"User-Agent", ldclient_config:get_user_agent()}
    ]}, Options, []).

check_secure_default_httpc_events_endpoint(_) ->
    Options = [{ssl, ldclient_config:tls_basic_options()}],
    {ok, _} = httpc:request(post, {"https://events.launchdarkly.com/bulk", [
        {"Authorization", os:getenv("LD_SDK_KEY")},
        {"X-LaunchDarkly-Event-Schema", ldclient_config:get_event_schema()},
        {"User-Agent", ldclient_config:get_user_agent()}],
        "application/json",
        <<"[{\"endDate\":1634839284004,\"features\":{\"test-boolean-flag\":{\"counters\":[{\"count\":1,\"unknown\":false,\"value\":true,\"variation\":0,\"version\":1}],\"default\":\"default-value\"}},\"kind\":\"summary\",\"startDate\":1634839284004},{\"creationDate\":1634839284004,\"kind\":\"index\",\"user\":{\"firstName\":\"Tester\",\"key\":\"12345-track\",\"lastName\":\"Testerson\",\"privateAttrs\":[]}}]">>
    }, Options, []).

check_secure_default_httpc_valid(_) ->
    Options = [{ssl, ldclient_config:tls_basic_options()}],
    {ok, _} = httpc:request(get, {"https://tls-v1-2.badssl.com:1012/", []}, Options, []),
    {ok, _} = httpc:request(get, {"https://sha256.badssl.com/", []}, Options, []),
    {ok, _} = httpc:request(get, {"https://rsa2048.badssl.com/", []}, Options, []),
    {ok, _} = httpc:request(get, {"https://ecc256.badssl.com/", []}, Options, []),
    {ok, _} = httpc:request(get, {"https://ecc384.badssl.com/", []}, Options, []),
    {ok, _} = httpc:request(get, {"https://extended-validation.badssl.com/", []}, Options, []),
    {ok, _} = httpc:request(get, {"https://mozilla-modern.badssl.com/", []}, Options, []).

check_secure_default_httpc_invalid(_) ->
    Options = [{ssl, ldclient_config:tls_basic_options()}],
    {error, _} = httpc:request(get, {"https://expired.badssl.com/", []}, Options, []),
    {error, _} = httpc:request(get, {"https://wrong.host.badssl.com/", []}, Options, []),
    {error, _} = httpc:request(get, {"https://self-signed.badssl.com/", []}, Options, []),
    {error, _} = httpc:request(get, {"https://self-signed.badssl.com/", []}, Options, []).

check_secure_default_httpc_interception(_) ->
    Options = [{ssl, ldclient_config:tls_basic_options()}],
    {error, _} = httpc:request(get, {"https://superfish.badssl.com/", []}, Options, []),
    {error, _} = httpc:request(get, {"https://edellroot.badssl.com/", []}, Options, []),
    {error, _} = httpc:request(get, {"https://dsdtestprovider.badssl.com/", []}, Options, []),
    {error, _} = httpc:request(get, {"https://preact-cli.badssl.com/", []}, Options, []),
    {error, _} = httpc:request(get, {"https://webpack-dev-server.badssl.com/", []}, Options, []).

check_secure_default_httpc_broken_crypto(_) ->
    Options = [{ssl, ldclient_config:tls_basic_options()}],
    {error, _} = httpc:request(get, {"https://rc4.badssl.com/", []}, Options, []),
    {error, _} = httpc:request(get, {"https://rc4-md5.badssl.com/", []}, Options, []),
    {error, _} = httpc:request(get, {"https://dh480.badssl.com/", []}, Options, []),
    {error, _} = httpc:request(get, {"https://dh512.badssl.com/", []}, Options, []),
    {error, _} = httpc:request(get, {"https://dh1024.badssl.com/", []}, Options, []),
    {error, _} = httpc:request(get, {"https://null.badssl.com/", []}, Options, []).

check_secure_default_httpc_legacy(_) ->
    Options = [{ssl, ldclient_config:tls_basic_options()}],
    {error, _} = httpc:request(get, {"https://tls-v1-0.badssl.com:1010/", []}, Options, []),
    {error, _} = httpc:request(get, {"https://tls-v1-1.badssl.com:1011/", []}, Options, []),
    {error, _} = httpc:request(get, {"https://3des.badssl.com/", []}, Options, []),
    {error, _} = httpc:request(get, {"https://dh2048.badssl.com/", []}, Options, []).

check_secure_default_httpc_revocation(_) ->
    Options = [{ssl, ldclient_config:with_tls_revocation(ldclient_config:tls_basic_options())}],
    %% This test can pass even without the revocation configuration. But this helps to ensure the
    %% configuration doesn't prevent it from working.
    {error, _} = httpc:request(get, {"https://revoked.badssl.com/", []}, Options, []).

check_secure_default_shotgun_invalid(_) ->
    {error, gun_open_failed, "Could not open connection to host"} = open_stream("https://expired.badssl.com/", #{
        tls_options => ldclient_config:tls_basic_options(),
        connect_timeout => undefined,
        custom_headers => undefined
    }).
