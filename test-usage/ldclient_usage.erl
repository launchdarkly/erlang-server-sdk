%%-------------------------------------------------------------------
%% @doc `ldclient_usage' module
%% @private
%% @end
%%-------------------------------------------------------------------

%% This file contains top level usage of ldclient to allow for dialyzing
%% these methods. If the methods were only used in tests, then issue with
%% their usage would not be reported.

-module(ldclient_usage).

-export([
    init_with_all_options/0,
    use_variation_with_all_types_context/0
]).

init_with_all_options() ->
    ldclient:start_instance("sdk-key", instance_name, #{
        base_uri => "",
        stream_uri => "",
        feature_store => ldclient_storage_map,
        events_capacity => 10,
        events_flush_interval => 5000,
        events_dispatcher => ldclient_event_dispatch_test,
        context_keys_capacity => 1,
        private_attributes => all,
        stream => false,
        polling_interval => 5000,
        polling_update_requestor => ldclient_update_requestor_httpc,
        offline => true,
        redis_host => "redis_host",
        redis_port => 9900,
        redis_database => 0,
        redis_password => "password",
        redis_prefix => "prefix",
        use_ldd => false,
        cache_ttl => 1000,
        send_events => false,
        file_datasource => true,
        file_paths => ["example1.json", "example2.yml"],
        file_auto_update => true,
        file_poll_interval => 1000,
        file_allow_duplicate_keys => true,
        http_options => #{
            tls_options => ldclient_config:tls_basic_options(),
            connect_timeout => 12345,
            custom_headers => [{"example-header-1", "heaver-value"}]
        }
    }).

use_variation_with_all_types_context() ->
    ldclient:variation(<<"flag-key">>,
        ldclient_context:set_private_attributes([<<"binary">>],
        ldclient_context:set(<<"binary">>, <<"myString">>,
        ldclient_context:set(<<"integer">>, 14,
        ldclient_context:set(<<"float">>, 3.14,
        ldclient_context:set(<<"boolean">>, false,
        ldclient_context:set(<<"array">>, [true, 14, 3.14, <<"myString">>, #{<<"a">> => <<"b">>}, [<<"arr">>]],
        ldclient_context:set(<<"object">>, #{
            <<"binary">> => <<"myString">>,
            <<"integer">> => <<"myString">>,
            <<"float">> => <<"myString">>,
            <<"boolean">> => <<"myString">>,
            <<"array">> => [<<"arr">>],
            <<"object">> => #{<<"a">> => <<"b">>}
        },
        ldclient_context:new(<<"org-key">>, <<"org">>)))))))), false).

