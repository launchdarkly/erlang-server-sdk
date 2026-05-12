-module(ldclient_headers_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    can_sort_tags/1,
    can_combine_tags/1,
    can_get_default_headers/1,
    instance_id_header_is_uuid_v4/1,
    instance_id_header_is_stable_across_calls/1,
    instance_id_header_differs_between_instances/1,
    instance_id_header_in_string_pairs_format/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        can_sort_tags,
        can_combine_tags,
        can_get_default_headers,
        instance_id_header_is_uuid_v4,
        instance_id_header_is_stable_across_calls,
        instance_id_header_differs_between_instances,
        instance_id_header_in_string_pairs_format
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
%% Tests
%%====================================================================

can_sort_tags(_) ->
    %% Works fine with empty list of tags.
    [] = ldclient_headers:sort_tags([]),
    %% Sorts tags and their values.
    [
        {<<"a">>, [<<"3">>, <<"7">>, <<"9">>]},
        {<<"b">>, [<<"1">>, <<"2">>, <<"3">>]},
        {<<"c">>, [<<"1">>, <<"4">>, <<"5">>]}
    ] = ldclient_headers:sort_tags([
        {<<"b">>, [<<"2">>, <<"1">>, <<"3">>]},
        {<<"c">>, [<<"5">>, <<"4">>, <<"1">>]},
        {<<"a">>, [<<"7">>, <<"9">>, <<"3">>]}
    ]).

can_combine_tags(_) ->
    <<"a/3 a/7 a/9 b/1 b/2 b/3 c/1 c/4 c/5">> = ldclient_headers:combine_tags([
        {<<"a">>, [<<"3">>, <<"7">>, <<"9">>]},
        {<<"b">>, [<<"1">>, <<"2">>, <<"3">>]},
        {<<"c">>, [<<"1">>, <<"4">>, <<"5">>]}
    ]).

can_get_default_headers(_) ->
    {ok, _} = application:ensure_all_started(ldclient),
    OptionsWithAllTags = #{
        stream => false,
        polling_update_requestor => ldclient_update_requestor_test,
        application => #{
            id => <<"the-id">>,
            version => <<"the-version">>
        }
    },
    ldclient:start_instance("an-sdk-key", version_and_id, OptionsWithAllTags),
    UserAgent = list_to_binary(ldclient_config:get_user_agent()),
    #{
        <<"authorization">> := <<"an-sdk-key">>,
        <<"user-agent">> := UserAgent,
        <<"x-launchdarkly-tags">> := <<"application-id/the-id application-version/the-version">>
    } = ldclient_headers:get_default_headers(version_and_id),

    OptionsWithIdOnly = #{
        stream => false,
        polling_update_requestor => ldclient_update_requestor_test,
        application => #{
            id => <<"the-id">>
        }
    },
    ldclient:start_instance("an-sdk-key", id_only, OptionsWithIdOnly),
    UserAgent = list_to_binary(ldclient_config:get_user_agent()),
    #{
        <<"authorization">> := <<"an-sdk-key">>,
        <<"user-agent">> := UserAgent,
        <<"x-launchdarkly-tags">> := <<"application-id/the-id">>
    } = ldclient_headers:get_default_headers(id_only),

    OptionsWithVersionOnly = #{
        stream => false,
        polling_update_requestor => ldclient_update_requestor_test,
        application => #{
            version => <<"the-version">>
        }
    },
    ldclient:start_instance("an-sdk-key", version_only, OptionsWithVersionOnly),
    UserAgent = list_to_binary(ldclient_config:get_user_agent()),
    #{
        <<"authorization">> := <<"an-sdk-key">>,
        <<"user-agent">> := UserAgent,
        <<"x-launchdarkly-tags">> := <<"application-version/the-version">>
    } = ldclient_headers:get_default_headers(version_only),
    OptionsWithoutApplication = #{
        stream => false,
        polling_update_requestor => ldclient_update_requestor_test
    },
    ldclient:start_instance("an-sdk-key", no_application, OptionsWithoutApplication),
    UserAgent = list_to_binary(ldclient_config:get_user_agent()),
    #{
        <<"authorization">> := <<"an-sdk-key">>,
        <<"user-agent">> := UserAgent
    } = ldclient_headers:get_default_headers(no_application),

    OptionsEmptyApplication = #{
        stream => false,
        polling_update_requestor => ldclient_update_requestor_test,
        application => #{}
        },
    ldclient:start_instance("an-sdk-key", empty_application, OptionsEmptyApplication),
    UserAgent = list_to_binary(ldclient_config:get_user_agent()),
    #{
        <<"authorization">> := <<"an-sdk-key">>,
        <<"user-agent">> := UserAgent
    } = ldclient_headers:get_default_headers(empty_application),

    application:stop(ldclient).

instance_id_header_is_uuid_v4(_) ->
    %% Every call to get_default_headers must produce an X-LaunchDarkly-Instance-Id header
    %% containing a parseable v4 UUID. This is the spec contract for
    %% SCMP-server-connection-minutes-polling section 1.1.
    {ok, _} = application:ensure_all_started(ldclient),
    ldclient:start_instance("an-sdk-key", instance_id_v4, #{
        stream => false,
        polling_update_requestor => ldclient_update_requestor_test
    }),
    Headers = ldclient_headers:get_default_headers(instance_id_v4),
    InstanceId = maps:get(<<"x-launchdarkly-instance-id">>, Headers),
    true = is_binary(InstanceId),
    %% A standard UUID string is 36 characters: 8-4-4-4-12 hex with dashes.
    36 = byte_size(InstanceId),
    %% uuid:is_v4/1 takes the binary uuid representation. Parse the string
    %% form back to that representation before validating the version bits.
    Parsed = uuid:string_to_uuid(binary_to_list(InstanceId)),
    true = uuid:is_v4(Parsed),
    application:stop(ldclient).

instance_id_header_is_stable_across_calls(_) ->
    %% The GUID must remain constant throughout the lifetime of the SDK
    %% instance, so two successive calls to get_default_headers for the same
    %% tag must yield the same value.
    {ok, _} = application:ensure_all_started(ldclient),
    ldclient:start_instance("an-sdk-key", instance_id_stable, #{
        stream => false,
        polling_update_requestor => ldclient_update_requestor_test
    }),
    H1 = ldclient_headers:get_default_headers(instance_id_stable),
    H2 = ldclient_headers:get_default_headers(instance_id_stable),
    Id1 = maps:get(<<"x-launchdarkly-instance-id">>, H1),
    Id2 = maps:get(<<"x-launchdarkly-instance-id">>, H2),
    Id1 = Id2,
    application:stop(ldclient).

instance_id_header_differs_between_instances(_) ->
    %% Different SDK instances must get different GUIDs.
    {ok, _} = application:ensure_all_started(ldclient),
    ldclient:start_instance("an-sdk-key", instance_id_a, #{
        stream => false,
        polling_update_requestor => ldclient_update_requestor_test
    }),
    ldclient:start_instance("an-sdk-key", instance_id_b, #{
        stream => false,
        polling_update_requestor => ldclient_update_requestor_test
    }),
    IdA = maps:get(<<"x-launchdarkly-instance-id">>,
        ldclient_headers:get_default_headers(instance_id_a)),
    IdB = maps:get(<<"x-launchdarkly-instance-id">>,
        ldclient_headers:get_default_headers(instance_id_b)),
    true = IdA =/= IdB,
    application:stop(ldclient).

instance_id_header_in_string_pairs_format(_) ->
    %% The string_pairs format is used by httpc-based clients (polling and
    %% events). Confirm the instance id header survives the binary->string
    %% conversion and the value is still a 36-character UUID string.
    {ok, _} = application:ensure_all_started(ldclient),
    ldclient:start_instance("an-sdk-key", instance_id_string_pairs, #{
        stream => false,
        polling_update_requestor => ldclient_update_requestor_test
    }),
    Pairs = ldclient_headers:get_default_headers(instance_id_string_pairs, string_pairs),
    {value, {_, InstanceIdStr}} = lists:search(
        fun({K, _V}) -> K =:= "x-launchdarkly-instance-id" end,
        Pairs),
    true = is_list(InstanceIdStr),
    36 = length(InstanceIdStr),
    true = uuid:is_v4(uuid:string_to_uuid(InstanceIdStr)),
    application:stop(ldclient).
