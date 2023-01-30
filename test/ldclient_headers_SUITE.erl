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
    can_get_default_headers/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        can_sort_tags,
        can_combine_tags,
        can_get_default_headers
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
