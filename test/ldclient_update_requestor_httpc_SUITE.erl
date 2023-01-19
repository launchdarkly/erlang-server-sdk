-module(ldclient_update_requestor_httpc_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    authorization_header_set_on_request/1,
    user_agent_header_set_on_request/1,
    event_schema_set_on_request/1,
    none_match_is_not_set_with_empty_state/1,
    none_match_is_set_with_state/1,
    etag_response_recorded/1,
    not_modified_test/1,
    etag_updated_on_modified/1,
    error_status_returned/1,
    invalid_uri_test/1,
    no_server_uri_test/1,
    custom_headers_appended/1
]).

all() ->
    [
        authorization_header_set_on_request,
        user_agent_header_set_on_request,
        event_schema_set_on_request,
        none_match_is_not_set_with_empty_state,
        none_match_is_set_with_state,
        etag_response_recorded,
        not_modified_test,
        etag_updated_on_modified,
        error_status_returned,
        invalid_uri_test,
        no_server_uri_test,
        custom_headers_appended
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_) ->
    ok.

init_per_testcase(_, Config) ->
    {ok, _} = bookish_spork:start_server(),
    Settings = ldclient_config:parse_options("sdk-key", #{}),
    ok = ldclient_config:register(default, Settings),
    CustomSettings = ldclient_config:parse_options("sdk-key", #{
        http_options => #{
            custom_headers => [
                {"Basic-String-Header", "String"},
                {"Binary-String-Header", "Binary"}
            ]}
    }),
    ok = ldclient_config:register(custom, CustomSettings),
    Config.

end_per_testcase(_, _Config) ->
    bookish_spork:stop_server().

%%====================================================================
%% Helpers
%%====================================================================

-define(MOCK_URI, "http://localhost:32002").
-define(INVALID_URI, "abc://1.4:67000").
% Assigned as TEST-NET-1, unused on net.
-define(NO_SERVER_URI, "http://192.0.2.1").

%%====================================================================
%% Tests
%%====================================================================

authorization_header_set_on_request(_Config) ->
    State = ldclient_update_requestor_httpc:init(default, "sdk-key"),
    bookish_spork:stub_request([200, #{}, <<>>]),
    {{ok, <<>>}, _} = ldclient_update_requestor_httpc:all(?MOCK_URI, State),
{ok, Request} = bookish_spork:capture_request(),
    "sdk-key" = bookish_spork_request:header(Request, "authorization").

user_agent_header_set_on_request(_Config) ->
    State = ldclient_update_requestor_httpc:init(default, "sdk-key"),
    bookish_spork:stub_request([200, #{}, <<>>]),
    {{ok, <<>>}, _} = ldclient_update_requestor_httpc:all(?MOCK_URI, State),
    {ok, Request} = bookish_spork:capture_request(),
    UserAgent = bookish_spork_request:header(Request, "user-agent"),
    true = string:prefix(UserAgent, "ErlangClient") /= nomatch.

event_schema_set_on_request(_Config) ->
    State = ldclient_update_requestor_httpc:init(default, "sdk-key"),
    bookish_spork:stub_request([200, #{}, <<>>]),
    {{ok, <<>>}, _} = ldclient_update_requestor_httpc:all(?MOCK_URI, State),
    {ok, Request} = bookish_spork:capture_request(),
    "4" = bookish_spork_request:header(Request, "x-launchdarkly-event-schema").

none_match_is_not_set_with_empty_state(_Config) ->
    State = ldclient_update_requestor_httpc:init(default, "sdk-key"),
    bookish_spork:stub_request([200, #{}, <<>>]),
    {{ok, <<>>}, _} = ldclient_update_requestor_httpc:all(?MOCK_URI, State),
    {ok, Request} = bookish_spork:capture_request(),
    nil = bookish_spork_request:header(Request, "if-none-match").

none_match_is_set_with_state(_Config) ->
    BaseState = ldclient_update_requestor_httpc:init(default, "sdk-key"),
    bookish_spork:stub_request([200, #{}, <<>>]),
    State = BaseState#{etag_state => #{?MOCK_URI => "etagval"}},
    {{ok, <<>>}, State} = ldclient_update_requestor_httpc:all(?MOCK_URI, State),
    {ok, Request} = bookish_spork:capture_request(),
    "etagval" = bookish_spork_request:header(Request, "if-none-match").

etag_response_recorded(_Config) ->
    State = ldclient_update_requestor_httpc:init(default, "sdk-key"),
    bookish_spork:stub_request([200, #{<<"etag">> => <<"etagval">>}, <<>>]),
    {{ok, <<>>}, #{etag_state := #{?MOCK_URI := "etagval"}}} = ldclient_update_requestor_httpc:all(?MOCK_URI, State).

not_modified_test(_Config) ->
    State = ldclient_update_requestor_httpc:init(default, "sdk-key"),
    bookish_spork:stub_request([304, #{}, <<>>]),
    {{ok, not_modified}, State} = ldclient_update_requestor_httpc:all(?MOCK_URI, State).

etag_updated_on_modified(_Config) ->
    BaseState = ldclient_update_requestor_httpc:init(default, "sdk-key"),
    bookish_spork:stub_request([200, #{<<"etag">> => <<"etagval2">>}, <<>>]),
    State = BaseState#{etag_state => #{?MOCK_URI => "etagval"}},
    {{ok, <<>>}, UpdatedState} = ldclient_update_requestor_httpc:all(?MOCK_URI, State),
    #{etag_state := #{?MOCK_URI := "etagval2"}} = UpdatedState.

error_status_returned(_Config) ->
    State = ldclient_update_requestor_httpc:init(default, "sdk-key"),
    bookish_spork:stub_request([504, #{}, <<>>]),
    {Response, State} = ldclient_update_requestor_httpc:all(?MOCK_URI, State),
    {error, {bad_status, 504, _FormattedResponse}} = Response.

invalid_uri_test(_Config) ->
    State = ldclient_update_requestor_httpc:init(default, "sdk-key"),
    {Response, State} = ldclient_update_requestor_httpc:all(?INVALID_URI, State),
    {error, network_error} = Response.

no_server_uri_test(_Config) ->
    State = ldclient_update_requestor_httpc:init(default, "sdk-key"),
    {Response, State} = ldclient_update_requestor_httpc:all(?NO_SERVER_URI, State),
    {error, network_error} = Response.

custom_headers_appended(_Config) ->
    State = ldclient_update_requestor_httpc:init(custom, "sdk-key"),
    bookish_spork:stub_request([200, #{}, <<>>]),
    {{ok, <<>>}, State} = ldclient_update_requestor_httpc:all(?MOCK_URI, State),
    {ok, Request} = bookish_spork:capture_request(),
    %% Includes non-custom headers.
    "sdk-key" = bookish_spork_request:header(Request, "authorization"),
    %% The custom headers are there as well.
    "String" = bookish_spork_request:header(Request, "basic-string-header"),
    "Binary" = bookish_spork_request:header(Request, "binary-string-header").
