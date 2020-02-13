-module(eld_update_requestor_httpc_SUITE).

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
    no_server_uri_test/1
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
        no_server_uri_test
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_) ->
    ok.

init_per_testcase(_, Config) ->
    {ok, _} = bookish_spork:start_server(),
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
    bookish_spork:stub_request([200, #{}, <<>>]),
    {{ok, <<>>}, #{}} = eld_update_requestor_httpc:all(?MOCK_URI, "sdk-key", #{}),
    {ok, Request} = bookish_spork:capture_request(),
    "sdk-key" = bookish_spork_request:header(Request, "authorization").

user_agent_header_set_on_request(_Config) ->
    bookish_spork:stub_request([200, #{}, <<>>]),
    {{ok, <<>>}, #{}} = eld_update_requestor_httpc:all(?MOCK_URI, "sdk-key", #{}),
    {ok, Request} = bookish_spork:capture_request(),
    UserAgent = bookish_spork_request:header(Request, "user-agent"),
    true = string:prefix(UserAgent, "ErlangClient") /= nomatch.

event_schema_set_on_request(_Config) ->
    bookish_spork:stub_request([200, #{}, <<>>]),
    {{ok, <<>>}, #{}} = eld_update_requestor_httpc:all(?MOCK_URI, "sdk-key", #{}),
    {ok, Request} = bookish_spork:capture_request(),
    "3" = bookish_spork_request:header(Request, "x-launchdarkly-event-schema").

none_match_is_not_set_with_empty_state(_Config) ->
    bookish_spork:stub_request([200, #{}, <<>>]),
    {{ok, <<>>}, #{}} = eld_update_requestor_httpc:all(?MOCK_URI, "sdk-key", #{}),
    {ok, Request} = bookish_spork:capture_request(),
    nil = bookish_spork_request:header(Request, "if-none-match").

none_match_is_set_with_state(_Config) ->
    bookish_spork:stub_request([200, #{}, <<>>]),
    State = #{?MOCK_URI => "etagval"},
    {{ok, <<>>}, State} = eld_update_requestor_httpc:all(?MOCK_URI, "sdk-key", State),
    {ok, Request} = bookish_spork:capture_request(),
    "etagval" = bookish_spork_request:header(Request, "if-none-match").

etag_response_recorded(_Config) ->
    bookish_spork:stub_request([200, #{<<"etag">> => <<"etagval">>}, <<>>]),
    {{ok, <<>>}, #{?MOCK_URI := "etagval"}} = eld_update_requestor_httpc:all(?MOCK_URI, "sdk-key", #{}).

not_modified_test(_Config) ->
    bookish_spork:stub_request([304, #{}, <<>>]),
    {{ok, not_modified}, #{}} = eld_update_requestor_httpc:all(?MOCK_URI, "sdk-key", #{}).

etag_updated_on_modified(_Config) ->
    bookish_spork:stub_request([200, #{<<"etag">> => <<"etagval2">>}, <<>>]),
    {{ok, <<>>}, State} = eld_update_requestor_httpc:all(?MOCK_URI, "sdk-key", #{?MOCK_URI => "etagval1"}),
    #{?MOCK_URI := "etagval2"} = State.

error_status_returned(_Config) ->
    bookish_spork:stub_request([504, #{}, <<>>]),
    {Response, #{}} = eld_update_requestor_httpc:all(?MOCK_URI, "sdk-key", #{}),
    {error, {bad_status, 504, _FormattedResponse}} = Response.

invalid_uri_test(_Config) ->
    {Response, #{}} = eld_update_requestor_httpc:all(?INVALID_URI, "sdk-key", #{}),
    {error, network_error} = Response.

no_server_uri_test(_Config) ->
    {Response, #{}} = eld_update_requestor_httpc:all(?NO_SERVER_URI, "sdk-key", #{}),
    {error, network_error} = Response.
