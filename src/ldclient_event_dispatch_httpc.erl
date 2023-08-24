%%-------------------------------------------------------------------
%% @doc Event dispatcher
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_event_dispatch_httpc).

-behaviour(ldclient_event_dispatch).

%% Behavior callbacks
-export([init/2, send/4]).

%% Internal type for ETag cache state
-type state() :: #{
headers => list(),
http_options => list()
}.

%% Expose non-exported methods for tests.
-ifdef(TEST).
-compile(export_all).
-endif.

%%===================================================================
%% Behavior callbacks
%%===================================================================

-spec init(Tag :: atom(), SdkKey :: string()) -> state().
init(Tag, _SdkKey) ->
    Options = ldclient_config:get_value(Tag, http_options),
    HttpOptions = ldclient_http_options:httpc_parse_http_options(Options),
    DefaultHeaders = ldclient_headers:get_default_headers(Tag, string_pairs),
    Headers = ldclient_http_options:httpc_append_custom_headers([
        {"X-LaunchDarkly-Event-Schema", ldclient_config:get_event_schema()}
        | DefaultHeaders
    ], Options),
    #{
        headers => Headers,
        http_options => HttpOptions
    }.

%% @doc Send events to LaunchDarkly event server
%%
%% @end
-spec send(State :: state(), JsonEvents :: binary(), PayloadId :: uuid:uuid(), Uri :: string()) ->
    {ok, integer()} | {error, temporary, string()} | {error, permanent, string()}.
send(State, JsonEvents, PayloadId, Uri) ->
    #{headers := BaseHeaders, http_options := HttpOptions} = State,
    Headers = [
        {"X-LaunchDarkly-Payload-ID", uuid:uuid_to_string(PayloadId)} |
        BaseHeaders
    ],
    Request = httpc:request(post, {Uri, Headers, "application/json", JsonEvents}, HttpOptions, []),
    process_request(Request).

%%===================================================================
%% Internal functions
%%===================================================================

-type http_request() :: {ok, {{string(), integer(), string()}, [{string(), string()}], string() | binary()}}.

-spec process_request({error, term()} | http_request()) 
    -> {ok, integer()} | {error, temporary, string()} | {error, permanent, string()}.
process_request({error, Reason}) ->
    {error, temporary, Reason};
process_request({ok, {{_Version, StatusCode, _ReasonPhrase}, Headers, _Body}}) when StatusCode < 400 ->
    {ok, get_server_time(Headers)};
process_request({ok, {{Version, StatusCode, ReasonPhrase}, _Headers, _Body}}) ->
    Reason = format_response(Version, StatusCode, ReasonPhrase),
    HttpErrorType = ldclient_http:is_http_error_code_recoverable(StatusCode),
    {error, HttpErrorType, Reason}.

-spec format_response(Version :: string(), StatusCode :: integer(), ReasonPhrase :: string()) ->
    string().
format_response(Version, StatusCode, ReasonPhrase) ->
    io_lib:format("~s ~b ~s", [Version, StatusCode, ReasonPhrase]).

%% Get the server time, and if there is not time, then return 0.
-spec get_server_time(Headers :: [{Field :: [byte()], Value :: binary() | iolist()}]) -> integer().
get_server_time([{"date", Date}|_T]) when is_list(Date) ->
    %% convert_request_date expects a string that is a list of characters.
    %% Not a binary string. The guard can make sure it is a list, but not
    %% that it is a char list. So that gets checked here.
    case io_lib:char_list(Date) of
        true -> case httpd_util:convert_request_date(Date) of
                    bad_date ->
                        %% This would be a date in a bad format.
                        0;
                    ParsedDate ->
                        ldclient_time:datetime_to_timestamp(ParsedDate)
                end;
        false ->
            %% The date was a list, but was not a list of characters.
            0
    end;
get_server_time([_H|T]) ->
    get_server_time(T);
get_server_time(_) -> 0.
