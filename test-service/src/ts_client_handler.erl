%-------------------------------------------------------------------
%% @doc `ts_client_handler' module
%%
%% Handle requests for creating and destroying clients.
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ts_client_handler).
-author("rlamb").

%% API
-export([create_client/2]).
-export([delete_client/1]).

-spec wait_init(Tag :: atom(), Initialized :: boolean(),
    StartedWaiting :: pos_integer(), WaitTime :: pos_integer()) -> ok.
wait_init(Tag, false, StartedWaiting, WaitTime) ->
    ElapsedTime = erlang:system_time(milli_seconds) - StartedWaiting,
    wait_init(Tag, ldclient:initialized(Tag) or (ElapsedTime >= WaitTime), StartedWaiting, WaitTime);
wait_init(_Tag, true, _StartedWaiting, _WaitTime) -> ok.

-spec create_client(Tag :: atom(),
    Params :: ts_service_params:create_instance_params()) -> ok.
create_client(Tag,
    #{configuration := #{credential := Credential,
        start_wait_time_ms := StartWaitTimeMs} = Configuration} = _Params) ->
    Options = ts_sdk_config_params:to_ldclient_options(Configuration),
    ok = ldclient:start_instance(binary_to_list(Credential), Tag, Options),
    wait_init(Tag, ldclient:initialized(Tag), erlang:system_time(milli_seconds), StartWaitTimeMs),
    ok.

-spec delete_client(Tag :: atom()) -> ok.
delete_client(Tag) ->
    ldclient:stop_instance(Tag),
    ok.
