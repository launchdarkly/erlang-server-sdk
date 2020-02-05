%%-------------------------------------------------------------------
%% @doc `eld_update_requestor' module
%%
%% This is a behavior that event dispatchers must implement. It is used to send
%% event batches to LaunchDarkly.
%% @end
%%-------------------------------------------------------------------

-module(eld_update_requestor).

-type response() :: {ok, binary() | not_modified}
                  | {error, errors()}.

-type errors() :: {bad_status, httpc:status_code(), string()}
                | network_error.

-export_type([response/0, errors/0]).

%% `all' must request and return all flags and segments, along with an updated
%% state value. It takes the destination URI, the SDK key, along with a state
%% value from `init' or the previous invocation of `all' to allow for features
%% such as ETag caching.
-callback all(Uri :: string(), SdkKey :: string(), State :: any()) -> {response(), any()}.

%% `init' should return an initial value for the `State' argument to `all'
-callback init() -> any().
