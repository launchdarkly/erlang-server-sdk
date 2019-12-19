%%-------------------------------------------------------------------
%% @doc `eld_update_requestor' module
%%
%% This is a behavior that event dispatchers must implement. It is used to send
%% event batches to LaunchDarkly.
%% @end
%%-------------------------------------------------------------------

-module(eld_update_requestor).

-type response() :: {ok, binary()} | {error, httpc:status_code(), string()}.

-export_type([response/0]).

%% `all' must request and return all flags and segments. It takes the
%% destination URI and SDK key.
-callback all(Uri :: string(), SdkKey :: string()) -> response().
