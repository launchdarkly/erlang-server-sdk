%%%-------------------------------------------------------------------
%%% @doc `eld' module
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(eld).

%% API
-export([start_stream/1]).
-export([start_stream/2]).
-export([stop_all_streams/0]).

%% Constants
-define(DEFAULT_BASE_URI, "https://app.launchdarkly.com").
-define(DEFAULT_EVENTS_URI, "https://events.launchdarkly.com").
-define(DEFAULT_STREAM_URI, "https://events.launchdarkly.com").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts streamer client process with given SDK key
%%
%% Host and port will be taken from defaults.
%% @end
-spec start_stream(SdkKey :: string()) -> ok | {error, atom(), term()}.
start_stream(SdkKey) ->
    start_stream(SdkKey, ?DEFAULT_STREAM_URI).

%% @doc Starts streamer client process with given SDK key and URI
%%
%% @end
-spec start_stream(SdkKey :: string(), Uri :: http_uri:uri()) -> ok | {error, atom(), term()}.
start_stream(SdkKey, Uri) ->
    {ok, Pid} = supervisor:start_child(eld_stream_sup, [SdkKey]),
    ok = eld_stream_server:listen(Pid, Uri).

%% @doc Terminates all current stream listeners
%%
%% @end
-spec stop_all_streams() -> ok.
stop_all_streams() ->
    ok = terminate_all_children(eld_stream_sup).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Terminates all children of a given supervisor
%%
%% @end
-spec terminate_all_children(Sup :: atom()) -> ok.
terminate_all_children(Sup) ->
    Pids = [Pid || {_, Pid, worker, _} <- supervisor:which_children(eld_stream_sup)],
    terminate_all_children(Sup, Pids).

%% @doc Recursively terminates processes of given children Pids
%% @private
%%
%% @end
-spec terminate_all_children(Sup :: atom(), [pid()]) -> ok.
terminate_all_children(_Sup, []) ->
    ok;
terminate_all_children(Sup, [Pid|Rest]) ->
    ok = supervisor:terminate_child(Sup, Pid),
    terminate_all_children(Sup, Rest).
