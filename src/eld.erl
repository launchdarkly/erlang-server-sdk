%%-------------------------------------------------------------------
%% @doc `eld' module
%%
%% Acts as an interface to most common SDK functions: starting and stopping
%% client instances, and evaluating feature flags for users.
%% @end
%%-------------------------------------------------------------------

-module(eld).

%% API
-export([start_instance/1]).
-export([start_instance/2]).
-export([start_instance/3]).
-export([stop_instance/0]).
-export([stop_instance/1]).
-export([evaluate/3]).
-export([evaluate/4]).

%% Constants
-define(DEFAULT_INSTANCE_NAME, default).

%%===================================================================
%% API
%%===================================================================

%% @doc Start client with default options
%%
%% The SDK key is required to connect. Default streamer URL, storage backend
%% and instance name `default' will be used.
%% @end
-spec start_instance(SdkKey :: string()) -> ok | {error, atom(), term()}.
start_instance(SdkKey) ->
    start_instance(SdkKey, ?DEFAULT_INSTANCE_NAME).

%% @doc Start client with custom name or options
%%
%% When `TagOrOptions' is an atom, the instance is started with that name. When
%% it's a map, it can be used to start with custom options.
%% @end
-spec start_instance(SdkKey :: string(), TagOrOptions :: atom() | map()) ->
    ok | {error, atom(), term()}.
start_instance(SdkKey, Tag) when is_list(SdkKey), is_atom(Tag) ->
    start_instance(SdkKey, Tag, #{});
start_instance(SdkKey, Options) when is_list(SdkKey), is_map(Options) ->
    start_instance(SdkKey, ?DEFAULT_INSTANCE_NAME, Options).

%% @doc Start client with custom name and options
%%
%% Specify both custom client name and options when starting the client.
%% @end
-spec start_instance(SdkKey :: string(), Tag :: atom(), Options :: map()) ->
    ok | {error, atom(), term()}.
start_instance(SdkKey, Tag, Options) when is_list(SdkKey), is_atom(Tag), is_map(Options) ->
    eld_instance:start(Tag, SdkKey, Options).

%% @doc Stop client instance
%%
%% Stops the default client instance.
%% @end
-spec stop_instance() -> ok | {error, not_found, term()}.
stop_instance() ->
    stop_instance(?DEFAULT_INSTANCE_NAME).

%% @doc Stop client with the custom name
%%
%% This is useful if a client instance was started with a custom name.
%% @end
-spec stop_instance(Tag :: atom()) -> ok | {error, not_found, term()}.
stop_instance(Tag) when is_atom(Tag) ->
    eld_instance:stop(Tag).

%% @doc Evaluate given flag key for given user
%%
%% Evaluation iterates through flag's prerequisites, targets, rules, associated
%% clauses and percentage rollouts. It returns the flag variation index, value
%% and reason, explaining why the specific result was chosen.
%% @end
-spec evaluate(FlagKey :: binary(), User :: eld_user:user(), DefaultValue :: eld_flag:variation_value()) ->
    eld_eval:detail().
evaluate(FlagKey, User, DefaultValue) when is_binary(FlagKey), is_map(User) ->
    evaluate(?DEFAULT_INSTANCE_NAME, FlagKey, User, DefaultValue).

%% @doc Evaluate given flag key for given user and given client instance
%%
%% Evaluation iterates through flag's prerequisites, targets, rules, associated
%% clauses and percentage rollouts. It returns the flag variation index, value
%% and reason, explaining why the specific result was chosen.
%% @end
-spec evaluate(Tag :: atom(), FlagKey :: binary(), User :: eld_user:user(), DefaultValue :: eld_flag:variation_value()) ->
    eld_eval:detail().
evaluate(Tag, FlagKey, User, DefaultValue) when is_binary(FlagKey), is_map(User) ->
    % Get evaluation result detail
    {Detail, _Events} = eld_eval:flag_key_for_user(Tag, FlagKey, User, DefaultValue),
    % TODO Send all events
    % Return evaluation detail
    Detail.
