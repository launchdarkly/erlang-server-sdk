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
-export([stop_all_instances/0]).
-export([stop_instance/0]).
-export([stop_instance/1]).
-export([evaluate/3]).
-export([evaluate/4]).
-export([identify/1]).
-export([identify/2]).
-export([track/3]).
-export([track/4]).

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

%% @doc Stop all client instances
%%
%% @end
-spec stop_all_instances() -> ok.
stop_all_instances() ->
    eld_instance:stop_all().

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
    {Detail, Events} = eld_eval:flag_key_for_user(Tag, FlagKey, User, DefaultValue),
    % Send events
    SendEventsFun = fun(Event) -> eld_event_server:add_event(Tag, Event) end,
    lists:foreach(SendEventsFun, Events),
    % Return evaluation detail
    Detail.

%% @doc Identify reports details about a user
%%
%% This function uses the default client instance.
%% @end
-spec identify(User :: eld_user:user()) -> ok.
identify(User) ->
    identify(?DEFAULT_INSTANCE_NAME, User).

%% @doc Identify reports details about a user
%%
%% This is useful to report user to a specific client instance.
%% @end
-spec identify(Tag :: atom(), User :: eld_user:user()) -> ok.
identify(Tag, User) when is_atom(Tag) ->
    Event = eld_event:new_identify(User),
    eld_event_server:add_event(Tag, Event).

%% @doc Track reports that a user has performed an event
%%
%% Custom data can be attached to the event.
%% @end
-spec track(Key :: binary(), User :: eld_user:user(), Data :: map()) -> ok.
track(Key, User, Data) when is_binary(Key), is_map(Data) ->
    track(?DEFAULT_INSTANCE_NAME, Key, User, Data).

%% @doc Track reports that a user has performed an event
%%
%% This is useful for specifying a specific client instance.
%% @end
-spec track(Tag :: atom(), Key :: binary(), User :: eld_user:user(), Data :: map()) -> ok.
track(Tag, Key, User, Data) when is_atom(Tag), is_binary(Key), is_map(Data) ->
    Event = eld_event:new_custom(Key, User, Data),
    eld_event_server:add_event(Tag, Event).
