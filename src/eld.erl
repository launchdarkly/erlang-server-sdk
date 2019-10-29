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
-export([variation/3]).
-export([variation/4]).
-export([variation_detail/3]).
-export([variation_detail/4]).
-export([all_flags_state/1]).
-export([all_flags_state/2]).
-export([identify/1]).
-export([identify/2]).
-export([track/3]).
-export([track/4]).

%% Types
-type feature_flags_state() :: #{
    flag_values => #{binary() => any()}
}.

-export_type([feature_flags_state/0]).

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
%% Evaluates the flag and returns the resulting variation value. The default
%% value will be returned in case of any errors.
%% @end
-spec variation(FlagKey :: binary(), User :: eld_user:user(), DefaultValue :: eld_eval:result_value()) ->
    eld_eval:result_value().
variation(FlagKey, User, DefaultValue) when is_binary(FlagKey), is_map(User) ->
     variation(FlagKey, User, DefaultValue, ?DEFAULT_INSTANCE_NAME).

%% @doc Evaluate given flag key for given user and given client instance
%%
%% Evaluates the flag and returns the resulting variation value. The default
%% value will be returned in case of any errors.
%% @end
-spec variation(FlagKey :: binary(), User :: eld_user:user(), DefaultValue :: eld_eval:result_value(), Tag :: atom()) ->
    eld_eval:result_value().
variation(FlagKey, User, DefaultValue, Tag) when is_binary(FlagKey), is_map(User) ->
    % Get evaluation result detail
    {{_Index, Value, _Reason}, Events} = eld_eval:flag_key_for_user(Tag, FlagKey, User, DefaultValue),
    % Send events
    SendEventsFun = fun(Event) -> eld_event_server:add_event(Tag, Event, #{}) end,
    lists:foreach(SendEventsFun, Events),
    % Return evaluation result
    Value.

%% @doc Evaluate given flag key for given user
%%
%% Evaluates the flag and returns the result detail containing the variation
%% index, value, and reason why the specific result was chosen. The default
%% value will be returned in case of any errors.
%% @end
-spec variation_detail(FlagKey :: binary(), User :: eld_user:user(), DefaultValue :: eld_eval:result_value()) ->
    eld_eval:detail().
variation_detail(FlagKey, User, DefaultValue) when is_binary(FlagKey), is_map(User) ->
    variation_detail(FlagKey, User, DefaultValue, ?DEFAULT_INSTANCE_NAME).

%% @doc Evaluate given flag key for given user and given client instance
%%
%% Evaluates the flag and returns the result detail containing the variation
%% index, value, and reason why the specific result was chosen. The default
%% value will be returned in case of any errors.
%% @end
-spec variation_detail(FlagKey :: binary(), User :: eld_user:user(), DefaultValue :: eld_eval:result_value(), Tag :: atom()) ->
    eld_eval:detail().
variation_detail(FlagKey, User, DefaultValue, Tag) when is_binary(FlagKey), is_map(User) ->
    % Get evaluation result detail
    {Detail, Events} = eld_eval:flag_key_for_user(Tag, FlagKey, User, DefaultValue),
    % Send events
    SendEventsFun = fun(Event) -> eld_event_server:add_event(Tag, Event, #{include_reasons => true}) end,
    lists:foreach(SendEventsFun, Events),
    % Return evaluation detail
    Detail.

%% @doc Evaluate all flags for a given user and return their values
%%
%% Evaluates all existing flags, but does not create any events as a side
%% effect of the evaluation. It returns a map of flag keys to evaluated values.
%% @end
-spec all_flags_state(User :: eld_user:user()) -> feature_flags_state().
all_flags_state(User) ->
    all_flags_state(User, ?DEFAULT_INSTANCE_NAME).

%% @doc Evaluate all flags for a given user and given client instance
%%
%% Evaluates all existing flags, but does not create any events as a side
%% effect of the evaluation. It returns a map of flag keys to evaluated values.
%% @end
-spec all_flags_state(User :: eld_user:user(), Tag :: atom()) -> feature_flags_state().
all_flags_state(User, Tag) ->
    StorageBackend = eld_settings:get_value(Tag, storage_backend),
    AllFlags = [FlagKey || {FlagKey, _} <- StorageBackend:list(Tag, flags)],
    EvalFun = fun(FlagKey, Acc) ->
        {{_, V, _}, _Events} = eld_eval:flag_key_for_user(Tag, FlagKey, User, null),
        Acc#{FlagKey => V}
    end,
    #{flag_values => lists:foldl(EvalFun, #{}, AllFlags)}.

%% @doc Identify reports details about a user
%%
%% This function uses the default client instance.
%% @end
-spec identify(User :: eld_user:user()) -> ok.
identify(User) ->
    identify(User, ?DEFAULT_INSTANCE_NAME).

%% @doc Identify reports details about a user
%%
%% This is useful to report user to a specific client instance.
%% @end
-spec identify(User :: eld_user:user(), Tag :: atom()) -> ok.
identify(User, Tag) when is_atom(Tag) ->
    Event = eld_event:new_identify(User),
    eld_event_server:add_event(Tag, Event, #{}).

%% @doc Track reports that a user has performed an event
%%
%% Custom data can be attached to the event.
%% @end
-spec track(Key :: binary(), User :: eld_user:user(), Data :: map()) -> ok.
track(Key, User, Data) when is_binary(Key), is_map(Data) ->
    track(Key, User, Data, ?DEFAULT_INSTANCE_NAME).

%% @doc Track reports that a user has performed an event
%%
%% This is useful for specifying a specific client instance.
%% @end
-spec track(Key :: binary(), User :: eld_user:user(), Data :: map(), Tag :: atom()) -> ok.
track(Key, User, Data, Tag) when is_atom(Tag), is_binary(Key), is_map(Data) ->
    Event = eld_event:new_custom(Key, User, Data),
    eld_event_server:add_event(Tag, Event, #{}).
