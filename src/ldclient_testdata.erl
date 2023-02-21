%%-------------------------------------------------------------------
%% @doc TestData  server
%%
%% A mechanism for providing dynamically updatable feature flag state
%% in a simplified form to an SDK client in test scenarios.
%%
%% Unlike the file data mechanism, this does not use any external resources.
%% It provides only the data that the application has put into it using
%% the update/2 function.
%%
%% ```
%%    {ok, Flag} = ldclient_testdata:flag("flag-key-1"),
%%    ldclient_testdata:update(ldclient_flagbuilder:variation_for_all(true, Flag)),
%%
%%    Options = #{
%%        datasource => testdata,
%%        send_events => false,
%%        feature_store => ldclient_storage_map
%%    },
%%    ldclient:start_instance(SdkKey, Options),
%%
%%    %% flags can be updated at any time:
%%    {ok, Flag2} = ldclient_testdata:flag("flag-key-2"),
%%    UpdatedFlag2 = ldclient_flagbuilder:fallthrough_variation(false,
%%                   ldclient_flagbuilder:variation_for_context(<<"user">>, <<"some-user-key">>, true, Flag2)),
%% '''
%%
%% The above example uses a simple boolean flag, but more complex configurations
%% are possible using the functions in {@link ldclient_flagbuilder}.
%%
%% {@link ldclient_flagbuilder} supports many of the ways a flag can be configured
%% on the LaunchDarkly dashboard, but does not currently support
%% <ol>
%%   <li>rule operators other than "in" and "not in", or</li>
%%   <li>percentage rollouts.</li>
%% </ol>
%%
%% If the same `ldclient_testdata' instance is used to configure multiple `ldclient_instance' instances, any changes made to the data will propagate to all of the instances
%%
%% @end
%%-------------------------------------------------------------------

-module(ldclient_testdata).

-behaviour(gen_server).

%% Supervision
-export([start_link/1, init/1, child_spec/2]).

%% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Methods
-export([flag/1, flag/2, update/1, update/2]).

-type state() :: #{
        current_builders => #{ binary() => ldclient_flagbuilder:flag_builder() },
        current_flags => #{ binary() => ldclient_flag:flag() },
        instances => [atom()]
}.

-ifdef(TEST).
-compile(export_all).
-endif.

%%===================================================================
%% Helper
%%===================================================================

%% @doc
%% @private
%% @end
-spec get_ref(Tag :: atom() | pid()) -> atom() | pid().
get_ref(Pid) when is_pid(Pid) -> Pid;
get_ref(Tag) when is_atom(Tag) -> list_to_atom("ldclient_testdata_" ++ atom_to_list(Tag)).


%%===================================================================
%% Supervision
%%===================================================================

-spec child_spec(Tag :: atom(), Args :: [term()]) -> supervisor:child_spec().
child_spec(Tag, Args) ->
    #{
       id => Tag,
       start => {?MODULE, start_link, [Tag|Args]}
     }.

%% @doc Starts the server
%% @private
%% @end
-spec start_link(Tag :: atom()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Tag) ->
    gen_server:start_link({local, get_ref(Tag)}, ?MODULE, [], []).

%% @doc
%% @private
%% @end
-spec init(Args :: term()) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init(_) ->
    {ok, initial_state()}.

-spec initial_state() -> state().
initial_state() ->
    #{ current_builders => #{},
       current_flags => #{},
       instances => []
     }.
%%-------------------------------------------------------------------
%% Public Api
%%-------------------------------------------------------------------

%% @doc Creates or copies a {@link ldclient_flagbuilder:flag_builder()}
%% for building a test flag configuration.
%%
%% If this flag key has already been defined in this `ldclient_testdata' instance,
%% then the builder starts with the same configuration that was last provided for this flag.
%%
%% Otherwise, it starts with a new default configuration in which the flag has `true'
%% and `false' variations, is `true' for all contexts when targeting is turned on and
%% `false' otherwise, and currently has targeting turned on.
%%
%% You can change any of those properties, and provide more complex behavior,
%% using the functions in {@link ldclient_flagbuilder}.
%%
%% Once you have set the desired configuration, pass the builder to {@link update/2}.
%%
%% @param FlagKey the flag key
%% @return a flag configuration builder
%% @see update/1
%% @end
-spec flag(FlagKey :: binary() | string()) -> {ok, ldclient_flagbuilder:flag_builder()}.
flag(FlagKey) when is_list(FlagKey) ->
    flag(default, list_to_binary(FlagKey));
flag(FlagKey) ->
    flag(default, FlagKey).

%% @doc Creates or copies a {@link ldclient_flagbuilder:flag_builder()}
%% for building a test flag configuration.
%%
%% If this flag key has already been defined in this `ldclient_testdata' instance,
%% then the builder starts with the same configuration that was last provided for this flag.
%%
%% Otherwise, it starts with a new default configuration in which the flag has `true'
%% and `false' variations, is `true' for all contexts when targeting is turned on and
%% `false' otherwise, and currently has targeting turned on.
%%
%% You can change any of those properties, and provide more complex behavior,
%% using the functions in {@link ldclient_flagbuilder}.
%%
%% Once you have set the desired configuration, pass the builder to {@link update/2}.
%%
%% @param Tag the tag or pid of the `ldclient_testdata' instance
%% @param FlagKey the flag key
%% @return a flag configuration builder
%% @see update/2
%% @end
-spec flag(Tag :: atom() | pid(), FlagKey :: binary()) -> {ok, ldclient_flagbuilder:flag_builder()}.
flag(Tag, FlagKey) ->
   gen_server:call(get_ref(Tag), {flag, FlagKey}).

%% @doc Updates the test data with the specified flag configuration.
%%
%% This has the same effect as if a flag were added or modified on the LaunchDarkly dashboard.
%% It immediately propagates the flag change to any `ldclient_instance(s)' that you have
%% already configured to use this `ldclient_testdata'. If no `ldclient_instance' has
%% been started yet, it simply adds this flag to the test data which will be provided
%% to any `ldclient_instance' that you subsequently configure.
%%
%% @param Flag a flag configuration builder
%% @see flag/1
%% @end
-spec update(Flag :: ldclient_flagbuilder:flag_builder()) -> ok.
update(Flag) ->
    update(default, Flag).

%% @doc Updates the test data with the specified flag configuration.
%%
%% This has the same effect as if a flag were added or modified on the LaunchDarkly dashboard.
%% It immediately propagates the flag change to any `ldclient_instance(s)' that you have
%% already configured to use this `ldclient_testdata'. If no `ldclient_instance' has
%% been started yet, it simply adds this flag to the test data which will be provided
%% to any `ldclient_instance' that you subsequently configure.
%%
%% @param Ref the tag of the `ldclient_testdata' instance
%% @param Flag a flag configuration builder
%% @see flag/2
%% @end
-spec update(Tag :: atom() | pid(), Flag :: ldclient_flagbuilder:flag_builder()) -> ok.
update(Tag, Flag) ->
   gen_server:call(get_ref(Tag), {update, Flag}).

%%===================================================================
%% Behavior callbacks
%%===================================================================

-type from() :: {pid(), term()}.

%% @doc
%% @private
%% @end
-spec handle_call(Request :: term(), From :: from(), State :: state()) ->
    {reply, Reply :: term(), NewState :: state()} |
    {stop, normal, {error, atom(), term()}, state()}.

handle_call({flag, FlagName}, _From, State) ->
    #{ current_builders := CurrentBuilders } = State,
    Flag = case CurrentBuilders of
        #{FlagName := ExistingFlag} -> ExistingFlag;
        _ -> ldclient_flagbuilder:new(FlagName)
    end,
    {reply, {ok, Flag}, State};

handle_call({update, FlagBuilder}, _From, State) ->
    #{ current_builders := CurrentBuilders } = State,
    FlagName = ldclient_flagbuilder:key(FlagBuilder),
    NewBuilders = maps:put(FlagName, FlagBuilder, CurrentBuilders),

    #{ current_flags := CurrentFlags } = State,
    #{ version := OldVersion } = maps:get(FlagName, CurrentFlags, #{ version => 0 }),
    NewFlag = ldclient_flagbuilder:build(FlagBuilder, OldVersion + 1),
    NewFlags = maps:put(FlagName, NewFlag, CurrentFlags),

    NewState = State#{
                 current_flags := NewFlags,
                 current_builders := NewBuilders
               },
    #{ instances := Instances } = State,
    lists:foreach(fun(Tag) ->
                    FeatureStore = ldclient_config:get_value(Tag, feature_store),
                    ok = FeatureStore:upsert(Tag, features, #{ FlagName => NewFlag })
                  end, Instances),

    {reply, ok, NewState};


handle_call({register_instance, Tag}, _From, State = #{instances := Tags, current_flags := Flags}) ->
    NewState = State#{ instances := [Tag|Tags] },
    FeatureStore = ldclient_config:get_value(Tag, feature_store),
    ok = FeatureStore:upsert_clean(Tag, features, Flags),
    ldclient_update_processor_state:set_initialized_state(Tag, true),
    {reply, ok, NewState};

handle_call({unregister_instance, Tag}, _From, State = #{instances := Tags}) ->
    NewInstances = lists:delete(Tag, Tags),
    case NewInstances of
        [] -> %% reset state if this was the last client to deregister
            {reply, ok, initial_state()};
        _ ->
            NewState = State#{ instances := NewInstances},
            {reply, ok, NewState}
    end.

%% @doc
%% @private
%% @end
handle_cast(_Request, State) ->
    {noreply, State}.

%% @doc
%% @private
%% @end
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc
%% @private
%% @end
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: state()) -> term().
terminate(Reason, _State) ->
    error_logger:info_msg("Terminating, reason: ~p; Pid none~n", [Reason]),
    ok.

%% @doc
%% @private
%% @end
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
