%%-------------------------------------------------------------------
%% @doc Testdata update server
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_update_testdata_server).

-behaviour(gen_server).

%% Supervision
-export([start_link/1, init/1]).

%% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-type state() :: #{
    tag => atom(),
    test_data_server => atom()
}.

-ifdef(TEST).
-compile(export_all).
-endif.

%%===================================================================
%% Supervision
%%===================================================================

%% @doc Starts the server
%%
%% @end
-spec start_link(Tag :: atom()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Tag) ->
    error_logger:info_msg("Starting testdata update server for ~p", [Tag]),
    gen_server:start_link(?MODULE, [Tag], []).

-spec init(Args :: term()) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init([Tag]) ->
    % Need to trap exit so supervisor:terminate_child calls terminate callback
    process_flag(trap_exit, true),
    TestDataInstanceTag = ldclient_config:get_value(Tag, testdata_tag),
    TestDataServer = case supervisor:start_child(ldclient_sup, ldclient_testdata:child_spec(TestDataInstanceTag, [])) of
                        {ok, Child} -> Child;
                        {error, { already_started, Child}} -> Child
                     end,
    State = #{
      tag => Tag,
      test_data_server => TestDataServer
    },
    gen_server:call(TestDataServer, {register_instance, Tag}),
    {ok, State}.

%%===================================================================
%% Behavior callbacks
%%===================================================================

-type from() :: {pid(), term()}.
-spec handle_call(Request :: term(), From :: from(), State :: state()) ->
    {reply, Reply :: term(), NewState :: state()} |
    {stop, normal, {error, atom(), term()}, state()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: state()) -> term().
terminate(Reason, #{ tag := Tag, test_data_server := TestDataServer }) ->
    error_logger:info_msg("Terminating, reason: ~p; Pid: ~p ~n", [Reason, self()]),
    gen_server:call(TestDataServer, {unregister_instance, Tag}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
