%%-------------------------------------------------------------------
%% @doc File update server
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_update_file_server).

-behaviour(gen_server).

%% Supervision
-export([start_link/1, init/1]).

%% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-type state() :: #{
    file_paths := [string()],
    file_auto_update := boolean(),
    file_poll_interval := pos_integer(),
    file_allow_duplicate_keys := boolean(),
    storage_tag := atom(),
    file_info := map(),
    timer_ref => timer:tref() | undefined
}.

%%===================================================================
%% Supervision
%%===================================================================

%% @doc Starts the server
%%
%% @end
-spec start_link(Tag :: atom()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Tag) ->
    error_logger:info_msg("Starting file update server for ~p", [Tag]),
    gen_server:start_link(?MODULE, [Tag], []).

-spec init(Args :: term()) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init([Tag]) ->
    FilePaths = ldclient_config:get_value(Tag, file_paths),
    FileAutoUpdate = ldclient_config:get_value(Tag, file_auto_update),
    FilePollInterval = ldclient_config:get_value(Tag, file_poll_interval),
    FileAllowDuplicateKeys = ldclient_config:get_value(Tag, file_allow_duplicate_keys),

    % Need to trap exit so supervisor:terminate_child calls terminate callback
    process_flag(trap_exit, true),
    State = #{
        file_paths => FilePaths,
        file_auto_update => FileAutoUpdate,
        file_poll_interval => FilePollInterval,
        file_allow_duplicate_keys => FileAllowDuplicateKeys,
        storage_tag => Tag,
        file_info => #{},
        timer_ref => undefined
    },
    self() ! {load_files},
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

handle_info({load_files}, #{file_paths := FilePaths, file_auto_update := FileAutoUpdate, file_poll_interval := FilePollInterval, storage_tag := Tag} = State) ->
    {Status, NewState} = load_files(FilePaths, State),
    case Status of
        ok -> true = ldclient_update_processor_state:set_initialized_state(Tag, true),
            start_timer(NewState, FilePollInterval, FileAutoUpdate);
        error -> {noreply, State}
    end;
handle_info({poll}, #{file_paths := FilePaths} = State) ->
    {ok, NewState} = load_files_if_modified(FilePaths, State),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: state()) -> term().
terminate(Reason, #{timer_ref := undefined} = _State) ->
    error_logger:info_msg("Terminating, reason: ~p; Pid none~n", [Reason]),
    ok;
terminate(Reason, #{timer_ref := TimerRef} = _State) ->
    error_logger:info_msg("Terminating, reason: ~p; Pid none~n", [Reason]),
    _ = timer:cancel(TimerRef),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%===================================================================
%% Internal functions
%%===================================================================

-spec start_timer(State :: state(), Interval :: any(), AutoUpdate :: boolean()) -> {noreply, State :: state()}.
start_timer(#{timer_ref := undefined} = State, Interval, true) ->
    {ok, TimerRef} = timer:send_interval(Interval, {poll}),
    NewState = State#{timer_ref => TimerRef},
    {noreply, NewState};
start_timer(State, _Interval, false) ->
    {noreply, State}.

-spec get_fields(Decoded :: map()) -> {Flags :: map(), Segments :: map(), SimpleFlags :: map()}.
get_fields(Decoded) ->
    Flags = maps:get(<<"flags">>, Decoded, #{}),
    Segments = maps:get(<<"segments">>, Decoded, #{}),
    SimpleFlags = maps:get(<<"flagValues">>, Decoded, #{}),
    {Flags, Segments, SimpleFlags}.

-spec parse_from_map(Decoded :: map()) -> {
    ParsedFlags :: map(),
    ParsedSegments :: map(),
    ParsedSimpleFlags :: map()
}.
parse_from_map(Decoded) ->
    {Flags, Segments, SimpleFlags} = get_fields(Decoded),
    ParsedFlags = maps:map(
        fun(_K, V) -> ldclient_flag:new(V) end
        , Flags),
    ParsedSegments = maps:map(
        fun(_K, V) -> ldclient_segment:new(V) end
        , Segments),
    ParsedSimpleFlags = maps:map(
        fun(K, V) -> ldclient_flag:new_simple(K, V) end
        , SimpleFlags),
    {ParsedFlags, ParsedSegments, ParsedSimpleFlags}.

-spec upsert_file_data(Tag :: atom(), ParsedFlags :: map(),
    ParsedSegments :: map()) -> ok.
upsert_file_data(Tag, ParsedFlags, ParsedSegments) ->
    ok = ldclient_storage_map:upsert_clean(Tag, features, ParsedFlags),
    ok = ldclient_storage_map:upsert_clean(Tag, segments, ParsedSegments),
    ok.

-spec upsert_valid_flag_data(Valid :: boolean(), Tag :: atom(), ParsedFlags :: map(),
    ParsedSegments :: map()) -> ok | error.
upsert_valid_flag_data(true, Tag, ParsedFlags, ParsedSegments) ->
    upsert_file_data(Tag, ParsedFlags, ParsedSegments);
upsert_valid_flag_data(false, _Tag, _ParsedFlags, _ParsedSegments) ->
    error.

-spec read_file(FilePath :: string(), Extension :: string()) -> {ok | error, map()}.
read_file(FilePath, ".yaml") ->
    Result = yamerl_constr:file(FilePath, [{detailed_constr, true}]),
    [Head | _] = ldclient_yaml_mapper:to_map_docs(Result, []),
    {ok, Head};
read_file(FilePath, ".json") ->
    {ok, Data} = file:read_file(FilePath),
    {ok, jsx:decode(Data, [return_maps])};
read_file(FilePath, _Extension) ->
    error_logger:warning_msg("File had unrecognized file extension. Valid extensions are .yaml and .json. File: ~p", [FilePath]),
    {error, #{}}.

-spec try_read_file(FilePath :: string(), Extension :: string()) -> {ok | error, map()}.
try_read_file(FilePath, Extension) ->
    try
        read_file(FilePath, Extension)
    catch _:Exception:Stacktrace ->
        error_logger:warning_msg("Problem reading file: ~p Exception: ~p ~p", [FilePath, Exception, Stacktrace]),
        {error, #{}}
    end.

-spec load_file(FilePath :: string(), Extension :: string(), State :: state(), LoadedFlags :: map(), LoadedSegments :: map(), FlagCount :: non_neg_integer()) ->
    {ok | error, NewState :: state(), LoadedFlags :: map(), LoadedSegments :: map(), FlagCount :: non_neg_integer()}.
load_file(FilePath, Extension, #{file_info := CurFileInfo} = State, LoadedFlags, LoadedSegments, FlagCount) ->
    ModifiedTime = filelib:last_modified(FilePath),
    NewFileInfo = maps:merge(CurFileInfo, #{FilePath => ModifiedTime}),
    NewState = State#{file_info := NewFileInfo},
    {Status, Decoded} = try_read_file(FilePath, Extension),
    process_decoded_file(Status, Decoded, NewState, LoadedFlags, LoadedSegments, FlagCount).

-spec process_decoded_file(Status :: ok | error, Decoded :: map(), State :: state(), LoadedFlags :: map(), LoadedSegments :: map(), FlagCount :: non_neg_integer()) ->
    {ok | error, NewState :: state(), LoadedFlags :: map(), LoadedSegments :: map(), FlagCount :: non_neg_integer()}.
process_decoded_file(ok, Decoded, State, LoadedFlags, LoadedSegments, FlagCount) ->
    {ParsedFlags, ParsedSegments, ParsedSimpleFlags} = parse_from_map(Decoded),
    NewFlagCount = FlagCount + length(maps:keys(ParsedFlags)) + length(maps:keys(ParsedSimpleFlags)),
    CombinedFlags = maps:merge(maps:merge(LoadedFlags, ParsedFlags), ParsedSimpleFlags),
    CombinedSegments = maps:merge(LoadedSegments, ParsedSegments),
    {ok, State, CombinedFlags, CombinedSegments, NewFlagCount};
process_decoded_file(error, _Decoded, State, LoadedFlags, LoadedSegments, FlagCount) ->
    {error, State, LoadedFlags, LoadedSegments, FlagCount}.

-spec check_modified(FilesToCheck :: [string()], Modified :: boolean(), State :: state()) ->
    {Modified :: boolean(), State :: state()}.
check_modified([], Modified, State) ->
    {Modified, State};
check_modified([FileToCheck | RestOfFiles], Modified, State) ->
    #{file_info := CurFileInfo} = State,
    ExistingModifiedTime = maps:get(FileToCheck, CurFileInfo, 0),
    ModifiedTime = filelib:last_modified(FileToCheck),
    NewModified = Modified or (ExistingModifiedTime =/= ModifiedTime),
    check_modified(RestOfFiles, NewModified, State).

-spec load_files_if_modified(Files :: [string()], State :: state()) -> {ok | error, State :: state()}.
load_files_if_modified(Files, State) ->
    case check_modified(Files, false, State) of
        {true, UpdatedState} -> load_files(Files, UpdatedState);
        {false, UpdatedState} -> {ok, UpdatedState}
    end.

-spec load_regular_file(FilePath :: string(), IsRegularFile :: boolean(), State :: state(), LoadedFlags :: map(), LoadedSegments :: map(), FlagCount :: non_neg_integer()) ->
    {ok | error, NewState :: state(), LoadedFlags :: map(), LoadedSegments :: map(), FlagCount :: non_neg_integer()}.
load_regular_file(FileToLoad, true, State, LoadedFlags, LoadedSegments, FlagCount) ->
    load_file(FileToLoad, filename:extension(FileToLoad), State, LoadedFlags, LoadedSegments, FlagCount);
load_regular_file(_FileToLoad, false, State, LoadedFlags, LoadedSegments, FlagCount) ->
    {error, State, LoadedFlags, LoadedSegments, FlagCount}.

-spec load_files(Files :: [string()], State :: state()) -> {ok | error, State :: state()}.
load_files(Files, State) ->
    load_files(Files, State, #{}, #{}, 0, ok).

-spec load_files(Files :: [string()], State :: state(), CombinedFlags :: map(), CombinedSegments :: map(), FlagCount :: non_neg_integer(), Status :: ok | error) ->
    {ok | error, State :: state()}.
load_files([FileToLoad | RestOfFiles], State, LoadedFlags, LoadedSegments, FlagCount, ok) ->
    {NewStatus, NewState, CombinedFlags, CombinedSegments, UpdatedCount} =
        load_regular_file(FileToLoad, filelib:is_regular(FileToLoad), State, LoadedFlags, LoadedSegments, FlagCount),
    load_files(RestOfFiles, NewState, CombinedFlags, CombinedSegments, UpdatedCount, NewStatus);
load_files([], #{file_allow_duplicate_keys := AllowDuplicateKeys, storage_tag := Tag} = State, LoadedFlags, LoadedSegments, FlagCount, ok) ->
    Valid = (FlagCount == length(maps:keys(LoadedFlags))) or AllowDuplicateKeys,
    NewStatus = upsert_valid_flag_data(Valid, Tag, LoadedFlags, LoadedSegments),
    {NewStatus, State};
load_files(_Files, State, _LoadedFlags, _LoadedSegments, _FlagCount, error) ->
    %% If there is an error, then do not process any more files.
    {error, State}.
