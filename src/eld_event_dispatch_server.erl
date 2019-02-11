%%-------------------------------------------------------------------
%% @doc Event dispatch server
%%
%% @end
%%-------------------------------------------------------------------

-module(eld_event_dispatch_server).

-behaviour(gen_server).

%% Supervision
-export([start_link/1, init/1]).

%% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% API
-export([send_events/3]).

-type state() :: #{
    sdk_key := string(),
    events_uri := string()
}.

%%===================================================================
%% API
%%===================================================================

%% @doc Start listening to streaming events
%%
%% @end
-spec send_events(Tag :: atom(), Events :: [eld_event:event()], SummaryEvent :: eld_event_server:summary_event()) ->
    ok.
send_events(Tag, Events, SummaryEvent) ->
    ServerName = get_local_reg_name(Tag),
    gen_server:cast(ServerName, {send_events, Events, SummaryEvent}).

%%===================================================================
%% Supervision
%%===================================================================

%% @doc Starts the server
%%
%% @end
-spec start_link(Tag :: atom()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Tag) ->
    ServerName = get_local_reg_name(Tag),
    io:format("Starting events dispatcher with name: ~p~n", [ServerName]),
    gen_server:start_link({local, ServerName}, ?MODULE, [Tag], []).

-spec init(Args :: term()) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init([Tag]) ->
    SdkKey = eld_settings:get_value(Tag, sdk_key),
    EventsUri = eld_settings:get_value(Tag, events_uri),
    State = #{
        sdk_key => SdkKey,
        events_uri => EventsUri
    },
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

handle_cast({send_events, Events, SummaryEvent}, #{sdk_key := SdkKey, events_uri := Uri} = State) ->
    io:format("Sending events: ~p~n", [Events]),
    io:format("Sending summary event: ~p~n", [SummaryEvent]),
    FormattedSummaryEvent = format_summary_event(SummaryEvent),
    FormattedEvents = format_events(Events),
    io:format("Formatted events: ~p~n", [FormattedEvents]),
    io:format("Formatted summary event: ~p~n", [FormattedSummaryEvent]),
    AllEvents = [FormattedSummaryEvent|FormattedEvents],
    io:format("Encoded list of events: ~p~n", [jsx:encode(AllEvents)]),
    Headers = [{"Authorization", SdkKey}],
    {ok, {{_Version, ResponseCode, _ReasonPhrase}, _Headers, _Body}} =
        httpc:request(post, {Uri, Headers, "application/json", jsx:encode(AllEvents)}, [], []),
    io:format("Response code from server: ~p~n", [ResponseCode]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: state()) -> term().
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%===================================================================
%% Internal functions
%%===================================================================

-spec format_events([eld_event:event()]) -> list().
format_events(Events) ->
    lists:foldl(fun format_event/2, [], Events).

-spec format_event(eld_event:event(), list()) -> list().
format_event(
    #{
        type := feature_request,
        timestamp := Timestamp,
        user := User,
        data := #{
            debug := Debug,
            key := Key,
            variation := Variation,
            value := Value,
            default := Default,
            version := Version,
            prereq_of := PrereqOf,
            eval_reason := EvalReason
        }
    },
    Acc
) ->
    Kind = if Debug -> <<"debug">>; true -> <<"feature">> end,
    OutputEvent = #{
        <<"kind">> => Kind,
        <<"creationDate">> => Timestamp,
        <<"key">> => Key,
        <<"variation">> => Variation,
        <<"value">> => Value,
        <<"default">> => Default,
        <<"version">> => Version,
        <<"prereqOf">> => PrereqOf,
        <<"reason">> => format_eval_reason(EvalReason)
    },
    [format_event_set_user(Kind, User, OutputEvent)|Acc].

-spec format_eval_reason(eld_eval:reason()) -> map().
format_eval_reason(target_match) -> #{<<"kind">> => <<"TARGET_MATCH">>};
format_eval_reason({rule_match, _, _}) -> #{<<"kind">> => <<"RULE_MATCH">>};
format_eval_reason({prerequisite_failed, _}) -> #{<<"kind">> => <<"PREREQUISITE_FAILED">>};
format_eval_reason({error, _}) -> #{kind => <<"ERROR">>};
format_eval_reason(fallthrough) -> #{<<"kind">> => <<"FALLTHROUGH">>};
format_eval_reason(off) -> #{<<"kind">> => <<"OFF">>}.

-spec format_event_set_user(binary(), eld_user:user(), map()) -> map().
format_event_set_user(<<"feature">>, #{key := UserKey}, OutputEvent) ->
    OutputEvent#{<<"userKey">> => UserKey};
format_event_set_user(<<"debug">>, User, OutputEvent) ->
    OutputEvent#{<<"user">> => User}.

-spec format_summary_event(eld_event_server:summary_event()) -> map().
format_summary_event(SummaryEvent) when map_size(SummaryEvent) == 0 -> #{};
format_summary_event(#{start_date := StartDate, end_date := EndDate, counters := Counters}) ->
    #{
        <<"kind">> => <<"summary">>,
        <<"startDate">> => StartDate,
        <<"endDate">> => EndDate,
        <<"features">> => format_summary_event_counters(Counters)
    }.

-spec format_summary_event_counters(eld_event_server:counters()) -> map().
format_summary_event_counters(Counters) ->
    maps:fold(fun format_summary_event_counters/3, #{}, Counters).

-spec format_summary_event_counters(eld_event_server:counter_key(), eld_event_server:counter_value(), map()) ->
    map().
format_summary_event_counters(
    #{
        key := FlagKey,
        variation := Variation,
        version := Version
    },
    #{
        count := Count,
        flag_value := FlagValue,
        flag_default := Default
    },
    Acc
) ->
    FlagMap = maps:get(FlagKey, Acc, #{default => Default, counters => []}),
    Counter = #{
        value => FlagValue,
        version => Version,
        count => Count,
        variation => Variation,
        unknown => if Version == 0 -> true; true -> false end
    },
    NewFlagMap = FlagMap#{counters := [Counter|maps:get(counters, FlagMap)]},
    Acc#{FlagKey => NewFlagMap}.

-spec get_local_reg_name(Tag :: atom()) -> atom().
get_local_reg_name(Tag) ->
    list_to_atom("eld_event_dispatch_server_" ++ atom_to_list(Tag)).
