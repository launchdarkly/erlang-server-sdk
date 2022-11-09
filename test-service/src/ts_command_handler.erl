%-------------------------------------------------------------------
%% @doc `ts_command_handler' module
%%
%% Handle commands which are directed to client instances.
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ts_command_handler).

-export([handle_command/2]).

-spec handle_command(Tag :: atom(), Command :: ts_command_params:command_params()) -> error | map().
handle_command(Tag, #{command := evaluate, evaluate := Evaluate} = _Command) ->
    handle_evaluate_command(Tag, Evaluate);
handle_command(Tag, #{command := flush_events} = _Command) ->
    handle_flush_events_command(Tag);
handle_command(Tag, #{command := identify_event, identify_event := IdentifyEvent} = _Command) ->
    handle_identify_event_command(Tag, IdentifyEvent);
handle_command(Tag, #{command := custom_event, custom_event := CustomEvent} = _Command) ->
    handle_custom_event_command(Tag, CustomEvent);
handle_command(Tag, #{command := evaluate_all, evaluate_all := EvaluateAll} = _Command) ->
    handle_evaluate_all_command(Tag, EvaluateAll);
handle_command(_Tag, _Command) ->
    error.

-spec handle_evaluate_command(Tag :: atom(),
    Evaluate :: ts_command_params:evaluate_flag_params()) -> ts_command_params:evaluate_flag_response().
handle_evaluate_command(Tag, #{
    flag_key := FlagKey,
    user := User,
    value_type := _ValueType,
    default_value := DefaultValue,
    detail := true} = _Evaluate) ->
    ts_command_params:format_evaluate_flag_response(
        ldclient:variation_detail(FlagKey, User, DefaultValue, Tag));
handle_evaluate_command(Tag, #{
    flag_key := FlagKey,
    user := User,
    value_type := _ValueType,
    default_value := DefaultValue} = _Evaluate) ->
    ts_command_params:format_evaluate_flag_response(
        ldclient:variation(FlagKey, User, DefaultValue, Tag)).

-spec handle_evaluate_all_command(Tag :: atom(),
    EvaluateAll :: ts_command_params:evaluate_all_flags_params())
        -> ts_command_params:evaluate_all_flags_response().
handle_evaluate_all_command(Tag, #{user := User} = EvaluateAll) ->
    #{state => ldclient:all_flags_state(User, EvaluateAll, Tag)}.

-spec handle_flush_events_command(Tag :: atom()) -> ok.
handle_flush_events_command(Tag) ->
    ldclient_event_server:flush(Tag).

-spec handle_identify_event_command(Tag :: atom(),
    IdentifyEvent :: ts_command_params:identify_event_params()) -> ok.
handle_identify_event_command(Tag, #{user := User} = _IdentifyEvent) ->
    ldclient:identify(User,Tag).

-spec handle_custom_event_command(Tag :: atom(),
    CustomEvent :: ts_command_params:custom_event_params()) -> ok.
handle_custom_event_command(Tag,
    #{event_key := EventKey, omit_null_data := false, user:= User,
        data := Data, metric_value := MetricValue} = _CustomEvent) ->
    ldclient:track_metric(EventKey, User, Data, MetricValue, Tag);
handle_custom_event_command(Tag,
    #{event_key := EventKey, omit_null_data := false, user:= User,  data := Data} = _CustomEvent) ->
    ldclient:track(EventKey, User, Data, Tag);
handle_custom_event_command(Tag,
    #{event_key := EventKey, omit_null_data := true, user:= User,  data := null, metric_value := MetricValue} = _CustomEvent) ->
    ldclient:track_metric(EventKey, User, null, MetricValue, Tag);
handle_custom_event_command(Tag,
    #{event_key := EventKey, omit_null_data := true, user:= User,  data := null} = _CustomEvent) ->
    ldclient:track(EventKey, User, null, Tag);
handle_custom_event_command(_Tag, _CustomEvent) ->
    % This combination doesn't seem valid, but this can be extended if it is.
    % No tests trigger this condition currently.
    ok.
