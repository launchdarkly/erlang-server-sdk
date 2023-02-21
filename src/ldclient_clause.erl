%%-------------------------------------------------------------------
%% @doc Rule clause data type
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_clause).

%% API
-export([new/1]).
-export([match_context/4]).
-export([match_context/5]).

%% Types
-type clause() :: #{
    attribute => ldclient_attribute_reference:attribute_reference(),
    op        => operator(),
    values    => [ldclient_flag:variation_value()],
    negate    => boolean(),
    context_kind => binary()
}.
%% Describes an individual clause within a targeting rule

-type operator() :: in | endsWith | startsWith | matches | contains
    | lessThan | lessThanOrEqual | greaterThan | greaterThanOrEqual
    | before | 'after' | segmentMatch | semVerEqual | semVerLessThan
    | semVerGreaterThan | none.
%% List of available operators

-export_type([clause/0]).

-ifdef(TEST).
-compile(export_all).
-endif.

-define(CLAUSE_TEMPLATE, #{
    <<"attribute">> => <<>>,
    <<"negate">> => false,
    <<"op">> => <<>>,
    <<"values">> => [],
    <<"contextKind">> => <<"user">>
}).

%%===================================================================
%% API
%%===================================================================

-spec new(map()) -> clause().
new(RawClauseMap) ->
    ContextKind = maps:get(<<"contextKind">>, RawClauseMap, undefined),
    ClauseMap = maps:merge(?CLAUSE_TEMPLATE, RawClauseMap),
    %% Clauses with no context kind are legacy.
    new_from_template(ClauseMap, ContextKind == undefined).

%% @doc Match all clauses to the context.
%%
%% @end
-spec match_context(
    Clause :: clause(),
    Context :: ldclient_context:context(),
    FeatureStore :: atom(),
    Tag :: atom()
) -> match | no_match | malformed_flag.
match_context(Clause, Context, FeatureStore, Tag) ->
    check_clause(Clause, Context, FeatureStore, Tag, []).

%% @doc Match all clauses to the context.
%%
%% Allows providing of already visited segments to allow detection of cyclic dependencies.
%% @end
-spec match_context(
    Clause :: clause(),
    Context :: ldclient_context:context(),
    FeatureStore :: atom(),
    Tag :: atom(),
    VisitedSegments :: [binary()]
) -> match | no_match | malformed_flag.
match_context(Clause, Context, FeatureStore, Tag, VisitedSegments) ->
    check_clause(Clause, Context, FeatureStore, Tag, VisitedSegments).

%%===================================================================
%% Internal functions
%%===================================================================

-spec new_from_template(map(), IsLegacy :: boolean()) -> clause().
new_from_template(#{
    <<"attribute">> := Attribute,
    <<"negate">> := Negate,
    <<"op">> := Op,
    <<"values">> := Values,
    <<"contextKind">> := ContextKind
}, IsLegacy) ->
    #{
        attribute => parse_attribute(Attribute, IsLegacy),
        negate => Negate,
        op => parse_operator(Op),
        values => Values,
        context_kind => ContextKind
    }.

-spec parse_attribute(Attribute :: binary(), Legacy :: boolean()) -> ldclient_attribute_reference:attribute_reference().
parse_attribute(Attribute, true) -> ldclient_attribute_reference:new_from_legacy(Attribute);
parse_attribute(Attribute, false) -> ldclient_attribute_reference:new(Attribute).

-spec parse_operator(binary()) -> operator().
parse_operator(<<"in">>) -> in;
parse_operator(<<"endsWith">>) -> endsWith;
parse_operator(<<"startsWith">>) -> startsWith;
parse_operator(<<"matches">>) -> matches;
parse_operator(<<"contains">>) -> contains;
parse_operator(<<"lessThan">>) -> lessThan;
parse_operator(<<"lessThanOrEqual">>) -> lessThanOrEqual;
parse_operator(<<"greaterThan">>) -> greaterThan;
parse_operator(<<"greaterThanOrEqual">>) -> greaterThanOrEqual;
parse_operator(<<"before">>) -> before;
parse_operator(<<"after">>) -> 'after';
parse_operator(<<"segmentMatch">>) -> segmentMatch;
parse_operator(<<"semVerEqual">>) -> semVerEqual;
parse_operator(<<"semVerLessThan">>) -> semVerLessThan;
parse_operator(<<"semVerGreaterThan">>) -> semVerGreaterThan;
parse_operator(_) -> none.

-spec check_clause(Clause :: clause(),
    Context :: ldclient_context:context(),
    FeatureStore :: atom(),
    Tag :: atom(),
    VisitedSegments :: [binary()]
) -> match | no_match | malformed_flag.
check_clause(#{op := segmentMatch, values := SegmentKeys} = Clause, Context, FeatureStore, Tag, VisitedSegments) ->
    maybe_negate_match(Clause, check_segment_keys_match(SegmentKeys, Context, FeatureStore, Tag, VisitedSegments));
check_clause(#{op := none}, _Context, _FeatureStore, _Tag, _VisitedSegments) -> no_match;
check_clause(Clause, Context, _FeatureStore, _Tag, _VisitedSegments) ->
    check_clause(Clause, Context).

-spec check_clause(
    Clause :: clause(),
    Context :: ldclient_context:context()
) ->  match | no_match | malformed_flag.
check_clause(#{attribute := #{valid := false}, context_kind := _ContextKind} = _Clause, _Context) ->
    %% The attribute reference is not valid. The flag is malformed.
    malformed_flag;
check_clause(#{attribute := #{components := [<<"kind">>]}, context_kind := _ContextKind} = Clause, Context) ->
    %% If the reference is to the kind of the context, then we handle it special and apply the checks
    %% to the kinds of the context (versus the kind attribute specifically).
    check_context_value_null(ldclient_context:get_kinds(Context), Clause);
check_clause(#{attribute := Attribute, context_kind := ContextKind} = Clause, Context) ->
    ContextValue = ldclient_context:get(ContextKind, Attribute, Context),
    check_context_value_null(ContextValue, Clause).

check_context_value_null(null, _Clause) -> no_match;
check_context_value_null(ContextValue, Clause) ->
    maybe_negate_match(Clause, check_attribute(ContextValue, Clause)).

check_attribute([] = ContextValues, _Clause) when is_list(ContextValues) -> no_match;
check_attribute([ContextValue|Rest] = ContextValues, Clause) when is_list(ContextValues) ->
    Result = check_attribute(ContextValue, Clause),
    check_attribute_result(Result, Rest, Clause);
check_attribute(ContextValue, #{values := ClauseValues, op := Operator}) ->
    check_attribute_against_clause_values(ContextValue, Operator, ClauseValues).

check_attribute_against_clause_values(_ContextValue, _Operator, []) -> no_match;
check_attribute_against_clause_values(ContextValue, Operator, [ClauseValue|Rest]) ->
    Result = check_attribute_against_clause_value(ContextValue, Operator, ClauseValue),
    check_attribute_against_clause_value_result(Result, ContextValue, Operator, Rest).

check_attribute_against_clause_value_result(true, _ContextValue, _Operator, _Rest) -> match;
check_attribute_against_clause_value_result(false, ContextValue, Operator, Rest) ->
    check_attribute_against_clause_values(ContextValue, Operator, Rest).

check_attribute_against_clause_value(null, _Operator, _ClauseValue) -> false;
check_attribute_against_clause_value(_ContextValue, _Operator, null) -> false;
check_attribute_against_clause_value(Value, in, Value) -> true;
check_attribute_against_clause_value(_ContextValue, in, _ClauseValue) -> false;
check_attribute_against_clause_value(ContextValue, endsWith, ClauseValue)
    when is_binary(ContextValue), is_binary(ClauseValue) ->
    binary:longest_common_suffix([ContextValue, ClauseValue]) == byte_size(ClauseValue);
check_attribute_against_clause_value(ContextValue, startsWith, ClauseValue)
    when is_binary(ContextValue), is_binary(ClauseValue) ->
    binary:longest_common_prefix([ContextValue, ClauseValue]) == byte_size(ClauseValue);
check_attribute_against_clause_value(ContextValue, matches, ClauseValue)
    when is_binary(ContextValue), is_binary(ClauseValue) ->
    try
        re:run(ContextValue, ClauseValue) =/= nomatch
    catch _:_ ->
        %% If the regex was not valid, then it isn't a match.
        false
    end;
check_attribute_against_clause_value(ContextValue, contains, ClauseValue)
    when is_binary(ContextValue), is_binary(ClauseValue) ->
    binary:match(ContextValue, ClauseValue) =/= nomatch;
check_attribute_against_clause_value(ContextValue, lessThan, ClauseValue)
    when is_number(ContextValue), is_number(ClauseValue) ->
    ContextValue < ClauseValue;
check_attribute_against_clause_value(ContextValue, lessThanOrEqual, ClauseValue)
    when is_number(ContextValue), is_number(ClauseValue) ->
    ContextValue =< ClauseValue;
check_attribute_against_clause_value(ContextValue, greaterThan, ClauseValue)
    when is_number(ContextValue), is_number(ClauseValue) ->
    ContextValue > ClauseValue;
check_attribute_against_clause_value(ContextValue, greaterThanOrEqual, ClauseValue)
    when is_number(ContextValue), is_number(ClauseValue) ->
    ContextValue >= ClauseValue;
check_attribute_against_clause_value(ContextValue, before, ClauseValue)
    when
    is_binary(ContextValue) =/= true, is_integer(ContextValue) =/= true;
    is_binary(ClauseValue) =/= true, is_integer(ClauseValue) =/= true ->
    % One of the values is neither binary nor integer
    false;
check_attribute_against_clause_value(ContextValue, before, ClauseValue) ->
    try
        ContextDate = parse_date_to_int(ContextValue),
        ClauseDate = parse_date_to_int(ClauseValue),
        ContextDate < ClauseDate
    catch _:_ ->
        false
    end;
check_attribute_against_clause_value(ContextValue, 'after', ClauseValue)
    when
    is_binary(ContextValue) =/= true, is_integer(ContextValue) =/= true;
    is_binary(ClauseValue) =/= true, is_integer(ClauseValue) =/= true ->
    % One of the values is neither binary nor integer
    false;
check_attribute_against_clause_value(ContextValue, 'after', ClauseValue) ->
    try
        ContextDate = parse_date_to_int(ContextValue),
        ClauseDate = parse_date_to_int(ClauseValue),
        ContextDate > ClauseDate
    catch _:_ ->
        false
    end;
check_attribute_against_clause_value(ContextValue, semVerEqual, ClauseValue)
    when is_binary(ContextValue), is_binary(ClauseValue) ->
    check_semver_equal(parse_semver(ContextValue), parse_semver(ClauseValue));
check_attribute_against_clause_value(ContextValue, semVerLessThan, ClauseValue)
    when is_binary(ContextValue), is_binary(ClauseValue) ->
    check_semver_less_than(parse_semver(ContextValue), parse_semver(ClauseValue));
check_attribute_against_clause_value(ContextValue, semVerGreaterThan, ClauseValue)
    when is_binary(ContextValue), is_binary(ClauseValue) ->
    check_semver_greater_than(parse_semver(ContextValue), parse_semver(ClauseValue));
check_attribute_against_clause_value(_ContextValue, _Operator, _ClauseValue) -> false.

-spec parse_date_to_int(binary()|integer()) -> integer().
parse_date_to_int(Value) when is_binary(Value) ->
    calendar:rfc3339_to_system_time(binary_to_list(Value), [{unit, nanosecond}]);
parse_date_to_int(Value) when is_integer(Value) ->
    % Convert milliseconds to nanoseconds
    Value * 1000000.

-spec parse_semver(binary()) -> binary().
parse_semver(S) ->
    case re:run(S, <<"^\\d+(\\.\\d+)?(\\.\\d)?">>, [{capture, all, binary}]) of
        {match, [M0|_] = Matches} when length(Matches) =:= 1 ->
            Rest = binary:part(S, byte_size(M0), byte_size(S)-byte_size(M0)),
            <<M0/binary, $., $0, $., $0, Rest/binary>>;
        {match, [M0|_] = Matches} when length(Matches) =:= 2 ->
            Rest = binary:part(S, byte_size(M0), byte_size(S)-byte_size(M0)),
            <<M0/binary, $., $0, Rest/binary>>;
        {match, _Matches} -> S;
        nomatch -> S
    end.

-spec check_semver_equal(binary(), binary()) -> boolean().
check_semver_equal(ContextSemVer, ClauseSemVer) ->
    case verl:compare(ContextSemVer, ClauseSemVer) of
        eq -> true;
        _ -> false
    end.

-spec check_semver_less_than(binary(), binary()) -> boolean().
check_semver_less_than(ContextSemVer, ClauseSemVer) ->
    case verl:compare(ContextSemVer, ClauseSemVer) of
        lt -> true;
        _ -> false
    end.

-spec check_semver_greater_than(binary(), binary()) -> boolean().
check_semver_greater_than(ContextSemVer, ClauseSemVer) ->
    case verl:compare(ContextSemVer, ClauseSemVer) of
        gt -> true;
        _ -> false
    end.

check_attribute_result(match, _Rest, _Clause) -> match;
check_attribute_result(no_match, Rest, Clause) ->
    check_attribute(Rest, Clause).

-spec check_segment_keys_match(
    SegmentKeys :: [binary()],
    Context :: ldclient_context:context(),
    FeatureStore :: atom(),
    Tag :: atom(),
    VisitedSegments :: [binary()]
) -> match | no_match.
check_segment_keys_match([], _Context, _FeatureStore, _Tag, _VisitedSegments) -> no_match;
check_segment_keys_match([SegmentKey|Rest], Context, FeatureStore, Tag, VisitedSegments) ->
    case lists:member(SegmentKey, VisitedSegments) of
        %% Detected a segment cycle, so the flag is malformed.
        true -> malformed_flag;
        %% No cycle, evaluate as normal.
        false ->
            Result = check_segment_key_match(SegmentKey, Context, FeatureStore, Tag, [SegmentKey | VisitedSegments]),
            check_segment_key_match_result(Result, Rest, Context, FeatureStore, Tag, [SegmentKey | VisitedSegments])
    end.

-spec check_segment_key_match_result(Result :: match | no_match | malformed_flag,
    SegmentKeys :: [binary()],
    Context :: ldclient_context:context(),
    FeatureStore :: atom(),
    Tag :: atom(),
    VisitedSegments :: [binary()]
) -> match | no_match | malformed_flag.
check_segment_key_match_result(malformed_flag, _Rest, _Context, _FeatureStore, _Tag, _VisitedSegments) -> malformed_flag;
check_segment_key_match_result(match, _Rest, _Context, _FeatureStore, _Tag, _VisitedSegments) -> match;
check_segment_key_match_result(no_match, Rest, Context, FeatureStore, Tag, VisitedSegments) ->
    check_segment_keys_match(Rest, Context, FeatureStore, Tag, VisitedSegments).

check_segment_key_match(SegmentKey, Context, FeatureStore, Tag, VisitedSegments) ->
    Segments = get_segment(Tag, FeatureStore, SegmentKey),
    check_segments_match(Segments, Context, FeatureStore, Tag, VisitedSegments).

check_segments_match([], _Context, _FeatureStore, _Tag, _VisitedSegments) -> no_match;
check_segments_match([{_SegmentKey, Segment}|_], Context, FeatureStore, Tag, VisitedSegments) ->
    ldclient_segment:match_context(Segment, Context, FeatureStore, Tag, VisitedSegments).

-spec maybe_negate_match(clause(), match | no_match) -> match | no_match.
maybe_negate_match(#{negate := false}, Match) -> Match;
maybe_negate_match(#{negate := true}, match) -> no_match;
maybe_negate_match(#{negate := true}, no_match) -> match.

-spec is_not_deleted(Item :: map()) -> boolean().
is_not_deleted(#{deleted := true}) -> false;
is_not_deleted(_) -> true.

%% @doc Get the segment for the specified segment key. If there is no segment matching the SegmentKey, then return
%% an empty list.
%%
%% @end
-spec get_segment(Tag :: atom(), FeatureStore :: atom(), SegmentKey :: binary()) ->
    [{SegmentKey :: binary(), SegmentValue :: ldclient_segment:segment()}].
get_segment(Tag, FeatureStore, SegmentKey) ->
    [Segment || Segment = {_, SegmentValue} <- FeatureStore:get(Tag, segments, SegmentKey), is_not_deleted(SegmentValue)].
