%%-------------------------------------------------------------------
%% @doc Rule clause data type
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_clause).

%% API
-export([new/1]).
-export([match_user/2]).
-export([match_user/4]).

%% Types
-type clause() :: #{
    attribute => binary(),
    op        => operator(),
    values    => [ldclient_flag:variation_value()],
    negate    => boolean()
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

%%===================================================================
%% API
%%===================================================================

-spec new(map()) -> clause().
new(RawClauseMap) ->
    ClauseTemplate = #{
        <<"attribute">> => <<>>,
        <<"negate">> => false,
        <<"op">> => <<>>,
        <<"values">> => []
    },
    ClauseMap = maps:merge(ClauseTemplate, RawClauseMap),
    new_from_template(ClauseMap).


%% @doc Match clauses to user, no segmentMatch allowed
%%
%% @end
-spec match_user(clause(), ldclient_user:user()) -> match | no_match.
match_user(Clause, User) ->
    check_clause(Clause, User).

%% @doc Match all clauses to user, includes possible segmentMatch
%%
%% @end
-spec match_user(clause(), ldclient_user:user(), atom(), atom()) -> match | no_match.
match_user(Clause, User, FeatureStore, Tag) ->
    check_clause(Clause, User, FeatureStore, Tag).

%%===================================================================
%% Internal functions
%%===================================================================

-spec new_from_template(map()) -> clause().
new_from_template(#{<<"attribute">> := Attribute, <<"negate">> := Negate, <<"op">> := Op, <<"values">> := Values}) ->
    #{attribute => Attribute, negate => Negate, op => parse_operator(Op), values => Values}.

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

-spec check_clause(clause(), ldclient_user:user(), atom(), atom()) -> match | no_match.
check_clause(#{op := segmentMatch, values := SegmentKeys} = Clause, User, FeatureStore, Tag) ->
    maybe_negate_match(Clause, check_segment_keys_match(SegmentKeys, User, FeatureStore, Tag));
check_clause(#{op := none}, _User, _FeatureStore, _Tag) -> no_match;
check_clause(Clause, User, _FeatureStore, _Tag) ->
    check_clause(Clause, User).

check_clause(#{attribute := Attribute} = Clause, User) ->
    UserValue = ldclient_user:get(Attribute, User),
    check_user_value_null(UserValue, Clause).

check_user_value_null(null, _Clause) -> no_match;
check_user_value_null(UserValue, Clause) ->
    maybe_negate_match(Clause, check_attribute(UserValue, Clause)).

check_attribute([] = UserValues, _Clause) when is_list(UserValues) -> no_match;
check_attribute([UserValue|Rest] = UserValues, Clause) when is_list(UserValues) ->
    Result = check_attribute(UserValue, Clause),
    check_attribute_result(Result, Rest, Clause);
check_attribute(UserValue, #{values := ClauseValues, op := Operator}) ->
    check_attribute_against_clause_values(UserValue, Operator, ClauseValues).

check_attribute_against_clause_values(_UserValue, _Operator, []) -> no_match;
check_attribute_against_clause_values(UserValue, Operator, [ClauseValue|Rest]) ->
    Result = check_attribute_against_clause_value(UserValue, Operator, ClauseValue),
    check_attribute_against_clause_value_result(Result, UserValue, Operator, Rest).

check_attribute_against_clause_value_result(true, _UserValue, _Operator, _Rest) -> match;
check_attribute_against_clause_value_result(false, UserValue, Operator, Rest) ->
    check_attribute_against_clause_values(UserValue, Operator, Rest).

check_attribute_against_clause_value(null, _Operator, _ClauseValue) -> false;
check_attribute_against_clause_value(_UserValue, _Operator, null) -> false;
check_attribute_against_clause_value(Value, in, Value) -> true;
check_attribute_against_clause_value(_UserValue, in, _ClauseValue) -> false;
check_attribute_against_clause_value(UserValue, endsWith, ClauseValue)
    when is_binary(UserValue), is_binary(ClauseValue) ->
    binary:longest_common_suffix([UserValue, ClauseValue]) == byte_size(ClauseValue);
check_attribute_against_clause_value(UserValue, startsWith, ClauseValue)
    when is_binary(UserValue), is_binary(ClauseValue) ->
    binary:longest_common_prefix([UserValue, ClauseValue]) == byte_size(ClauseValue);
check_attribute_against_clause_value(UserValue, matches, ClauseValue)
    when is_binary(UserValue), is_binary(ClauseValue) ->
    re:run(UserValue, ClauseValue) =/= nomatch;
check_attribute_against_clause_value(UserValue, contains, ClauseValue)
    when is_binary(UserValue), is_binary(ClauseValue) ->
    binary:match(UserValue, ClauseValue) =/= nomatch;
check_attribute_against_clause_value(UserValue, lessThan, ClauseValue)
    when is_number(UserValue), is_number(ClauseValue) ->
    UserValue < ClauseValue;
check_attribute_against_clause_value(UserValue, lessThanOrEqual, ClauseValue)
    when is_number(UserValue), is_number(ClauseValue) ->
    UserValue =< ClauseValue;
check_attribute_against_clause_value(UserValue, greaterThan, ClauseValue)
    when is_number(UserValue), is_number(ClauseValue) ->
    UserValue > ClauseValue;
check_attribute_against_clause_value(UserValue, greaterThanOrEqual, ClauseValue)
    when is_number(UserValue), is_number(ClauseValue) ->
    UserValue >= ClauseValue;
check_attribute_against_clause_value(UserValue, before, ClauseValue)
    when
    is_binary(UserValue) =/= true, is_integer(UserValue) =/= true;
    is_binary(ClauseValue) =/= true, is_integer(ClauseValue) =/= true ->
    % One of the values is neither binary nor integer
    false;
check_attribute_against_clause_value(UserValue, before, ClauseValue) ->
    try
        UserDate = parse_date_to_int(UserValue),
        ClauseDate = parse_date_to_int(ClauseValue),
        UserDate < ClauseDate
    catch _:_ ->
        false
    end;
check_attribute_against_clause_value(UserValue, 'after', ClauseValue)
    when
    is_binary(UserValue) =/= true, is_integer(UserValue) =/= true;
    is_binary(ClauseValue) =/= true, is_integer(ClauseValue) =/= true ->
    % One of the values is neither binary nor integer
    false;
check_attribute_against_clause_value(UserValue, 'after', ClauseValue) ->
    try
        UserDate = parse_date_to_int(UserValue),
        ClauseDate = parse_date_to_int(ClauseValue),
        UserDate > ClauseDate
    catch _:_ ->
        false
    end;
check_attribute_against_clause_value(UserValue, semVerEqual, ClauseValue)
    when is_binary(UserValue), is_binary(ClauseValue) ->
    check_semver_equal(parse_semver(UserValue), parse_semver(ClauseValue));
check_attribute_against_clause_value(UserValue, semVerLessThan, ClauseValue)
    when is_binary(UserValue), is_binary(ClauseValue) ->
    check_semver_less_than(parse_semver(UserValue), parse_semver(ClauseValue));
check_attribute_against_clause_value(UserValue, semVerGreaterThan, ClauseValue)
    when is_binary(UserValue), is_binary(ClauseValue) ->
    check_semver_greater_than(parse_semver(UserValue), parse_semver(ClauseValue));
check_attribute_against_clause_value(_UserValue, _Operator, _ClauseValue) -> false.

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
check_semver_equal(UserSemVer, ClauseSemVer) ->
    case verl:compare(UserSemVer, ClauseSemVer) of
        eq -> true;
        _ -> false
    end.

-spec check_semver_less_than(binary(), binary()) -> boolean().
check_semver_less_than(UserSemVer, ClauseSemVer) ->
    case verl:compare(UserSemVer, ClauseSemVer) of
        lt -> true;
        _ -> false
    end.

-spec check_semver_greater_than(binary(), binary()) -> boolean().
check_semver_greater_than(UserSemVer, ClauseSemVer) ->
    case verl:compare(UserSemVer, ClauseSemVer) of
        gt -> true;
        _ -> false
    end.

check_attribute_result(match, _Rest, _Clause) -> match;
check_attribute_result(no_match, Rest, Clause) ->
    check_attribute(Rest, Clause).

-spec check_segment_keys_match([binary()], ldclient_user:user(), atom(), atom()) -> match | no_match.
check_segment_keys_match([], _User, _FeatureStore, _Tag) -> no_match;
check_segment_keys_match([SegmentKey|Rest], User, FeatureStore, Tag) ->
    Result = check_segment_key_match(SegmentKey, User, FeatureStore, Tag),
    check_segment_key_match_result(Result, Rest, User, FeatureStore, Tag).

check_segment_key_match_result(match, _Rest, _User, _FeatureStore, _Tag) -> match;
check_segment_key_match_result(no_match, Rest, User, FeatureStore, Tag) ->
    check_segment_keys_match(Rest, User, FeatureStore, Tag).

check_segment_key_match(SegmentKey, User, FeatureStore, Tag) ->
    Segments = FeatureStore:get(Tag, segments, SegmentKey),
    check_segments_match(Segments, User).

check_segments_match([], _User) -> no_match;
check_segments_match([{_SegmentKey, Segment}|_], User) ->
    ldclient_segment:match_user(Segment, User).

-spec maybe_negate_match(clause(), match | no_match) -> match | no_match.
maybe_negate_match(#{negate := false}, Match) -> Match;
maybe_negate_match(#{negate := true}, match) -> no_match;
maybe_negate_match(#{negate := true}, no_match) -> match.
