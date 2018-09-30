%%%-------------------------------------------------------------------
%%% @doc Rule clause data type
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(eld_clause).

%% API
-export([new/1]).
-export([match_user/2]).
-export([match_user/3]).

-type clause() :: #{
    attribute => binary(),
    op        => operator(),
    values    => [eld_flag:variation_value()],
    negate    => boolean()
}.
%% Describes an individual clause within a targeting rule

-type operator() :: in | ends_with | starts_with | matches | contains
    | less_than | less_than_or_equal | greater_than | greater_than_or_equal
    | before | 'after' | segment_match | semver_equal | semver_less_than
    | semver_greater_than.
%% List of available operators

-export_type([clause/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(map()) -> clause().
new(#{<<"attribute">> := Attribute, <<"negate">> := Negate, <<"op">> := Op, <<"values">> := Values}) ->
    #{attribute => Attribute, negate => Negate, op => parse_operator(Op), values => Values}.

%% @doc Match clauses to user, no segment_match allowed
%%
%% @end
-spec match_user(clause(), eld_user:user()) -> match | no_match.
match_user(Clause, User) ->
    check_clause(Clause, User).

%% @doc Match all clauses to user, includes possible segment_match
%%
%% @end
-spec match_user(clause(), eld_user:user(), atom()) -> match | no_match.
match_user(Clause, User, StorageBackend) ->
    check_clause(Clause, User, StorageBackend).

%%====================================================================
%% Internal functions
%%====================================================================

-spec parse_operator(binary()) -> operator().
parse_operator(<<"in">>) -> in;
parse_operator(<<"endsWith">>) -> ends_with;
parse_operator(<<"startsWith">>) -> starts_with;
parse_operator(<<"matches">>) -> matches;
parse_operator(<<"contains">>) -> contains;
parse_operator(<<"lessThan">>) -> less_than;
parse_operator(<<"lessThanOrEqual">>) -> less_than_or_equal;
parse_operator(<<"greaterThan">>) -> greater_than;
parse_operator(<<"greaterThanOrEqual">>) -> greater_than_or_equal;
parse_operator(<<"before">>) -> before;
parse_operator(<<"after">>) -> 'after';
parse_operator(<<"segmentMatch">>) -> segment_match;
parse_operator(<<"semVerEqual">>) -> semver_equal;
parse_operator(<<"semVerLessThan">>) -> semver_less_than;
parse_operator(<<"semVerGreaterThan">>) -> semver_greater_than.

-spec check_clause(clause(), eld_user:user(), atom()) -> match | no_match.
check_clause(#{op := segment_match, values := SegmentKeys}, User, StorageBackend) ->
    check_segment_keys_match(SegmentKeys, User, StorageBackend);
check_clause(Clause, User, _StorageBackend) ->
    check_clause(Clause, User).

check_clause(_Clause, _User) ->
    % TODO implement
    no_match.

-spec check_segment_keys_match([binary()], eld_user:user(), atom()) -> match | no_match.
check_segment_keys_match([], _User, _StorageBackend) -> no_match;
check_segment_keys_match([SegmentKey|Rest], User, StorageBackend) ->
    Result = check_segment_key_match(SegmentKey, User, StorageBackend),
    check_segment_key_match_result(Result, Rest, User, StorageBackend).

check_segment_key_match_result(match, _Rest, _User, _StorageBackend) -> match;
check_segment_key_match_result(no_match, Rest, User, StorageBackend) ->
    check_segment_keys_match(Rest, User, StorageBackend).

check_segment_key_match(SegmentKey, User, StorageBackend) ->
    Segments = StorageBackend:get(segments, SegmentKey),
    check_segments_match(Segments, User).

check_segments_match([], _User) -> no_match;
check_segments_match([Segment|_], User) ->
    eld_segment:match_user(Segment, User).
