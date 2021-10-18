%%-------------------------------------------------------------------
%% @doc Utility to convert yamerl output into a map.
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_yaml_mapper).

%% API
-export([to_map_docs/2]).

-spec to_map_docs(DocsToMap :: list(), Results :: list()) ->
    list().
to_map_docs([Head | Rest], Results) ->
    Map = to_map(Head),
    to_map_docs(Rest, [Map | Results]);
to_map_docs([], Results) ->
    Results.

%%===================================================================
%% Internal functions
%%===================================================================

to_map({yamerl_doc, Doc}) ->
    to_map(Doc);
to_map({yamerl_seq, yamerl_node_seq, _Tag, _Loc, Seq, _N}) ->
    [to_map(X) || X <- Seq];
to_map({yamerl_map, yamerl_node_map, _Tag, _Loc, MapTuples}) ->
    tuples_to_map(MapTuples, #{});
to_map({yamerl_str, yamerl_node_str, _Tag, _Loc, CharList}) ->
    list_to_binary(CharList);
to_map({yamerl_int, yamerl_node_int, _Tag, _Loc, Number}) ->
    Number;
to_map({yamerl_bool, yamerl_node_bool, _Tag, _Loc, Bool}) ->
    Bool.

tuples_to_map([], Map) ->
    Map;
tuples_to_map([{Key, Val} | Rest], Map) ->
    {yamerl_str, yamerl_node_str, _, _, Name} = Key,
    tuples_to_map(Rest, maps:put(list_to_binary(Name), to_map(Val), Map)).
