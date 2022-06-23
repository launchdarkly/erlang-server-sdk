%%-------------------------------------------------------------------
%% @doc HTTP utilities
%% @private
%% @end
%%-------------------------------------------------------------------
-module(ldclient_http).

%% API
-export([uri_parse/1]).

-spec port_for_scheme(Port :: undefined | inet:port_number(), Scheme :: atom()) -> inet:port_number().
port_for_scheme(undefined, undefined) ->
    80;
port_for_scheme(undefined, Scheme) ->
    case Scheme of
        https -> 443;
        _ -> 80
    end;
port_for_scheme(Port, _Scheme) ->
    Port.

-type uri_string() :: string() | binary().
-type parse_result() :: {atom(), uri_string(), inet:port_number(), uri_string(), uri_string()}.

-spec uri_parse(URL :: uri_string()) -> {ok, parse_result()}.
uri_parse(URL) ->
    Defaults = #{query => "", path => "", scheme => http, host => "", port => undefined},
    Parsed = uri_string:parse(URL),
    Merged = maps:merge(Defaults, Parsed),
    #{scheme:=Scheme,host:=Host,port:=Port,path:=Path,query:=Query} = Merged,
    SchemeAtom = list_to_atom(Scheme),
    {ok, {SchemeAtom, Host, port_for_scheme(Port, SchemeAtom), Path, Query}}.
