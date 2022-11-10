%%-------------------------------------------------------------------
%% @doc Attribute Reference
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_attribute_reference).

%% API
-export([
    new/1,
    new_from_legacy/1
]).

%% Opaque type, consumers are expected to interact with the type using the methods in this module.
-type attribute_reference() :: #{
    valid := boolean(),
    components => [binary()],
    %% Binary representation suitable for redaction list.
    binary => binary()
}.

-export_type([attribute_reference/0]).

%%===================================================================
%% Internal functions
%%===================================================================

-type unescape_state() :: #{
    valid := boolean(),
    acc := binary(),
    in_escape := boolean()
}.

%% Expose non-exported methods for tests.
-ifdef(TEST).
-compile(export_all).
-endif.

%% @doc Create a new attribute reference from an attribute reference string.
%% If you want to create a reference from a legacy attribute name, then use new_from_legacy.
%% @end
-spec new(BinaryReference :: binary()) -> attribute_reference().
new(<<"">> = _BinaryReference) ->
    #{valid => false};
new(<<"/">> = _BinaryReference) ->
    #{valid => false};
new(<<"/", _T/binary>> = BinaryReference) ->
    Components = get_components(BinaryReference),
    #{valid => valid_components(Components), binary => BinaryReference, components => Components};
%% Binary did not start with "/", so it is a literal.
new(BinaryReference) when is_binary(BinaryReference) ->
    #{valid => true, components => [BinaryReference], binary => BinaryReference};
%% Was not a binary value, so it is invalid.
new(_) ->
    #{valid => false}.

%% @doc Create a new attribute reference from a legacy attribute name.
%%
%% @end
-spec new_from_legacy(LegacyLiteral :: binary()) -> attribute_reference().
%% An empty string is an invalid literal.
new_from_legacy(<<"">> = _LegacyLiteral) -> #{valid => false};
%% If a legacy literal starts with /, then it will need escaped for the binary reference.
new_from_legacy(<<"/", _T/binary>> = LegacyLiteral) -> new(escape_legacy(LegacyLiteral));
new_from_legacy(LegacyLiteral) -> new(LegacyLiteral).

%%===================================================================
%% Internal functions
%%===================================================================

-spec unescape(Component :: binary()) -> UnescapedComponent :: binary() | error.
unescape(Component) ->
    unescape(Component, #{acc => <<"">>, valid => true, in_escape => false}).

-spec unescape(Component :: binary(), State :: unescape_state()) -> UnescapedComponent :: binary() | error.
unescape(<<"~">>, _State) ->
    error;
unescape(<<"~", T/binary>>, State) ->
    unescape(T, State#{ in_escape => true });
unescape(<<"0", T/binary>>, #{in_escape := true, acc := Acc} = State) ->
    unescape(T, State#{ acc => <<Acc/binary, "~">>, in_escape => false});
unescape(<<"1", T/binary>>, #{in_escape := true, acc := Acc} = State) ->
    unescape(T, State#{ acc => <<Acc/binary, "/">>, in_escape => false});
unescape(<<_H, _T/binary>>, #{in_escape := true} = _State) ->
    %% Was in an escape sequence and the next character was not 0 or 1.
    error;
unescape(<<H, T/binary>>, #{acc := Acc} = State) ->
    unescape(T, State#{acc => <<Acc/binary, H>>});
unescape(<<>>, #{acc := Acc } = _State) -> Acc.

-spec get_components(BinaryReference :: binary()) -> Components :: [binary() | error].
get_components(BinaryReference) ->
    %% Skip the first item in the split because of the leading /.
    lists:map(fun unescape/1, tl(binary:split(BinaryReference, <<"/">>, [global]))).

-spec valid_components([error | binary()]) -> boolean().
valid_components([error|_T]) -> false;
%% Empty components indicate multiple or trailing /.
%% /potato/ or /potato//potato
valid_components([<<>>|_T]) -> false;
valid_components([_H|T]) -> valid_components(T);
valid_components([]) -> true.

-spec escape_legacy(Component :: binary()) -> EscapedComponent :: binary().
escape_legacy(Component) -> escape_legacy(Component, <<"/">>).

-spec escape_legacy(Component :: binary(), Acc :: binary()) -> Escaped :: binary().
escape_legacy(<<"~", T/binary>> = _Component, Acc) -> escape_legacy(T, <<Acc/binary, "~0">>);
escape_legacy(<<"/", T/binary>> = _Component, Acc) -> escape_legacy(T, <<Acc/binary, "~1">>);
escape_legacy(<<H, T/binary>> = _Component, Acc) -> escape_legacy(T, <<Acc/binary, H>>);
escape_legacy(<<>> = _Component, Acc) -> Acc.
