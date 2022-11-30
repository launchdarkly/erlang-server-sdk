%%-------------------------------------------------------------------
%% @doc Context data type
%% When constructing a context by hand, not using new/1, new/2 or new_from_map,
%% then you must include a kind attribute.
%%
%% The kind attribute is used to differentiate between a map representing
%% a ldclient_user and one representing a ldclient_context.
%% @end
%%-------------------------------------------------------------------

-module(ldclient_context).

%% API
-export([
    new/1,
    new/2,
    set/3,
    set/4,
    get/3,
    get_kinds/1,
    new_multi_from/1,
    set_private_attributes/2,
    set_private_attributes/3,
    new_from_map/1,
    is_valid/1
]).

%% Types

%% Some of the last lines of complex types are formatted unusually. This is to ensure the edoc comments are included.
%% A type where the last line is '}.' will not work, so the last line needs to be collapsed until it includes a field.

-type key() :: binary().
%% Keys may be any non-empty binary() string. `<<>>' and `<<"">>' are not valid.
%% Keys must be binaries, and should match those in LaunchDarkly exactly.
%% No casing conversions will be applied `<<"my_attribute">>' will only match `<<"my_attribute">>', it would not match
%% `<<"myAttribute">>'.

-type attribute_value() :: binary() | integer() | float() | attribute_map() | [attribute_value()].
%% Attribute values should all be data types which are compatible with JSON and nested JSON collections.
%% The leaf nodes of these values are what are ultimately used when evaluating flags which are dependent on attributes.

-type attribute_key() :: binary() | key | kind | anonymous | private_attributes.
%% Attribute keys must be binary() strings. They should not be empty, and they must not be `<<"_meta">>'.
-type attribute_map() :: #{attribute_key() => attribute_value()}.

-type kind_value() :: binary().
%% May only contain ASCII letters, numbers, ., _ or -.
%% `<<"my_kind.0-1">>' would be valid.
%% `<<"my:kind.{0}">>' would not, it contains ':' and '{}' which are not allowed.
%% Kinds should be binaries, and should match the context kinds exactly.
%% No casing conversions will be applied `<<"my_kind">>' will only match `<<"my_kind">>', it would not match
%% `<<"myKind">>'.

-type single_context() :: #{
    key := key(),
    name => binary(),
    kind := kind_value(),
    private_attributes => [binary()],
    anonymous => boolean(),
    attributes => #{attribute_key() => attribute_value()}}.
%% A context which represents a single kind.
%%
%% For a single kind context the 'kind' may not be `<<"multi">>'.
%%
%% ```
%% MyOrgContext = #{
%%     kind => <<"org">>,
%%     key => <<"my-org-key">>,
%%     attributes => #{
%%       <<"someAttribute">> => <<"my-attribute-value">>
%%     }
%% }.
%% '''
%%
%% The above context would be a single kind context representing an organization. It has a key
%% for that organization, and a single attribute 'someAttribute'.

-type context_part() :: #{
    key := key(),
    name := binary(),
    private_attributes => [binary()],
    anonymous => boolean(),
    attributes => #{attribute_key() => attribute_value()}}.
%% The content of a multi context. Should be the same as a {@link single_context/0} aside from missing 'kind'.
%% A multi context is keyed by the 'kind'.

-type multi_context() :: #{
    kind := kind_value(),
    kind_value() := context_part()}.
%% A context which represents multiple kinds. Each kind having its own key and attributes.
%%
%% A multi-context must contain `kind => <<"multi">>' at the root.
%%
%% ```
%% MyMultiContext = #{
%%   %% Multi-contexts must be of kind <<"multi">>.
%%   kind => <<"multi">>,
%%   %% The context is namespaced by its kind. This is an 'org' kind context.
%%   <<"org">> => #{
%%     // Each component context has its own key and attributes.
%%     key => <<"my-org-key">>,
%%     attributes => #{
%%         <<"someAttribute">> => <<"my-attribute-value">>,
%%     }
%%   },
%%   <<"user">> => #{
%%     key => <<"my-user-key">>,
%%     %% Each component context has its own meta attributes. This will only apply the this
%%     %% 'user' context.
%%     private_attributes => [<<"firstName">>]
%%     attributes => #{
%%         <<"firstName">> => <<"Bob">>,
%%         <<"lastName">> => <<"Bobberson">>,
%%     }
%%   }
%% }.
%% '''
%%
%% The above multi-context contains both an 'org' and a 'user'. Each with their own key,
%% attributes, and _meta attributes.

-type context() :: single_context() | multi_context().

-export_type([
    single_context/0,
    multi_context/0,
    context/0,
    attribute_value/0,
    attribute_key/0,
    kind_value/0,
    key/0
]).

-ifdef(TEST).
-compile(export_all).
-endif.

%% @doc Create a new 'user' context with the specified key.
%%
%% @end
-spec new(Key :: binary()) -> single_context().
new(Key) when erlang:is_binary(Key) ->
    #{key => Key, kind => <<"user">>}.

%% @doc Create a new context with the specified key and kind.
%%
%% @end
-spec new(Key :: binary(), Kind :: kind_value()) -> single_context().
new(Key, Kind) when erlang:is_binary(Key) and erlang:is_binary(Kind) ->
    #{key => Key, kind => Kind}.

%% @doc Create a multi context from several multiple single kind contexts.
%%
%% ```
%% MyMultiContext = ldclient_context:new_multi_from([
%%     ldclient_context:new(<<"user-key">>), %% This defaults to a <<"user">> kind.
%%     ldclient_context:new(<<"org-key">>, <<"org">>)]).
%% '''
%%
%% Each of the contexts being combined should have unique keys. If more than one context of the same kind is added,
%% then only a single context of the duplicated type will remain.
%%
%% If `new_from_multi' is called with a list containing a single context, then the single context will be returned.
%% A multi context should contain more than one kind.
%% @end
-spec new_multi_from(Contexts :: [single_context()]) -> multi_context() | single_context().
new_multi_from(Contexts) when length(Contexts) =:= 1 ->
    hd(Contexts);
new_multi_from(Contexts) ->
    new_multi_from(Contexts, #{kind => <<"multi">>}).

%% @doc Create a context from a map.
%%
%% Using this method will help to ensure that all your context keys and values are of supported types. For instance
%% converting all atom() keys into binary() (both for attribute keys and kinds). This can be useful for contexts
%% from a serialized source.
%%
%% If the map contains a 'kind' attribute, then the resulting context will be of that kind.
%% If the map contains a 'kind' attribute, with the value of `<<"multi">>', then a multi context will be created,
%% and each top level field in the map will be a component of that context.
%%
%% If the input map contains invalid data, such as bad kinds, then the context will still be created.
%% If the context contains invalid data, then evaluations will return default values with a reason of
%% 'user_not_specified'.
%%
%% The same key should not be provided in the map as both an atom and a binary. For instance:
%% ```
%% #{key => <<"the-key">>, <<"key">> => <<"the-key">>}
%% '''
%%
%% Create a context without a kind specified:
%% ```
%% ldclient_context:new_from_map(#{
%%     key => <<"my-key">>,
%%     attributes => #{
%%         nested => #{
%%             deeper => #{
%%                 value => <<"my-value">>
%%             }
%%         }
%%     }
%% }).
%% Produces the context
%% #{
%%     key := <<"my-key">>,
%%     kind := <<"user">>,
%%     attributes := #{
%%         <<"nested">> := #{
%%             <<"deeper">> := #{
%%                 <<"value">> := <<"my-value">>
%%             }
%%         }
%%     }
%% }.
%% No kind was included, so it was defaulted to a <<"user">> kind.
%% Notice that all the keys, and nested keys, within attributes have been converted to binaries.
%% '''
%%
%% Creating a context with a specified kind.
%% ```
%%    ldclient_context:new_from_map(#{<<"key">> => <<"my-key">>, <<"kind">> => <<"the-kind">>}).
%% Produces the context
%% {key := <<"my-key">>, kind := <<"the-kind">>}.
%% Notice here how the built-in keys have been corrected to atoms.
%% '''
%%
%% ```
%% ldclient_context:new_from_map(#{
%%     kind => <<"multi">>,
%%     <<"meal">> => #{
%%         key => <<"user-key">>,
%%         <<"name">> => <<"the-name">>, %% Key that will become an atom.
%%         attributes => #{
%%             potato => #{ %% Key that will become a binary.
%%                 <<"bacon">> => true,
%%                 <<"cheese">> => true
%%             }
%%         }
%%     },
%%     <<"location">> => #{
%%         key => <<"location-key">>
%%     }
%% }).
%% Produces the context
%% #{
%%    kind := <<"multi">>,
%%    <<"meal">> := #{
%%        key := <<"user-key">>,
%%        name := <<"the-name">>,
%%        attributes := #{
%%            <<"potato">> := #{
%%                <<"bacon">> := true,
%%                <<"cheese">> := true
%%            }
%%        }
%%    },
%%    <<"location">> := #{
%%        key := <<"location-key">>
%%    }
%% }
%% '''
%% @end
-spec new_from_map(MapContext :: map()) -> Context :: context().
new_from_map(#{kind := <<"multi">>} = MapContext) ->
    Kinds = get_kinds(MapContext), %% Will be original type atom()/binary() at this point.
    lists:foldl(fun(Kind, Acc) ->
                    #{Kind := ContextPart} = MapContext,
                    Acc#{ensure_binary(Kind) => format_context_part(ContextPart)}
                end, #{kind => <<"multi">>, valid => true}, Kinds);
new_from_map(#{kind := _Kind} = MapContext) ->
    format_context_part(MapContext);
%% There isn't a kind, or the kind is not keyed with a kind atom.
new_from_map(MapContext) ->
    Kind = maps:get(kind, MapContext, maps:get(<<"kind">>, MapContext, <<"user">>)),
    new_from_map(MapContext#{kind => ensure_binary(Kind)}).

%% @doc Set an attribute value with the specified key in a single kind context.
%%
%% This method cannot be used to set attributes in nested maps.
%%
%% Any built-in attributes private_attributes, anonymous, key, kind, will be set at the top level of the context.
%% Any attributes that are not built-ins will be set in an 'attributes' map.
%%
%% Attempting to set 'attributes' will result in an attribute named `<<"attributes">>'.
%% @end
-spec set(AttributeKey :: attribute_key(), AttributeValue :: attribute_value(), Context :: single_context()) ->
    single_context().
set(AttributeKey, AttributeValue, Context) ->
    ProcessedKey = get_attribute_key(AttributeKey),
    case is_built_in(ProcessedKey) of
        true -> Context#{get_attribute_key(AttributeKey) => AttributeValue};
        false -> set_in_attributes(ProcessedKey, binary_keys(AttributeValue), Context)
    end.

%% @doc Set an attribute value in the specified context kind with the specified key.
%%
%% If the context is a single kind, then it must be of the kind specified.
%%
%% If it is a multi context, then specified kind must exist in it.
%%
%% This method cannot be used to set attributes in nested maps.
%% @end
-spec set(
    ContextKind :: kind_value(),
    AttributeKey :: attribute_key(),
    AttributeValue :: attribute_value(),
    Context :: context()
) -> multi_context().
set(ContextKind, AttributeKey, AttributeValue, #{kind := <<"multi">>} = Context) ->
    #{ContextKind := SingleContext} = Context,
    ProcessedKey = get_attribute_key(AttributeKey),
    case is_built_in(ProcessedKey) of
        true -> Context#{ContextKind => SingleContext#{ProcessedKey => AttributeValue}};
        false -> Context#{ContextKind => set_in_attributes(ProcessedKey, binary_keys(AttributeValue), SingleContext)}
    end;
set(ContextKind, AttributeKey, AttributeValue, #{kind := Kind} = Context) when ContextKind =:= Kind ->
    set(AttributeKey, AttributeValue, Context).

%% @doc Set private attributes for a single kind context.
%%
%% Designate any number of Context attributes, or properties within them, as private: that is,
%% their values will not be sent to LaunchDarkly.
%%
%% Each parameter can be a simple attribute name, such as "email". Or, if the first character is
%% a slash, the parameter is interpreted as a slash-delimited path to a property within a JSON
%% object, where the first path component is a Context attribute name and each following
%% component is a nested property name: for example, suppose the attribute "address" had the
%% following value
%%
%% ```
%% 	#{<<"street">>: #{<<"line1">>: <<"abc">>, <<"line2">>: <<"def">>}}
%% '''
%%
%% Using ["/address/street/line1"] in this case would cause the "line1" property to be marked as
%% private. This syntax deliberately resembles JSON Pointer, but other JSON Pointer features
%% such as array indexing are not supported for Private.
%%
%% This action only affects analytics events that involve this particular Context. To mark some
%% (or all) Context attributes as private for all users, use the overall configuration for the
%% SDK.
%%
%% The attributes "kind" and "key", and the meta attributes (like private_attribute_names) cannot be made private.
%%
%% In this example, firstName is marked as private, but lastName is not:
%%
%% ```
%% Context = #{
%%     kind => <<"org">>,
%%     key => <<"my-key">>,
%%     private_attributes: [<<"firstName">>],
%%     attributes => #{
%%         <<"firstName">> => <<"Pierre">>,
%%         <<"lastName">> => <<"Menard">>
%%     }
%% }.
%% '''
%%
%% This is a metadata property, rather than an attribute that can be addressed in evaluations:
%% that is, a rule clause that references the attribute name "private_attributes", will not use
%% this value, but instead will use whatever value (if any) you have set for that name with a
%% method such as set/3 or by including it in the attributes map.
%% @end
-spec set_private_attributes(AttributeValues :: [binary()], Context :: single_context()) -> single_context().
set_private_attributes(AttributeValues, Context) ->
    set(private_attributes, AttributeValues, Context).

%% @doc Set private attributes for the specified context kind.
%%
%% Context can either be a single kind context of the specified kind or a multi context containing the kind.
%% @end
-spec set_private_attributes(ContextKind :: kind_value(),
    AttributeValues :: [binary()], Context :: context()) -> context().
set_private_attributes(ContextKind, AttributeValues, Context) ->
    set(ContextKind, private_attributes, AttributeValues, Context).

%% @doc Get an attribute value from the specified context kind by the specified attribute reference
%%
%% If the context is a single kind context, and the ContextKind matches the context's kind, and the context contains
%% the specified attribute, then that value will be provided.
%%
%% If the context is a multi-context, and it contains the specified context kind, and that context kind contains
%% the specified attribute, then that value will be provided.
%%
%% If the attribute value does not exist, then the null atom will be returned.
%% @end
-spec get(
    ContextKind :: kind_value(),
    AttributeReference :: ldclient_attribute_reference:attribute_reference() | binary(),
    Context :: context()
) -> attribute_value() | null.
get(ContextKind, AttributeReference, Context) when erlang:is_binary(AttributeReference) ->
    get(ContextKind, ldclient_attribute_reference:new(AttributeReference), Context);
get(ContextKind, #{valid := true} = AttributeReference, #{kind := <<"multi">>} = Context) ->
    get_from_multi(ContextKind, AttributeReference, Context);
get(ContextKind, #{valid := true} = AttributeReference, #{kind := Kind} = Context) ->
    case ContextKind =:= Kind of
        true -> get_from_common(AttributeReference, Context);
        false -> null
    end;
get(_ContextKind, _AttributeReference, _Context) ->
    %% The attribute reference was not valid.
    null.

%% @doc Get all the kinds in the specified context. Can be a single or multi context.
%%
%% The kind in the context may be an atom or a binary, but this will always return them as binaries for use
%% in comparison against strings from LaunchDarkly.
%% @end
-spec get_kinds(Context :: context()) -> [binary()].
get_kinds(#{kind := <<"multi">>} = Context) ->
    filter_kinds(maps:keys(Context));
get_kinds(#{kind := Kind}) -> [Kind].

%% @doc Verify a context is valid.
%%
%% This will ensure that the context, or contexts of a multi context, have:
%% 1.) Valid keys. Key must exist, must be a binary, and cannot be empty.
%% 2.) Valid kind. Kind must exist, must be a binary, and must be composed of ASCII letters, numbers, as well as
%%     '-', '.', and '_'.
%% 3.) All parts of a multi context meet #1 and #2.
%%
%% Other aspects of the context may be invalid, and evaluation will proceed, but those invalid
%% parts will not impact the evaluation. For example an attribute with an atom() key will not successfully targeted
%% by rules. Some of these issues can be avoided by using the new_from_map function which will convert keys.
%%
%% Evaluations which are done against an invalid context will return default values with a reason
%% of user_not_specified.
%% @end
-spec is_valid(Context :: context()) -> boolean().
is_valid(#{kind := <<"multi">>} = Context) ->
    Kinds = get_kinds(Context), %% Will be original type atom()/binary() at this point.
    lists:foldl(fun(Kind, ValidAcc) ->
                            #{Kind := ContextPart} = Context,
                            ValidAcc and is_valid_kind(Kind) and is_valid_part(ContextPart)
                        end, true, Kinds);
is_valid(#{key := Key, kind := Kind} = _Context) when is_binary(Kind), is_binary(Key) ->
    is_valid_kind(Kind) and is_valid_context_key(Key);
%% Had a kind, but that kind was not binary. So the context is not valid.
is_valid(#{kind := _Kind} = _Context) -> false;
%% No kind, but does have a key and it is binary.
is_valid(#{key := Key} = _Context) when is_binary(Key) -> true;
is_valid(_Context) -> false.

%%===================================================================
%% Internal functions
%%===================================================================

-spec get_from_multi(ContextKind :: binary(),
    AttributeReference :: ldclient_attribute_reference:attribute_reference(),
    Context :: multi_context()) -> Value :: attribute_value().
get_from_multi(ContextKind, AttributeReference, Context) ->
    #{ContextKind := ContextPart} = Context,
    get_from_common(AttributeReference, ContextPart).

-spec get_from_common(AttributeReference :: ldclient_attribute_reference:attribute_reference(),
    Context :: map()) -> Value :: attribute_value().
get_from_common(#{components := [<<"key">>]} = _AttributeReference, Context) ->
    maps:get(key, Context, null);
get_from_common(#{components := [<<"anonymous">>]} = _AttributeReference, Context) ->
    maps:get(anonymous, Context, null);
get_from_common(#{components := Components} = _AttributeReference, #{attributes := Attributes} = _Context) ->
    get_by_components(Components, Attributes);
get_from_common(_AttributeReference, _Context) -> null.

get_by_components(_Components, null = _MapOrItem) ->
    null;
get_by_components([H | T] = _Components, MapOrItem) ->
    get_by_components(T, maps:get(H, MapOrItem, null));
get_by_components([] = _Components, MapOrItem) -> MapOrItem.

-spec get_attribute_key(attribute_key()) -> attribute_key().
get_attribute_key(key) -> key;
get_attribute_key(kind) -> kind;
get_attribute_key(anonymous) -> anonymous;
get_attribute_key(private_attributes) -> private_attributes;
get_attribute_key(name) -> name;
get_attribute_key(Attribute) when is_atom(Attribute) -> atom_to_binary(Attribute, utf8);
get_attribute_key(<<"key">>) -> key;
get_attribute_key(<<"kind">>) -> kind;
get_attribute_key(<<"anonymous">>) -> anonymous;
get_attribute_key(<<"private_attributes">>) -> private_attributes;
get_attribute_key(<<"name">>) -> name;
get_attribute_key(Attribute) when erlang:is_binary(Attribute) -> Attribute.

-spec is_built_in(Key :: binary() | atom()) -> boolean().
is_built_in(key = _Key) -> true;
is_built_in(kind = _Key) -> true;
is_built_in(private_attributes = _Key) -> true;
is_built_in(anonymous = _Key) -> true;
is_built_in(_Key) -> false.


%% Get a list of the kinds in a multi-context. This isn't all the keys in the object, because it excludes the 'kind'
%% key.
-spec filter_kinds(Kinds :: [binary()]) -> [binary()].
filter_kinds(Kinds) -> lists:filter(fun(Key) -> Key =/= kind end, Kinds).

-spec new_multi_from(Contexts :: [single_context()], MultiContext :: multi_context()) -> multi_context().
new_multi_from([#{kind := Kind} = Context | RemainingContexts] = _Contexts, MultiContext) ->
    ContextWithoutKind = maps:remove(kind, Context),
    new_multi_from(RemainingContexts, MultiContext#{Kind => ContextWithoutKind});
new_multi_from([] = _Contexts, MultiContext) -> MultiContext.

-spec ensure_binary(Value :: binary() | atom()) -> binary().
ensure_binary(Value) when is_atom(Value) -> atom_to_binary(Value);
ensure_binary(Value) when is_binary(Value) -> Value.

-spec is_valid_kind(Kind :: binary()) -> boolean().
is_valid_kind(<<"">> = _Kind) -> false;
is_valid_kind(<<"kind">> = _Kind) -> false;
is_valid_kind(Kind) when is_binary(Kind) -> valid_kind_chars(Kind, true);
is_valid_kind(_Kind) -> false.

%% This could be done with a regex instead, but it would be better to compile the regex for performance.
%% Considering this is just data, and not a process, keeping that compiled regex around is more complex.
%% Instead this just does a single comparison for ascii values/ranges.
%%
%% If we did use a regex, then it would look something like below. This regex is a bit verbose because
%% "\w" matches characters like the feminine ordinal indicator, which are not valid for kinds.
%% Sample regex: <<"^[a-zA-Z0-9._-]+$">>.
-spec valid_kind_chars(Chars :: binary(), Valid :: boolean()) -> boolean().
valid_kind_chars(_Chars, false = _Valid) -> false;
valid_kind_chars(<<H, T/binary>> = _Chars, true = _Valid) -> valid_kind_chars(T, valid_kind_char(H));
valid_kind_chars(<<>> = _Chars, Valid) -> Valid.

-spec valid_kind_char(Char :: integer()) -> boolean().
valid_kind_char($-) -> true;
valid_kind_char($.) -> true;
valid_kind_char($_) -> true;
valid_kind_char(H) ->
    ((H >= $0) and (H =< $9)) orelse %% Numbers
    ((H >= $A) and (H =< $Z)) orelse %% Capital letters
    ((H >= $a) and (H =< $z)). %% Lowercase letters

-spec set_in_attributes(Key :: attribute_key(), Value :: attribute_value(), Context :: map()) -> map().
set_in_attributes(Key, Value, Context) ->
    Attributes = maps:get(attributes, Context, #{}),
    Context#{attributes => Attributes#{Key => Value}}.

-spec format_context_part(ContextPart :: map()) -> context_part().
format_context_part(ContextPart) ->
    Attributes = maps:get(attributes, ContextPart, maps:get(<<"attributes">>, ContextPart, #{})),
    NewContext = maps:fold(fun(Key, Value, Acc) ->
        ProcessedKey = get_attribute_key(Key),
        case ProcessedKey of
            <<"attributes">> -> Acc;
            _ -> Acc#{ProcessedKey => Value}
        end
                           end, #{}, ContextPart),
    maps:fold(fun set/3, NewContext, Attributes).

%% Recursively convert keys in maps to binary.
-spec binary_keys(MaybeMap :: term()) -> term().
binary_keys(MaybeMap) when is_map(MaybeMap) ->
    maps:fold(fun(Key, Value, Acc) ->
                  Acc#{ensure_binary(Key) => binary_keys(Value)}
              end, #{}, MaybeMap);
binary_keys(MaybeMap) -> MaybeMap.

-spec is_valid_context_key(Key :: binary()) -> boolean().
is_valid_context_key(<<"">>) -> false;
is_valid_context_key(Key) when is_binary(Key) -> true.

-spec is_valid_part(ContextPart :: context_part()) -> boolean().
is_valid_part(#{key := Key} = _ContextPart) when is_binary(Key) -> true;
is_valid_part(_ContextPart) -> false.
