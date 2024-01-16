%%%-------------------------------------------------------------------
%%% @doc Filter private attributes and format for events.
%%% @private
%%% @end
%%%-------------------------------------------------------------------
-module(ldclient_context_filter).


%% API
-export([
    format_context_for_event/2,
    format_context_for_event_with_anonyous_redaction/2
]).

-ifdef(TEST).
-compile(export_all).
-endif.

%% @doc Format a context for events.
%%
%% Should produce an event schema v4 formatted context.
%%
%% The private attributes specified in the context, and those included in the private attributes parameter, will be
%% removed from the context. Additionally they will be added to the redacted attributes list in _meta.
%% @end
-spec format_context_for_event(PrivateAttributes :: [ldclient_attribute_reference:attribute_reference()] | all,
    Context :: ldclient_context:context()
    ) -> Formatted :: map().
format_context_for_event(PrivateAttributes, Context) ->
    internal_format_context_for_event(PrivateAttributes, Context, false).

%% @doc Format a context for events.
%%
%% Should produce an event schema v4 formatted context.
%%
%% The private attributes specified in the context, and those included in the private attributes parameter, will be
%% removed from the context. Additionally they will be added to the redacted attributes list in _meta.
%%
%% If a provided context is anonymous, all attributes will be redacted except for key, kind, and anonymous.
%% @end
-spec format_context_for_event_with_anonyous_redaction(PrivateAttributes :: [ldclient_attribute_reference:attribute_reference()] | all,
    Context :: ldclient_context:context()
    ) -> Formatted :: map().
format_context_for_event_with_anonyous_redaction(PrivateAttributes, Context) ->
    internal_format_context_for_event(PrivateAttributes, Context, true).

-spec internal_format_context_for_event(PrivateAttributes :: [ldclient_attribute_reference:attribute_reference()] | all,
    Context :: ldclient_context:context(),
    RedactAnonymous :: boolean()
    ) -> Formatted :: map().
internal_format_context_for_event(PrivateAttributes, #{kind := <<"multi">>} = Context, RedactAnonymous) ->
    maps:fold(fun(Key, Value, Acc) ->
                Acc#{ensure_binary(Key) => format_context_part(PrivateAttributes, Key, Value, RedactAnonymous)}
              end, #{}, Context);
internal_format_context_for_event(_PrivateAttributes, #{anonymous := true} = Context, true) ->
    internal_format_context_for_event(all, Context, false);
internal_format_context_for_event(PrivateAttributes, Context, _RedactAnonymous) ->
    Attributes = maps:get(attributes, Context, null),
    ContextPrivateAttributes = lists:map(fun(Value) ->
                                               ldclient_attribute_reference:new(Value)
                                           end, maps:get(private_attributes, Context, [])),
    ContextWithoutAttributes = maps:remove(attributes, Context),
    ContextWithoutPrivateAttributes = maps:remove(private_attributes, ContextWithoutAttributes),
    case PrivateAttributes of
        all -> clone_with_redaction(all,
            merge_attributes(ContextWithoutPrivateAttributes, Attributes));
        _ -> clone_with_redaction(PrivateAttributes ++ ContextPrivateAttributes,
            merge_attributes(ContextWithoutPrivateAttributes, Attributes))
    end.

%%===================================================================
%% Internal functions
%%===================================================================

-spec merge_attributes(Context :: map(), Attributes :: map() | null) -> map().
merge_attributes(Context, null = _Attributes) ->
    Context;
merge_attributes(Context, Attributes) ->
    maps:merge(Attributes, Context).

-spec clone_with_redaction(
    PrivateAttributes :: [ldclient_attribute_reference:attribute_reference()] | all,
    Context :: map()
) -> map().
clone_with_redaction(all, Context) ->
    #{redacted := RedactedAttributes, item := RedactedContext} = clone_with_redaction(all, Context, []),
    ContextMaybeWithMeta = case RedactedAttributes of
                               [] -> RedactedContext;
                               _ -> RedactedContext#{
                                   <<"_meta">> => #{
                                       <<"redactedAttributes">> => [Binary || #{binary := Binary} <- RedactedAttributes]
                                   }
                               }
                           end,
    ContextMaybeWithMeta;
clone_with_redaction(PrivateAttributes, Context) ->
    AttributeReferences =
    [Reference || #{valid := Valid, components := Components} = Reference <- PrivateAttributes, Valid and can_redact(Components)],
    #{redacted := RedactedAttributes, item := RedactedContext} = clone_with_redaction(AttributeReferences, Context, []),
    ContextMaybeWithMeta = case RedactedAttributes of
                               [] -> RedactedContext;
                               _ -> RedactedContext#{
                                   <<"_meta">> => #{
                                       <<"redactedAttributes">> => [Binary || #{binary := Binary} <- RedactedAttributes]
                                   }
                               }
                           end,
    ContextMaybeWithMeta.

-spec clone_with_redaction(
    PrivateAttributeComponents :: [[binary()]] | all,
    MapOrItem :: term(), ParentComponents :: [binary()]
) -> map().
%% When redacting `all' we never recurse, so we don't need parent components.
clone_with_redaction(all, MapOrItem, _ParentComponents) when is_map(MapOrItem) ->
    maps:fold(fun(Key, Value, #{redacted := Redacted, item := Item} = _Acc) ->
        BinaryKey = ensure_binary(Key),
        case can_redact([BinaryKey]) of
            true ->
                #{redacted => Redacted ++ [#{binary => BinaryKey}], item => Item};
            false ->
                %% No need to recurse, because you always remove the top level when all attribute are private.
                #{redacted => Redacted, item => Item#{BinaryKey => Value}}
        end
              end, #{redacted => [],item => #{}}, MapOrItem);
clone_with_redaction(PrivateAttributeComponents, MapOrItem, ParentComponents) when is_map(MapOrItem) ->
    maps:fold(fun(Key, Value, #{redacted := Redacted, item := Item} = _Acc) ->
        Components = lists:append(ParentComponents, [ensure_binary(Key)]),
        case lists:search(fun(AttributeReference) ->
                       #{components := ReferenceComponents} = AttributeReference,
                        ReferenceComponents == Components
                       end, PrivateAttributeComponents) of
            {value, MatchedReference} ->
                #{redacted => Redacted ++ [MatchedReference], item => Item};
            false ->
                #{redacted := RedactedKeys, item := RedactedValue} = clone_with_redaction(PrivateAttributeComponents, Value, Components),
                #{redacted => Redacted ++ RedactedKeys, item => Item#{ensure_binary(Key) => RedactedValue}}
        end
    end, #{redacted => [],item => #{}}, MapOrItem);
clone_with_redaction(_PrivateAttributes, MapOrItem, _ParentComponents) -> #{redacted => [], item => MapOrItem}.

-spec can_redact(Components :: [binary()]) -> boolean().
can_redact([<<"key">>]) -> false;
can_redact([<<"kind">>]) -> false;
can_redact([<<"anonymous">>]) -> false;
can_redact(_Components) -> true.

-spec format_context_part(PrivateAttributes :: [ldclient_attribute_reference:attribute_reference()],
    Key :: string() | atom(),
    Value :: map() | binary(),
    RedactAnonymous :: boolean()
) -> map() | binary().
format_context_part(_PrivateAttributes, kind, Value, _RedactAnonymous) ->  Value;
format_context_part(PrivateAttributes, _Key, Value, RedactAnonymous) ->
    internal_format_context_for_event(PrivateAttributes, Value, RedactAnonymous).

-spec ensure_binary(Value :: binary() | atom()) -> binary().
ensure_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
ensure_binary(Value) when is_binary(Value) -> Value.
