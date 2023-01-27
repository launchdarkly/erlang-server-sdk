%%-------------------------------------------------------------------
%% @doc Methods for dealing with http headers.
%% @private
%% @end
%%-------------------------------------------------------------------
-module(ldclient_headers).

%% API
-export([get_default_headers/2]).

-ifdef(TEST).
-compile(export_all).
-endif.

%% @doc Get the default headers for SDK requests.
%%
%% The headers can be accessed as a map of binary values, or as a list of pairs containing string() values.
%% This option is offered because different client libraries used have different header input formats.
%% ```
%% %% binary_map
%% #{<<"some-header">> => <<"some-value">>}
%% %% string_pairs
%% [{"some-header", "some-value"}]
%% '''
%% @end
-spec get_default_headers(Tag :: atom(), Format :: binary_map | string_pairs) -> map() | [{string(), string()}].
get_default_headers(Tag, _Format = binary_map) ->
    get_default_headers(Tag);
get_default_headers(Tag, _Format = string_pairs) ->
    %% Translate the headers from a map into a list of {binary(), binary()}.
    PairList = maps:to_list(get_default_headers(Tag)),
    %% Translate the {binary(), binary()} list into a {string(), string()} list.
    lists:map(fun({Key, Value}) -> {binary:bin_to_list(Key), binary:bin_to_list(Value)} end, PairList).

%%===================================================================
%% Internal functions
%%===================================================================

%% @doc Get the default headers, as a map, for the specified client Tag.
%%
%% @end
get_default_headers(Tag) ->
    with_tags(Tag, #{
        <<"authorization">> => list_to_binary(ldclient_config:get_value(Tag, sdk_key)),
        <<"user-agent">> => list_to_binary(ldclient_config:get_user_agent())
    }).

%% @doc Append the tags header to the given map.
%%
%% LaunchDarkly supports adding `tag' headers to requests. These are those tags.
%% Terminology wise this overlaps with the `Tag' used to identify client instances.
%% @end
-spec with_tags(Tag :: atom(), InMap :: map()) -> OutMap :: map().
with_tags(Tag, InMap) ->
    Tags = get_tags(Tag),
    case Tags of
        [] -> InMap;
        _ -> InMap#{
            <<"x-launchdarkly-tags">> => combine_tags(Tags)
        }
    end.

%% @doc Get the tags as a  list of binary pairs.
%%
%% @end
-spec get_tags(Tag :: atom()) -> [{binary(), [binary()]}].
get_tags(Tag) ->
    %% This is where additional tags should be added.
    AppInfo = ldclient_config:get_value(Tag, application),
    sort_tags(add_version_tag(AppInfo, add_id_tag(AppInfo, []))).

%% @doc Combine all the tags into the format for application tags.
%%
%% Formatted tags are of the format `tag/value' a tag can have multiple values
%% in that case it will be represented by space delimited pairs `tagA/valueA tagA/ValueB`.
%% @end
-spec combine_tags(Tags :: [{binary(), [binary()]}]) -> binary().
combine_tags(Tags) -> combine_tags(Tags, <<>>).

-spec combine_tags(Tags :: [{binary(), [binary()]}], AccIn :: binary()) -> binary().
combine_tags([], AccIn) -> AccIn;
combine_tags([Tag|Remainder] = _Tags, <<>>) ->
    combine_tags(Remainder, combine_tag(Tag));
combine_tags([Tag|Remainder] = _Tags, AccIn) ->
    join(<<" ">>, [AccIn, combine_tags(Remainder, combine_tag(Tag))]).

-spec combine_tag(Tag :: {binary(), [binary()]}) -> binary().
combine_tag({TagKey, TagValues}) ->
    join(<<" ">>, lists:map(fun(TagValue) -> <<TagKey/binary,$/,TagValue/binary>> end, TagValues)).

%% @doc Join a list of binary() into a single binary() with a separator.
%%
%% @end
-spec join(Separator :: binary(), List :: [binary()]) -> binary().
join(_Separator, []) ->
    <<>>;
join(Separator, [H|T]) ->
    lists:foldl(fun (Value, Acc) -> <<Acc/binary, Separator/binary, Value/binary>> end, H, T).

add_id_tag(#{id := Id} = _Info, InList) -> [{<<"application-id">>, [Id]}| InList];
add_id_tag(_Info, InList) -> InList.

add_version_tag(#{version := Version} = _Info, InList) -> [{<<"application-version">>, [Version]}| InList];
add_version_tag(_Info, InList) -> InList.

%% @doc Tags and values for tags should both be sorted.
%%
%% ```
%% [
%%     {<<"b">>, [<<"2">>, <<"1">>, <<"3">>]},
%%     {<<"c">>, [<<"5">>, <<"4">>, <<"1">>]},
%%     {<<"a">>, [<<"7">>, <<"9">>, <<"3">>]}
%% ]
%% %% Would sort to:
%% [
%%     {<<"a">>, [<<"3">>, <<"7">>, <<"9">>]},
%%     {<<"b">>, [<<"1">>, <<"2">>, <<"3">>]},
%%     {<<"c">>, [<<"1">>, <<"4">>, <<"5">>]}
%% ]
%% @end
-spec sort_tags(Tags :: [{binary(), [binary()]}]) -> SortedTags :: [{binary(), [binary()]}].
sort_tags(Tags) ->
    %% Sort the values of each tag.
    lists:map(fun({Key, Value}) -> {Key, sort_tag_values(Value)} end,
        %% Sort the tags by key.
        lists:sort(fun({KeyA, _ValueA} = _A, {KeyB, _ValueB} = _B) -> KeyA < KeyB end, Tags)).

-spec sort_tag_values(TagValue :: [binary()]) -> [binary()].
sort_tag_values(TagValue) -> lists:sort(TagValue).
