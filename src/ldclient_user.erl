%%-------------------------------------------------------------------
%% @doc User data type
%%
%% @end
%%-------------------------------------------------------------------

-module(ldclient_user).

%% API
-export([new/1]).
-export([new_from_map/1]).
-export([get/2]).
-export([set/3]).
-export([set_private_attribute_names/2]).
-export([scrub/2]).
-export([event_format/1]).

%% Types
-type user() :: #{
    key := key(),
    ip => binary(),
    country => binary(),
    email => binary(),
    first_name => binary(),
    last_name => binary(),
    avatar => binary(),
    name => binary(),
    anonymous => boolean(),
    custom => custom_attributes(),
    private_attribute_names => private_attribute_names()
}.

-type key() :: binary() | null.
-type attribute() :: binary() | atom().
-type custom_attributes() :: #{binary() := any()}.
-type private_attribute_names() :: [binary()].

-export_type([user/0]).
-export_type([key/0]).
-export_type([attribute/0]).

%%===================================================================
%% API
%%===================================================================

-spec new(key()) -> user().
new(Key) when is_binary(Key) ->
    #{key => Key}.

-spec new_from_map(map()) -> user().
new_from_map(Map) ->
    maps:fold(fun set/3, #{}, Map).

%% @doc Get an attribute value of a user
%%
%% Lookup includes custom attributes. Returns `null' if attribute doesn't exist.
%% @end
-spec get(attribute(), user()) -> term().
get(Attribute, User) ->
    Attr = get_attribute(Attribute),
    get_attribute_value(Attr, User).

%% @doc Set an attribute value for a user
%%
%% Sets given attribute to given value and returns the new user. This function
%% can handle both built-in and custom user attributes.
%% @end
-spec set(attribute(), any(), user()) -> user().
set(Attribute, Value, User) ->
    Attr = get_attribute(Attribute),
    set_attribute_value(Attr, Value, User).

%% @doc Sets a list of private attribute names for a user
%%
%% Any attributes that are on this list will not be sent to and indexed by
%% LaunchDarkly. However, they are still available for flag evaluations
%% performed by the SDK locally. This handles both built-in and custom
%% attributes. The built-in `key' attribute cannot be made private - it will
%% always be sent.
%% @end
-spec set_private_attribute_names([attribute()], user()) -> user().
set_private_attribute_names(null, User) ->
    maps:remove(private_attribute_names, User);
set_private_attribute_names([], User) ->
    maps:remove(private_attribute_names, User);
set_private_attribute_names(AttributeNames, User) when is_list(AttributeNames) ->
    maps:put(private_attribute_names, AttributeNames, User).

%% @doc Scrub private attributes from user
%%
%% Returns the scrubbed user and the list of attributes that were actually
%% scrubbed.
%% @end
-spec scrub(user(), ldclient_config:private_attributes()) -> {user(), private_attribute_names()}.
scrub(User, all) ->
    AllStandardAttributes = [key, ip, country, email, first_name, last_name, avatar, name, anonymous],
    AllCustomAttributes = maps:keys(maps:get(custom, User, #{})),
    AllPrivateAttributes = lists:append(AllStandardAttributes, AllCustomAttributes),
    scrub(User#{private_attribute_names => AllPrivateAttributes});
scrub(User, GlobalPrivateAttributes) when is_list(GlobalPrivateAttributes) ->
    UserPrivateAttributes = maps:get(private_attribute_names, User, []),
    AllPrivateAttributes = lists:append(GlobalPrivateAttributes, UserPrivateAttributes),
    scrub(User#{private_attribute_names => AllPrivateAttributes});
scrub(User, []) ->
    scrub(User).

%% @doc Formats a user for use in an event.
%%
%% Returns the user with first_name and last_name attributes changed to firstName and 
%% lastName so that LaunchDarkly can properly record user data.
%% @end
-spec event_format(user()) -> user().
event_format(User) ->
    UserFirstName = key_replace(first_name, firstName, User),
    key_replace(last_name, lastName, UserFirstName).

%%===================================================================
%% Internal functions
%%===================================================================

-spec scrub(user()) -> {user(), private_attribute_names()}.
scrub(User) ->
    PrivateAttributeNames = maps:get(private_attribute_names, User, []),
    StartingUser = maps:remove(private_attribute_names, User),
    {ScrubbedUser, ScrubbedAttrNames} = scrub_private_attributes(StartingUser, PrivateAttributeNames),
    CustomAttributes = maps:get(custom, ScrubbedUser, #{}),
    {ScrubbedCustomAttributes, ScrubbedCustomAttrNames} = scrub_custom_attributes(CustomAttributes, PrivateAttributeNames),
    FinalUser = set_custom_attributes(ScrubbedUser, ScrubbedCustomAttributes),
    {FinalUser, lists:append(ScrubbedAttrNames, ScrubbedCustomAttrNames)}.

-spec get_attribute_value(Attr :: attribute(), User :: user()) -> any().
get_attribute_value(Attr, User) when is_atom(Attr) ->
    maps:get(Attr, User, null);
get_attribute_value(Attr, #{custom := Custom}) when is_binary(Attr) ->
    maps:get(Attr, Custom, null);
get_attribute_value(Attr, _) when is_binary(Attr) ->
    null.

-spec get_attribute(attribute()) -> attribute().
get_attribute(key) -> key;
get_attribute(ip) -> ip;
get_attribute(country) -> country;
get_attribute(email) -> email;
get_attribute(first_name) -> first_name;
get_attribute(last_name) -> last_name;
get_attribute(avatar) -> avatar;
get_attribute(name) -> name;
get_attribute(anonymous) -> anonymous;
get_attribute(Attribute) when is_atom(Attribute) -> atom_to_binary(Attribute, utf8);
get_attribute(<<"key">>) -> key;
get_attribute(<<"ip">>) -> ip;
get_attribute(<<"country">>) -> country;
get_attribute(<<"email">>) -> email;
get_attribute(<<"firstName">>) -> first_name;
get_attribute(<<"lastName">>) -> last_name;
get_attribute(<<"avatar">>) -> avatar;
get_attribute(<<"name">>) -> name;
get_attribute(<<"anonymous">>) -> anonymous;
get_attribute(Attribute) when is_binary(Attribute) -> Attribute.

-spec set_attribute_value(Attr :: attribute(), Value :: any(), User :: user()) -> user().
set_attribute_value(Attr, Value, User) when is_atom(Attr) ->
    maps:put(Attr, Value, User);
set_attribute_value(Attr, Value, User) when is_binary(Attr) ->
    Custom = maps:get(custom, User, #{}),
    maps:put(custom, Custom#{Attr => Value}, User).

-spec scrub_private_attributes(user(), private_attribute_names()) -> {user(), [attribute()]}.
scrub_private_attributes(User, PrivateAttributeNames) ->
    scrub_private_attributes(User, PrivateAttributeNames, []).

-spec format_attr_for_scrub(Attr :: atom() | binary()) -> binary().
format_attr_for_scrub(first_name) -> <<"firstName">>;
format_attr_for_scrub(last_name) -> <<"lastName">>;
format_attr_for_scrub(Attr) when is_atom(Attr) -> atom_to_binary(Attr, utf8);
format_attr_for_scrub(Attr) -> Attr.

-spec scrub_private_attributes(user(), private_attribute_names(), private_attribute_names()) ->
    {user(), private_attribute_names()}.
scrub_private_attributes(User, [], ScrubbedAttrNames) ->
    {User, ScrubbedAttrNames};
scrub_private_attributes(User, [Attr|Rest], ScrubbedAttrNames) ->
    RealAttr = get_attribute(Attr),
    {NewUser, NewScrubbedAttrNames} = case scrub_private_attribute(RealAttr, User) of
        {ScrubbedUser, true} -> {ScrubbedUser, [format_attr_for_scrub(Attr)|ScrubbedAttrNames]};
        {SameUser, false} -> {SameUser, ScrubbedAttrNames}
    end,
    scrub_private_attributes(NewUser, Rest, NewScrubbedAttrNames).

-spec scrub_private_attribute(attribute(), user()) -> {user(), boolean()}.
scrub_private_attribute(key, User) ->
    % The key attribute is never scrubbed, even if marked private.
    {User, false};
scrub_private_attribute(Attr, User) ->
    {maps:remove(Attr, User), maps:is_key(Attr, User)}.

-spec scrub_custom_attributes(custom_attributes(), private_attribute_names()) ->
    {custom_attributes(), private_attribute_names()}.
scrub_custom_attributes(CustomAttributes, PrivateAttributeNames) ->
    scrub_custom_attributes(CustomAttributes, PrivateAttributeNames, []).

-spec scrub_custom_attributes(custom_attributes(), private_attribute_names(), private_attribute_names()) ->
    {custom_attributes(), private_attribute_names()}.
scrub_custom_attributes(CustomAttributes, _, ScrubbedAttributeNames) when map_size(CustomAttributes) == 0 ->
    {#{}, ScrubbedAttributeNames};
scrub_custom_attributes(CustomAttributes, [], ScrubbedAttributeNames) ->
    {CustomAttributes, ScrubbedAttributeNames};
scrub_custom_attributes(CustomAttributes, [Attr|Rest], ScrubbedAttributeNames) ->
    {NewCustomAttributes, NewScrubbedAttributeNames} = case maps:is_key(Attr, CustomAttributes) of
        true -> {maps:remove(Attr, CustomAttributes), [Attr|ScrubbedAttributeNames]};
        false -> {CustomAttributes, ScrubbedAttributeNames}
    end,
    scrub_custom_attributes(NewCustomAttributes, Rest, NewScrubbedAttributeNames).

-spec set_custom_attributes(user(), custom_attributes()) -> user().
set_custom_attributes(User, CustomAttributes) when map_size(CustomAttributes) == 0 ->
    maps:remove(custom, User);
set_custom_attributes(User, CustomAttributes) ->
    User#{custom => CustomAttributes}.

-spec key_replace(attribute(), attribute(), map()) -> map().
key_replace(Key1, Key2, Map) ->
    case maps:take(Key1, Map) of
        {Value, Map2} ->
            maps:put(Key2, Value, Map2);
        error -> Map
    end.
