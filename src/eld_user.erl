%%-------------------------------------------------------------------
%% @doc User data type
%%
%% @end
%%-------------------------------------------------------------------

-module(eld_user).

%% API
-export([new/1]).
-export([get/2]).
-export([scrub/1]).

%% Types
-type user() :: #{
    key := key(),
    secondary => binary(),
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

-type key() :: binary().
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

%% @doc Get an attribute value of a user
%%
%% Lookup includes custom attributes. Returns `null' if attribute doesn't exist.
%% @end
-spec get(attribute(), user()) -> term().
get(Attribute, User) ->
    Attr = get_attribute(Attribute),
    get_attribute_value(Attr, User).

%% @doc Scrub private attributes from user
%%
%% @end
-spec scrub(user()) -> user().
scrub(User) ->
    PrivateAttributeNames = maps:get(private_attribute_names, User, []),
    StartingUser = maps:remove(private_attribute_names, User),
    ScrubbedUser = scrub_private_attributes(StartingUser, PrivateAttributeNames),
    CustomAttributes = maps:get(custom, ScrubbedUser, #{}),
    ScrubbedCustomAttributes = scrub_custom_attributes(CustomAttributes, PrivateAttributeNames),
    maybe_set_custom_attributes(ScrubbedUser, ScrubbedCustomAttributes).

%%===================================================================
%% Internal functions
%%===================================================================

-spec get_attribute_value(Attr :: attribute(), User :: user()) -> any().
get_attribute_value(Attr, User) when is_atom(Attr) ->
    maps:get(Attr, User, null);
get_attribute_value(Attr, #{custom := Custom}) when is_binary(Attr) ->
    maps:get(Attr, Custom, null);
get_attribute_value(Attr, _) when is_binary(Attr) ->
    null.

-spec get_attribute(attribute()) -> attribute().
get_attribute(<<"key">>) -> key;
get_attribute(<<"secondary">>) -> secondary;
get_attribute(<<"ip">>) -> ip;
get_attribute(<<"country">>) -> country;
get_attribute(<<"email">>) -> email;
get_attribute(<<"first_name">>) -> first_name;
get_attribute(<<"last_name">>) -> last_name;
get_attribute(<<"avatar">>) -> avatar;
get_attribute(<<"name">>) -> name;
get_attribute(Attribute) when is_binary(Attribute) -> Attribute;
get_attribute(Attribute) when is_atom(Attribute) -> Attribute.

-spec scrub_private_attributes(user(), private_attribute_names()) -> user().
scrub_private_attributes(User, []) ->
    User;
scrub_private_attributes(User, [Attr|Rest]) ->
    RealAttr = get_attribute(Attr),
    scrub_private_attributes(maps:remove(RealAttr, User), Rest).

-spec scrub_custom_attributes(custom_attributes(), private_attribute_names()) -> custom_attributes().
scrub_custom_attributes(CustomAttributes, _) when map_size(CustomAttributes) == 0 ->
    #{};
scrub_custom_attributes(CustomAttributes, []) ->
    CustomAttributes;
scrub_custom_attributes(CustomAttributes, [Attr|Rest]) ->
    scrub_private_attributes(maps:remove(Attr, CustomAttributes), Rest).

-spec maybe_set_custom_attributes(user(), custom_attributes()) -> user().
maybe_set_custom_attributes(User, CustomAttributes) when map_size(CustomAttributes) == 0 ->
    User;
maybe_set_custom_attributes(User, CustomAttributes) ->
    User#{custom => CustomAttributes}.
