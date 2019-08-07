-module(eld_user_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    new_from_key/1,
    new_from_map_key_only/1,
    new_from_map_custom_only/1,
    new_from_map_full/1,
    get/1,
    get_custom/1,
    set/1,
    set_binary/1,
    set_custom/1,
    set_custom_atom/1,
    set_private_attribute_names/1,
    set_private_attribute_names_empty/1,
    set_private_attribute_names_reset/1,
    scrub/1,
    scrub_empty/1,
    scrub_null/1,
    scrub_key/1,
    scrub_all_attributes_private/1,
    scrub_global_private_attributes/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        new_from_key,
        new_from_map_key_only,
        new_from_map_custom_only,
        new_from_map_full,
        get,
        get_custom,
        set,
        set_binary,
        set_custom,
        set_custom_atom,
        set_private_attribute_names,
        set_private_attribute_names_empty,
        set_private_attribute_names_reset,
        scrub,
        scrub_empty,
        scrub_null,
        scrub_key,
        scrub_all_attributes_private,
        scrub_global_private_attributes
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%%====================================================================
%% Helpers
%%====================================================================

%%====================================================================
%% Tests
%%====================================================================

new_from_key(_) ->
    Key = <<"123">>,
    User = eld_user:new(Key),
    User = #{key => Key}.

new_from_map_key_only(_) ->
    Key = <<"123">>,
    Map = #{<<"key">> => Key},
    User = eld_user:new_from_map(Map),
    User = #{key => Key}.

new_from_map_custom_only(_) ->
    CustomAttr1 = <<"custom-attr-name1">>,
    CustomAttr2 = custom_attr_name2,
    CustomAttr2Expected = atom_to_binary(CustomAttr2, utf8),
    CustomVal1 = <<"custom-val1">>,
    CustomVal2 = 12345,
    Map = #{CustomAttr1 => CustomVal1, CustomAttr2 => CustomVal2},
    User = eld_user:new_from_map(Map),
    User = #{custom => #{CustomAttr1 => CustomVal1, CustomAttr2Expected => CustomVal2}}.

new_from_map_full(_) ->
    Key = 12345,
    Secondary = "abc",
    Ip = "1.2.3.4",
    Country = <<"some-country">>,
    Email = "foo@bar.com",
    FirstName = "a",
    LastName = "z",
    Avatar = "ratavA",
    Name = "foobar",
    Anonymous = false,
    CustomKey1 = <<"custom-key1">>,
    CustomKey2 = <<"custom-key2">>,
    CustomValue1 = <<"custom-foo">>,
    CustomValue2 = <<"custom-bar">>,
    Map = #{
        <<"key">> => Key,
        secondary => Secondary,
        ip => Ip,
        country => Country,
        email => Email,
        first_name => FirstName,
        last_name => LastName,
        avatar => Avatar,
        name => Name,
        anonymous => Anonymous,
        CustomKey1 => CustomValue1,
        CustomKey2 => CustomValue2
    },
    User = eld_user:new_from_map(Map),
    UserExpected = #{
        key => Key,
        secondary => Secondary,
        ip => Ip,
        country => Country,
        email => Email,
        first_name => FirstName,
        last_name => LastName,
        avatar => Avatar,
        name => Name,
        anonymous => Anonymous,
        custom => #{
            CustomKey1 => CustomValue1,
            CustomKey2 => CustomValue2
        }
    },
    io:format("Actual user: ~p~n", [User]),
    io:format("Expected user: ~p~n", [UserExpected]),
    UserExpected = User.

get(_) ->
    Key = <<"123">>,
    User = eld_user:new(Key),
    Key = eld_user:get(key, User),
    Key = eld_user:get(<<"key">>, User).

get_custom(_) ->
    CustomAttrAtom = 'my-custom-attr',
    CustomAttrBin = atom_to_binary(CustomAttrAtom, utf8),
    CustomVal = 12345,
    User = #{custom => #{CustomAttrBin => CustomVal}},
    CustomVal = eld_user:get(CustomAttrBin, User),
    CustomVal = eld_user:get(CustomAttrAtom, User).

set(_) ->
    Key = <<"12345">>,
    KeyNew = 789,
    User = eld_user:set(key, KeyNew, eld_user:new(Key)),
    User = #{key => KeyNew}.

set_binary(_) ->
    Key = <<"12345">>,
    KeyNew = 789,
    User = eld_user:set(<<"key">>, KeyNew, eld_user:new(Key)),
    User = #{key => KeyNew}.

set_custom(_) ->
    Key = <<"12345">>,
    CustomAttr = <<"my-custom-attr">>,
    CustomVal = 345,
    User = eld_user:set(CustomAttr, CustomVal, eld_user:new(Key)),
    User = #{key => Key, custom => #{CustomAttr => CustomVal}}.

set_custom_atom(_) ->
    Key = <<"12345">>,
    CustomAttr = 'my-custom-attr',
    CustomAttrExpected = atom_to_binary(CustomAttr, utf8),
    CustomVal = 345,
    User = eld_user:set(CustomAttr, CustomVal, eld_user:new(Key)),
    User = #{key => Key, custom => #{CustomAttrExpected => CustomVal}}.

set_private_attribute_names(_) ->
    Key = <<"123">>,
    PrivateAttributeNames = ["name", "country"],
    User = eld_user:new(Key),
    UserActual = eld_user:set_private_attribute_names(PrivateAttributeNames, User),
    UserExpected = #{key => Key, private_attribute_names => PrivateAttributeNames},
    UserExpected = UserActual.

set_private_attribute_names_empty(_) ->
    Key = <<"123">>,
    User = eld_user:new(Key),
    UserActual = eld_user:set_private_attribute_names([], User),
    User = UserActual.

set_private_attribute_names_reset(_) ->
    Key = <<"123">>,
    PrivateAttributeNames = ["name", "country"],
    User = eld_user:new(Key),
    UserWithPrivateAttributes = eld_user:set_private_attribute_names(PrivateAttributeNames, User),
    UserActual = eld_user:set_private_attribute_names([], UserWithPrivateAttributes),
    User = UserActual.

scrub(_) ->
    Key = 123,
    Name = "foo",
    Country = "bar",
    Custom1Name = <<"custom-attr-1">>,
    Custom1Value = 345,
    Custom2Name = <<"custom-attr-2">>,
    Custom2Value = 789,
    Custom3Name = <<"custom-attr-3">>,
    User = eld_user:new_from_map(#{
        key => Key,
        name => Name,
        country => Country,
        Custom1Name => Custom1Value,
        Custom2Name => Custom2Value
    }),
    PrivateAttributeNames = [<<"country">>, Custom1Name, Custom3Name],
    UserWithPrivateAttrs = eld_user:set_private_attribute_names(PrivateAttributeNames, User),
    {UserActual, ScrubbedActual} = eld_user:scrub(UserWithPrivateAttrs, []),
    UserExpected = eld_user:new_from_map(#{
        key => Key,
        name => Name,
        Custom2Name => Custom2Value
    }),
    ScrubbedExpected = [<<"country">>, Custom1Name],
    {UserExpected, ScrubbedExpected} = {UserActual, ScrubbedActual}.

scrub_empty(_) ->
    Key = 123,
    Name = "foo",
    Custom1Name = <<"custom-attr-1">>,
    Custom1Value = 345,
    User = eld_user:new_from_map(#{
        key => Key,
        name => Name,
        Custom1Name => Custom1Value
    }),
    UserWithPrivateAttrs = eld_user:set_private_attribute_names([], User),
    {UserActual, ScrubbedActual} = eld_user:scrub(UserWithPrivateAttrs, []),
    {User, []} = {UserActual, ScrubbedActual}.

scrub_null(_) ->
    Key = 123,
    Name = "foo",
    Custom1Name = <<"custom-attr-1">>,
    Custom1Value = 345,
    User = eld_user:new_from_map(#{
        key => Key,
        name => Name,
        Custom1Name => Custom1Value
    }),
    UserWithPrivateAttrs = eld_user:set_private_attribute_names(null, User),
    {UserActual, ScrubbedActual} = eld_user:scrub(UserWithPrivateAttrs, []),
    {User, []} = {UserActual, ScrubbedActual}.

scrub_key(_) ->
    Key = 123,
    Name = "foo",
    Custom1Name = <<"custom-attr-1">>,
    Custom1Value = 345,
    User = eld_user:new_from_map(#{
        key => Key,
        name => Name,
        Custom1Name => Custom1Value
    }),
    UserWithPrivateAttrs = eld_user:set_private_attribute_names([key], User),
    {UserActual, ScrubbedActual} = eld_user:scrub(UserWithPrivateAttrs, []),
    {User, []} = {UserActual, ScrubbedActual}.

scrub_all_attributes_private(_) ->
    Key = 123,
    Name = "foo",
    Custom1Name = <<"custom-attr-1">>,
    Custom1Value = 345,
    User = eld_user:new_from_map(#{
        key => Key,
        name => Name,
        Custom1Name => Custom1Value
    }),
    {UserActual, ScrubbedActual} = eld_user:scrub(User, all),
    UserExpected = eld_user:new_from_map(#{key => Key}),
    {UserExpected, [<<"name">>, Custom1Name]} = {UserActual, ScrubbedActual}.

scrub_global_private_attributes(_) ->
    Key = 123,
    Name = "foo",
    Custom1Name = <<"custom-attr-1">>,
    Custom1Value = 345,
    Custom2Name = <<"custom-attr-2">>,
    Custom2Value = 789,
    User = eld_user:new_from_map(#{
        key => Key,
        name => Name,
        Custom1Name => Custom1Value,
        Custom2Name => Custom2Value
    }),
    {UserActual, ScrubbedActual} = eld_user:scrub(User, [<<"name">>, Custom2Name]),
    UserExpected = eld_user:new_from_map(#{
        key => Key,
        Custom1Name => Custom1Value
    }),
    {UserExpected, [<<"name">>, Custom2Name]} = {UserActual, ScrubbedActual}.
