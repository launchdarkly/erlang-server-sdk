-module(ldclient_context_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    can_make_single_kind_context/1,
    can_get_kind_of_single_kind_context/1,
    can_set_attribute_in_single_context/1,
    can_get_attribute_in_single_context/1,
    returns_null_for_missing_attribute_in_single_context/1,
    returns_null_for_invalid_attribute_reference/1,
    can_create_a_multi_context_from_singles/1,
    can_get_kinds_of_multi_context/1,
    can_get_attribute_in_multi_context/1,
    multi_created_from_single_is_single/1,
    setting_attribute_by_atom_uses_binary/1,
    can_validate_kinds/1,
    can_set_built_in_attributes/1,
    can_create_single_from_map/1,
    can_create_multi_context_from_map/1,
    can_validate_single_context/1,
    can_validate_multi_context/1,
    can_set_private_attributes/1,
    can_get_built_in_attributes/1,
    can_encode_context_key/1,
    can_get_canonical_key/1,
    can_create_context_from_basic_user/1,
    can_create_context_from_full_user_custom/1,
    can_allow_empty_key_for_user/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        can_make_single_kind_context,
        can_get_kind_of_single_kind_context,
        can_set_attribute_in_single_context,
        can_get_attribute_in_single_context,
        returns_null_for_missing_attribute_in_single_context,
        returns_null_for_invalid_attribute_reference,
        can_create_a_multi_context_from_singles,
        can_get_kinds_of_multi_context,
        can_get_attribute_in_multi_context,
        multi_created_from_single_is_single,
        setting_attribute_by_atom_uses_binary,
        can_validate_kinds,
        can_set_built_in_attributes,
        can_create_single_from_map,
        can_create_multi_context_from_map,
        can_validate_single_context,
        can_validate_multi_context,
        can_set_private_attributes,
        can_get_built_in_attributes,
        can_encode_context_key,
        can_get_canonical_key,
        can_create_context_from_basic_user,
        can_create_context_from_full_user_custom,
        can_allow_empty_key_for_user
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

can_make_single_kind_context(_) ->
    #{key := <<"user-key">>, kind := <<"user">>} = ldclient_context:new(<<"user-key">>),
    #{key := <<"org-key">>, kind := <<"org">>} = ldclient_context:new(<<"org-key">>, <<"org">>).

can_get_kind_of_single_kind_context(_) ->
    [<<"user">>] = ldclient_context:get_kinds(ldclient_context:new(<<"user-key">>)),
    [<<"org">>] = ldclient_context:get_kinds(ldclient_context:new(<<"org-key">>, <<"org">>)).

can_set_attribute_in_single_context(_) ->
    #{key := <<"user-key">>, kind := <<"user">>, attributes := #{<<"myAttribute">> := <<"myValue">>}} =
        ldclient_context:set(<<"myAttribute">>, <<"myValue">>, ldclient_context:new(<<"user-key">>)),
    #{key := <<"org-key">>, kind := <<"org">>, attributes := #{<<"myAttribute">> := <<"myValue">>}} =
        ldclient_context:set(<<"myAttribute">>, <<"myValue">>, ldclient_context:new(<<"org-key">>, <<"org">>)).

can_get_attribute_in_single_context(_) ->
    <<"myValue">> = ldclient_context:get(<<"user">>, <<"/myAttribute">>,
        #{kind => <<"user">>, key => <<"my-key">>, attributes => #{<<"myAttribute">> => <<"myValue">>}}),
    %% Works with a pre-processed attribute as well.
    <<"myValue">> = ldclient_context:get(<<"user">>, ldclient_attribute_reference:new(<<"/myAttribute">>),
        #{kind => <<"user">>, key => <<"my-key">>, attributes => #{<<"myAttribute">> => <<"myValue">>}}),
    <<"myValue">> = ldclient_context:get(<<"user">>, <<"/myContainer/myAttribute">>,
        #{kind => <<"user">>, key => <<"my-key">>,
            attributes => #{<<"myContainer">> => #{<<"myAttribute">> => <<"myValue">>}}}).

returns_null_for_missing_attribute_in_single_context(_) ->
    null = ldclient_context:get(<<"user">>, <<"/myPotato">>,
        #{kind => <<"user">>, <<"myAttribute">> => <<"myValue">>, key => <<"my-key">>}),
    null = ldclient_context:get(<<"user">>, <<"/myContainer/myPotato">>,
        #{kind => <<"user">>, <<"myContainer">> => #{<<"myAttribute">> => <<"myValue">>}, key => <<"my-key">>}).

returns_null_for_invalid_attribute_reference(_) ->
    null = ldclient_context:get(<<"user">>, <<"/myAttribute/">>,
        #{kind => <<"user">>, <<"myAttribute">> => <<"myValue">>, key => <<"my-key">>}),
    null = ldclient_context:get(<<"user">>, <<"/myContainer/myAttribute~3">>,
        #{kind => <<"user">>, <<"myContainer">> => #{<<"myAttribute">> => <<"myValue">>}, key => <<"my-key">>}).

can_create_a_multi_context_from_singles(_) ->
    #{kind := <<"multi">>, <<"user">> := #{key := <<"user-key">>}, <<"org">> := #{key := <<"org-key">>}} =
        ldclient_context:new_multi_from([
            ldclient_context:new(<<"user-key">>),
            ldclient_context:new(<<"org-key">>, <<"org">>)]).

multi_created_from_single_is_single(_) ->
    %% If you attempt to create a multi context from a single context, then you just get that same single context back.
    #{kind := <<"org">>, key := <<"org-key">>} =
        ldclient_context:new_multi_from([ldclient_context:new(<<"org-key">>, <<"org">>)]).

can_get_kinds_of_multi_context(_) ->
    [<<"org">>, <<"user">>] =
        ldclient_context:get_kinds(#{
            kind => <<"multi">>,
            <<"user">> => #{key => <<"user-key">>},
            <<"org">> => #{key => <<"org-key">>}}).

can_get_attribute_in_multi_context(_) ->
    MultiContext = #{
        kind => <<"multi">>,
        <<"user">> => #{key => <<"user-key">>, attributes => #{<<"firstName">> => <<"Bob">>}},
        <<"org">> => #{key => <<"org-key">>, attributes => #{<<"detail">> => #{<<"name">> => <<"LaunchDarkly">>}}}},
    <<"Bob">> = ldclient_context:get(<<"user">>, <<"firstName">>, MultiContext),
    <<"LaunchDarkly">> = ldclient_context:get(<<"org">>, <<"/detail/name">>, MultiContext).

setting_attribute_by_atom_uses_binary(_) ->
    #{key := <<"user-key">>, kind := <<"user">>, attributes := #{<<"myAttribute">> := <<"myValue">>}} =
        ldclient_context:set(myAttribute, <<"myValue">>, ldclient_context:new(<<"user-key">>)),
    #{key := <<"org-key">>, kind := <<"org">>, attributes := #{<<"myAttribute">> := <<"myValue">>}} =
        ldclient_context:set(myAttribute, <<"myValue">>, ldclient_context:new(<<"org-key">>, <<"org">>)).

can_validate_kinds(_) ->
    true = ldclient_context:is_valid_kind(<<"user">>),
    true = ldclient_context:is_valid_kind(<<"org">>),
    true = ldclient_context:is_valid_kind(<<"_._">>),
    false = ldclient_context:is_valid_kind(<<"kind">>),
    false = ldclient_context:is_valid_kind(<<" ">>),
    false = ldclient_context:is_valid_kind(<<>>),
    false = ldclient_context:is_valid_kind(<<"">>),
    true = ldclient_context:is_valid_kind(<<"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz._-">>),
    lists:foreach(fun(Grapheme) ->
        false = ldclient_context:is_valid_kind(Grapheme)
                  end,
        [
            <<"$"/utf8>>, <<"%"/utf8>>, <<"&"/utf8>>, <<"'"/utf8>>, <<"("/utf8>>, <<")"/utf8>>, <<"*"/utf8>>,
            <<"+"/utf8>>, <<","/utf8>>, <<"/"/utf8>>, <<":"/utf8>>, <<";"/utf8>>, <<"<"/utf8>>, <<"="/utf8>>,
            <<">"/utf8>>, <<"?"/utf8>>, <<"@"/utf8>>, <<"["/utf8>>, <<"\\"/utf8>>, <<"]"/utf8>>, <<"^"/utf8>>,
            <<"`"/utf8>>, <<"{"/utf8>>, <<"|"/utf8>>, <<"}"/utf8>>, <<"~"/utf8>>, <<" "/utf8>>, <<"¡"/utf8>>,
            <<"¢"/utf8>>, <<"£"/utf8>>, <<"¤"/utf8>>, <<"¥"/utf8>>, <<"¦"/utf8>>, <<"§"/utf8>>, <<"¨"/utf8>>,
            <<"©"/utf8>>, <<"ª"/utf8>>, <<"«"/utf8>>, <<"¬"/utf8>>, <<"®"/utf8>>, <<"¯"/utf8>>, <<"°"/utf8>>,
            <<"±"/utf8>>, <<"²"/utf8>>, <<"³"/utf8>>, <<"´"/utf8>>, <<"µ"/utf8>>, <<"¶"/utf8>>, <<"·"/utf8>>,
            <<"¸"/utf8>>, <<"¹"/utf8>>, <<"º"/utf8>>, <<"»"/utf8>>, <<"¼"/utf8>>, <<"½"/utf8>>, <<"¾"/utf8>>,
            <<"¿"/utf8>>, <<"À"/utf8>>, <<"汉"/utf8>>, <<"字"/utf8>>, <<"#"/utf8>>
        ]).

can_set_built_in_attributes(_) ->
    #{key := <<"after-key">>} = ldclient_context:set(key, <<"after-key">>, ldclient_context:new(<<"before-key">>)),
    #{key := <<"after-key">>} = ldclient_context:set(<<"key">>, <<"after-key">>, ldclient_context:new(<<"before-key">>)),
    #{kind := <<"after-kind">>} = ldclient_context:set(kind, <<"after-kind">>,
        ldclient_context:new(<<"key">>, <<"before-kind">>)),
    #{kind := <<"after-kind">>} = ldclient_context:set(<<"kind">>, <<"after-kind">>,
        ldclient_context:new(<<"key">>, <<"before-kind">>)),
    #{anonymous := false} = ldclient_context:set(anonymous, false, ldclient_context:new(<<"before-key">>)),
    #{anonymous := true} = ldclient_context:set(<<"anonymous">>, true, ldclient_context:new(<<"before-key">>)),
    #{private_attributes := []} = ldclient_context:set(private_attributes, [], ldclient_context:new(<<"before-key">>)),
    #{private_attributes := []} = ldclient_context:set(<<"private_attributes">>, [],
        ldclient_context:new(<<"before-key">>)).

can_create_single_from_map(_) ->
    #{key := <<"my-key">>, kind := <<"user">>} = ldclient_context:new_from_map(#{key => <<"my-key">>}),
    #{key := <<"my-key">>, kind := <<"the-kind">>} =
        ldclient_context:new_from_map(#{key => <<"my-key">>, kind => <<"the-kind">>}),
    #{key := <<"my-key">>} = ldclient_context:new_from_map(#{<<"key">> => <<"my-key">>}),
    #{key := <<"my-key">>, kind := <<"the-kind">>} =
        ldclient_context:new_from_map(#{<<"key">> => <<"my-key">>, <<"kind">> => <<"the-kind">>}),
    #{key := <<"my-key">>, kind := <<"user">>, attributes := #{<<"nested">> := #{
        <<"deeper">> := #{
            <<"value">> := <<"my-value">>
        }
    }}} = ldclient_context:new_from_map(#{key => <<"my-key">>, kind => <<"user">>, <<"attributes">> => #{nested => #{
        deeper => #{
            value => <<"my-value">>
        }
    }}}).

can_create_multi_context_from_map(_) ->
    #{
        kind := <<"multi">>,
        <<"meal">> := #{
            key := <<"user-key">>,
            name := <<"the-name">>,
            attributes := #{
                <<"potato">> := #{
                    <<"bacon">> := true,
                    <<"cheese">> := true
                }
            }
        },
        <<"location">> := #{
            key := <<"location-key">>
        }
    } = ldclient_context:new_from_map(#{
        kind => <<"multi">>,
        <<"meal">> => #{
            key => <<"user-key">>,
            <<"name">> => <<"the-name">>, %% Key that will become an atom.
            attributes => #{
                potato => #{ %% Key that will become a binary.
                    <<"bacon">> => true,
                    <<"cheese">> => true
                }
            }
        },
        <<"location">> => #{
            key => <<"location-key">>
        }
    }).

can_validate_single_context(_) ->
    true = ldclient_context:is_valid(#{key => <<"good-key">>}, false),
    true = ldclient_context:is_valid(#{key => <<"good-key">>, kind => <<"good-kind">>}, false),
    false = ldclient_context:is_valid(#{key => <<>>, kind => <<"good-kind">>}, false),
    false = ldclient_context:is_valid(#{key => 17, kind => <<"good-kind">>}, false),
    false = ldclient_context:is_valid(#{key => <<"good-key">>, kind => <<"bad$kind">>}, false),
    false = ldclient_context:is_valid(#{key => <<>>}, false).

can_validate_multi_context(_) ->
    true = ldclient_context:is_valid(#{
        kind => <<"multi">>,
        <<"user">> => #{key => <<"user-key">>},
        <<"org">> => #{key => <<"org-key">>}
    }, false),
    false = ldclient_context:is_valid(#{
        kind => <<"multi">>,
        <<"user">> => <<"not-a-context">>,
        <<"org">> => #{key => <<"org-key">>}
    }, false),
    false = ldclient_context:is_valid(#{
        kind => <<"multi">>,
        <<"us$er">> => #{key => <<"user-key">>},
        <<"org">> => #{key => <<"org-key">>}
    }, false).

can_set_private_attributes(_) ->
    #{private_attributes := [<<"attr1">>, <<"attr2">>]} =
        ldclient_context:set_private_attributes([<<"attr1">>, <<"attr2">>], ldclient_context:new(<<"the-key">>)),
    #{private_attributes := [<<"attr1">>, <<"attr2">>]} =
        ldclient_context:set_private_attributes(<<"user">>, [<<"attr1">>, <<"attr2">>],
            ldclient_context:new(<<"the-key">>)),
    #{<<"org">> := #{private_attributes := [<<"attr1">>, <<"attr2">>]}} =
        ldclient_context:set_private_attributes(<<"org">>, [<<"attr1">>, <<"attr2">>],
            ldclient_context:new_multi_from([
                ldclient_context:new(<<"user-key">>),
                ldclient_context:new(<<"org-key">>, <<"org">>)
            ])
        ).

can_get_built_in_attributes(_) ->
    TestContext = #{
        kind => <<"user">>, %% Will not get this one, kinds work different.
        private_attributes => [], %% Cannot get this either, as it is a meta attribute.
        key => <<"user-key">>,
        anonymous => true
    },
    <<"user-key">> = ldclient_context:get(<<"user">>, <<"key">>, TestContext),
    true = ldclient_context:get(<<"user">>, <<"anonymous">>, TestContext),
    null = ldclient_context:get(<<"user">>, <<"kind">>, TestContext),
    null = ldclient_context:get(<<"user">>, <<"private_attributes">>, TestContext).

can_encode_context_key(_) ->
    <<"my%25silly%3Akey%25%3A%3A%25">> = ldclient_context:encode_key(<<"my%silly:key%::%">>),
    <<"my-less-silly-key">> = ldclient_context:encode_key(<<"my-less-silly-key">>).

can_get_canonical_key(_) ->
    <<"test">> = ldclient_context:get_canonical_key(#{kind => <<"user">>, key => <<"test">>}),
    <<"org:orgtest">> = ldclient_context:get_canonical_key(#{kind => <<"org">>, key => <<"orgtest">>}),
    <<"org:orgtest:user:usertest:zebra:a1">> = ldclient_context:get_canonical_key(
        #{
            kind => <<"multi">>,
            <<"zebra">> => #{key => <<"a1">>},
            <<"user">> => #{key => <<"usertest">>},
            <<"org">> => #{key => <<"orgtest">>}
        }
    ),
    %% Map order should not affect key.
    <<"org:orgtest:user:usertest:zebra:a1">> = ldclient_context:get_canonical_key(
        #{
            kind => <<"multi">>,
            <<"org">> => #{key => <<"orgtest">>},
            <<"zebra">> => #{key => <<"a1">>},
            <<"user">> => #{key => <<"usertest">>}
        }
    ),
    <<"org:org%25%3Atest:user:user%25%3Atest">> = ldclient_context:get_canonical_key(
        #{
            kind => <<"multi">>,
            <<"user">> => #{key => <<"user%:test">>},
            <<"org">> => #{key => <<"org%:test">>}
        }
    ).

can_create_context_from_basic_user(_) ->
    #{
        kind := <<"user">>,
        key := <<"user-key">>
    } = ldclient_context:new_from_user(ldclient_user:new(<<"user-key">>)).

can_create_context_from_full_user_custom(_) ->
    Key = <<"12345">>,
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
    CustomMap = #{
        CustomKey1 => CustomValue1,
        CustomKey2 => CustomValue2
    },
    User = #{
        key => Key,
        ip => Ip,
        country => Country,
        email => Email,
        first_name => FirstName,
        last_name => LastName,
        avatar => Avatar,
        name => Name,
        anonymous => Anonymous,
        custom => CustomMap
    },
    Context = ldclient_context:new_from_user(User),
    true = ldclient_context:is_valid(Context, false),
    #{
        key := Key,
        kind := <<"user">>,
        <<"ip">> := Ip,
        <<"country">> := Country,
        <<"email">> := Email,
        <<"firstName">> := FirstName,
        <<"lastName">> := LastName,
        <<"avatar">> := Avatar,
        <<"name">> := Name,
        <<"anonymous">> := Anonymous,
        CustomKey1 := CustomValue1,
        CustomKey2 := CustomValue2
    } = Context.

can_allow_empty_key_for_user(_) ->
    true = ldclient_context:is_valid(
        ldclient_context:new_from_user(
            ldclient_context:new_from_user(ldclient_user:new(<<>>))), true).
