-module(ldclient_context_filter_SUITE).
-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    can_flatten_attributes_single_context/1,
    can_redact_attributes_single_context/1,
    cannot_redact_key_kind_anonymous_single_context/1,
    can_redact_attributes_multi_context/1,
    redacts_from_meta_single_context/1,
    handles_missing_attributes/1,
    can_redact_all_attributes_single_context/1,
    can_redact_all_attributes_multi_context/1,
    can_redact_single_context_anonymous_attributes/1,
    can_redact_multi_context_anonymous_attributes/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        can_flatten_attributes_single_context,
        can_redact_attributes_single_context,
        cannot_redact_key_kind_anonymous_single_context,
        can_redact_attributes_multi_context,
        redacts_from_meta_single_context,
        handles_missing_attributes,
        can_redact_all_attributes_single_context,
        can_redact_all_attributes_multi_context,
        can_redact_single_context_anonymous_attributes,
        can_redact_multi_context_anonymous_attributes
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

can_flatten_attributes_single_context(_) ->
    TestContext =
        ldclient_context:set(<<"org">>, <<"anAttribute">>, <<"aValue">>,
        ldclient_context:new(<<"org-key">>, <<"org">>)),
    #{
        <<"kind">> := <<"org">>,
        <<"key">> := <<"org-key">>,
        <<"anAttribute">> := <<"aValue">>
    } = ldclient_context_filter:format_context_for_event([], TestContext).

can_redact_attributes_single_context(_) ->
    TestContext =
        ldclient_context:set(<<"org">>, <<"anAttribute">>, <<"aValue">>,
        ldclient_context:set(<<"org">>, <<"nested">>, #{
            <<"key1">> => <<"value1">>,
            <<"key2">> => <<"value2">>
        },
        ldclient_context:new(<<"org-key">>, <<"org">>))),
    #{
        <<"kind">> := <<"org">>,
        <<"key">> := <<"org-key">>,
        <<"nested">> := #{<<"key2">> := <<"value2">>},
        <<"_meta">> := #{<<"redactedAttributes">> := [<<"anAttribute">>, <<"/nested/key1">>]}
    } = ldclient_context_filter:format_context_for_event([
        ldclient_attribute_reference:new(<<"/nested/key1">>),
        ldclient_attribute_reference:new(<<"anAttribute">>)
    ], TestContext).

cannot_redact_key_kind_anonymous_single_context(_) ->
    TestContext =
        ldclient_context:set(<<"org">>, <<"anAttribute">>, <<"aValue">>,
            ldclient_context:set(<<"org">>, <<"nested">>, #{
                <<"key1">> => <<"value1">>,
                <<"key2">> => <<"value2">>
            },
                ldclient_context:new(<<"org-key">>, <<"org">>))),
    #{
        <<"kind">> := <<"org">>,
        <<"key">> := <<"org-key">>,
        <<"anAttribute">> := <<"aValue">>,
        <<"nested">> := #{
            <<"key1">> := <<"value1">>,
            <<"key2">> := <<"value2">>
        %% Also note no _meta because there are no redacted attributes.
        }
    } = ldclient_context_filter:format_context_for_event([
        ldclient_attribute_reference:new(<<"key">>),
        ldclient_attribute_reference:new(<<"kind">>),
        ldclient_attribute_reference:new(<<"anonymous">>)
    ], TestContext).

can_redact_attributes_multi_context(_) ->
    TestContext = ldclient_context:new_multi_from([
        ldclient_context:set(<<"org">>, <<"anAttribute">>, <<"aValue">>,
            ldclient_context:set(<<"org">>, <<"nested">>, #{
                <<"key1">> => <<"value1">>,
                <<"key2">> => <<"value2">>
            },
            ldclient_context:new(<<"org-key">>, <<"org">>))),
        ldclient_context:set(<<"user">>, <<"anAttribute">>, <<"aValue">>,
            ldclient_context:set(<<"user">>, <<"nested">>, #{
                <<"key1">> => <<"value1">>,
                <<"key2">> => <<"value2">>
            },
                ldclient_context:new(<<"user-key">>, <<"user">>)))
    ]),
    #{
        <<"kind">> := <<"multi">>,
        <<"org">> := #{
            <<"key">> := <<"org-key">>,
            <<"nested">> := #{<<"key2">> := <<"value2">>},
            <<"_meta">> := #{<<"redactedAttributes">> := [<<"anAttribute">>, <<"/nested/key1">>]}
        },
        <<"user">> := #{
            <<"key">> := <<"user-key">>,
            <<"nested">> := #{<<"key2">> := <<"value2">>},
            <<"_meta">> := #{<<"redactedAttributes">> := [<<"anAttribute">>, <<"/nested/key1">>]}
        }
    } = ldclient_context_filter:format_context_for_event([
        ldclient_attribute_reference:new(<<"/nested/key1">>),
        ldclient_attribute_reference:new(<<"anAttribute">>)
    ], TestContext).

redacts_from_meta_single_context(_) ->
    %% Mixed global and meta.
    TestContext =
        ldclient_context:set_private_attributes([<<"anAttribute">>],
            ldclient_context:set(<<"org">>, <<"anAttribute">>, <<"aValue">>,
                ldclient_context:set(<<"org">>, <<"nested">>, #{
                    <<"key1">> => <<"value1">>,
                    <<"key2">> => <<"value2">>
                },
                    ldclient_context:new(<<"org-key">>, <<"org">>)))),
    #{
        <<"kind">> := <<"org">>,
        <<"key">> := <<"org-key">>,
        <<"nested">> := #{<<"key2">> := <<"value2">>},
        <<"_meta">> := #{<<"redactedAttributes">> := [<<"anAttribute">>, <<"/nested/key1">>]}
    } = ldclient_context_filter:format_context_for_event([
        ldclient_attribute_reference:new(<<"/nested/key1">>)
    ], TestContext),
    %% Only Meta
    TestContext2 =
        ldclient_context:set_private_attributes([<<"anAttribute">>, <<"/nested/key1">>],
            ldclient_context:set(<<"org">>, <<"anAttribute">>, <<"aValue">>,
                ldclient_context:set(<<"org">>, <<"nested">>, #{
                    <<"key1">> => <<"value1">>,
                    <<"key2">> => <<"value2">>
                },
                    ldclient_context:new(<<"org-key">>, <<"org">>)))),
    #{
        <<"kind">> := <<"org">>,
        <<"key">> := <<"org-key">>,
        <<"nested">> := #{<<"key2">> := <<"value2">>},
        <<"_meta">> := #{<<"redactedAttributes">> := [<<"anAttribute">>, <<"/nested/key1">>]}
    } = ldclient_context_filter:format_context_for_event([], TestContext2).

handles_missing_attributes(_) ->
    NoAttributesContext = ldclient_context:new(<<"org-key">>, <<"org">>),
    #{
        <<"kind">> := <<"org">>,
        <<"key">> := <<"org-key">>
    } = ldclient_context_filter:format_context_for_event([], NoAttributesContext),
    #{
        <<"kind">> := <<"org">>,
        <<"key">> := <<"org-key">>
        } = ldclient_context_filter:format_context_for_event([
        ldclient_attribute_reference:new(<<"/nested/key1">>),
        ldclient_attribute_reference:new(<<"anAttribute">>)
    ], NoAttributesContext),
    AttributesContext =
        ldclient_context:set_private_attributes([<<"missingAttribute">>],
            ldclient_context:set(<<"org">>, <<"anAttribute">>, <<"aValue">>,
                ldclient_context:set(<<"org">>, <<"nested">>, #{
                    <<"key1">> => <<"value1">>,
                    <<"key2">> => <<"value2">>
                },
                    ldclient_context:new(<<"org-key">>, <<"org">>)))),
    #{
        <<"kind">> := <<"org">>,
        <<"key">> := <<"org-key">>,
        <<"anAttribute">> := <<"aValue">>,
        <<"nested">> := #{<<"key1">> := <<"value1">>},
        <<"_meta">> := #{<<"redactedAttributes">> := [<<"/nested/key2">>]}
    } = ldclient_context_filter:format_context_for_event([
        ldclient_attribute_reference:new(<<"potato">>),
        ldclient_attribute_reference:new(<<"/nested/key2">>)
    ], AttributesContext),
    #{
        <<"kind">> := <<"org">>,
        <<"key">> := <<"org-key">>,
        <<"anAttribute">> := <<"aValue">>,
        <<"nested">> := #{<<"key1">> := <<"value1">>, <<"key2">> := <<"value2">>}
    } = ldclient_context_filter:format_context_for_event([
        ldclient_attribute_reference:new(<<"potato">>)
    ], AttributesContext).

can_redact_all_attributes_single_context(_) ->
    TestContext =
        ldclient_context:set(name, <<"the-name">>,
        ldclient_context:set(anonymous, true,
        ldclient_context:set(<<"org">>, <<"anAttribute">>, <<"aValue">>,
            ldclient_context:set(<<"org">>, <<"nested">>, #{
                <<"key1">> => <<"value1">>,
                <<"key2">> => <<"value2">>
            },
                ldclient_context:new(<<"org-key">>, <<"org">>))))),
    #{
        <<"kind">> := <<"org">>,
        <<"key">> := <<"org-key">>,
        <<"anonymous">> := true,
        <<"_meta">> := #{
            <<"redactedAttributes">> := [
                <<"name">>,
                <<"anAttribute">>,
                <<"nested">>
            ]
        }
    } = ldclient_context_filter:format_context_for_event(all, TestContext).

can_redact_all_attributes_multi_context(_) ->
    TestContext = ldclient_context:new_multi_from([
        ldclient_context:set(<<"org">>, <<"anAttribute">>, <<"aValue">>,
            ldclient_context:set(<<"org">>, <<"nested">>, #{
                <<"key1">> => <<"value1">>,
                <<"key2">> => <<"value2">>
            },
                ldclient_context:new(<<"org-key">>, <<"org">>))),
        ldclient_context:set(<<"user">>, <<"anAttribute">>, <<"aValue">>,
            ldclient_context:set(<<"user">>, <<"nested">>, #{
                <<"key1">> => <<"value1">>,
                <<"key2">> => <<"value2">>
            },
                ldclient_context:new(<<"user-key">>, <<"user">>)))
    ]),
    #{
        <<"kind">> := <<"multi">>,
        <<"org">> := #{
            <<"key">> := <<"org-key">>,
            <<"_meta">> := #{<<"redactedAttributes">> := [<<"anAttribute">>, <<"nested">>]}
        },
        <<"user">> := #{
            <<"key">> := <<"user-key">>,
            <<"_meta">> := #{<<"redactedAttributes">> := [<<"anAttribute">>, <<"nested">>]}
        }
    } = ldclient_context_filter:format_context_for_event(all, TestContext).

can_redact_single_context_anonymous_attributes(_) ->
    TestContext =
        ldclient_context:set(name, <<"the-name">>,
        ldclient_context:set(anonymous, true,
        ldclient_context:set(<<"org">>, <<"anAttribute">>, <<"aValue">>,
            ldclient_context:set(<<"org">>, <<"nested">>, #{
                <<"key1">> => <<"value1">>,
                <<"key2">> => <<"value2">>
            },
                ldclient_context:new(<<"org-key">>, <<"org">>))))),
    #{
        <<"kind">> := <<"org">>,
        <<"key">> := <<"org-key">>,
        <<"anonymous">> := true,
        <<"_meta">> := #{
            <<"redactedAttributes">> := [
                <<"name">>,
                <<"anAttribute">>,
                <<"nested">>
            ]
        }
    } = ldclient_context_filter:format_context_for_event_with_anonyous_redaction([], TestContext).

can_redact_multi_context_anonymous_attributes(_) ->
    TestContext = ldclient_context:new_multi_from([
        ldclient_context:set(<<"org">>, <<"anAttribute">>, <<"aValue">>,
            ldclient_context:set(anonymous, true,
            ldclient_context:set(<<"org">>, <<"nested">>, #{
                <<"key1">> => <<"value1">>,
                <<"key2">> => <<"value2">>
            },
                ldclient_context:new(<<"org-key">>, <<"org">>)))),
        ldclient_context:set(<<"user">>, <<"anAttribute">>, <<"aValue">>,
            ldclient_context:set(<<"user">>, <<"nested">>, #{
                <<"key1">> => <<"value1">>,
                <<"key2">> => <<"value2">>
            },
                ldclient_context:new(<<"user-key">>, <<"user">>)))
    ]),
    #{
        <<"kind">> := <<"multi">>,
        <<"org">> := #{
            <<"key">> := <<"org-key">>,
            <<"anonymous">> := true,
            <<"_meta">> := #{<<"redactedAttributes">> := [<<"anAttribute">>, <<"nested">>]}
        },
        <<"user">> := #{
            <<"key">> := <<"user-key">>,
            <<"anAttribute">> := <<"aValue">>,
            <<"nested">> := #{<<"key1">> := <<"value1">>, <<"key2">> := <<"value2">>}
        }
    } = ldclient_context_filter:format_context_for_event_with_anonyous_redaction([], TestContext).
