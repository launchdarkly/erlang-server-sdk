%%-------------------------------------------------------------------
%% @doc Reason
%% @private
%% @end
%%-------------------------------------------------------------------
-module(ldclient_eval_reason).

%% API
-export([format/1]).

-spec format(ldclient_eval:reason()) -> map().
format(target_match) -> #{<<"kind">> => <<"TARGET_MATCH">>};
format({rule_match, RuleIndex, RuleId}) -> #{kind => <<"RULE_MATCH">>, ruleIndex => RuleIndex, ruleId => RuleId};
format({rule_match, RuleIndex, RuleId, in_experiment}) -> #{kind => <<"RULE_MATCH">>, ruleIndex => RuleIndex, ruleId => RuleId, inExperiment => true};
format({prerequisite_failed, [PrereqKey|_]}) -> #{kind => <<"PREREQUISITE_FAILED">>, prerequisiteKey => PrereqKey};
format({error, client_not_ready}) -> #{kind => <<"ERROR">>, errorKind => <<"CLIENT_NOT_READY">>};
format({error, flag_not_found}) -> #{kind => <<"ERROR">>, errorKind => <<"FLAG_NOT_FOUND">>};
format({error, malformed_flag}) -> #{kind => <<"ERROR">>, errorKind => <<"MALFORMED_FLAG">>};
format({error, user_not_specified}) -> #{kind => <<"ERROR">>, errorKind => <<"USER_NOT_SPECIFIED">>};
format({error, wrong_type}) -> #{kind => <<"ERROR">>, errorKind => <<"WRONG_TYPE">>};
format({error, exception}) -> #{kind => <<"ERROR">>, errorKind => <<"EXCEPTION">>};
format(fallthrough) -> #{kind => <<"FALLTHROUGH">>};
format({fallthrough, in_experiment}) -> #{kind => <<"FALLTHROUGH">>, inExperiment => true};
format(off) -> #{kind => <<"OFF">>}.
