%-------------------------------------------------------------------
%% @doc `ts_sdk_config_params' module
%%
%% Parsers and types for service parameters.
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ts_service_params).

-export([
    parse_create_instance_params/1
]).

-type create_instance_params() :: #{
    configuration => ts_sdk_config_params:sdk_config_params(),
    tag => string()
}.

-export_type([create_instance_params/0]).

-spec parse_create_instance_params(Params :: map()) -> create_instance_params().
parse_create_instance_params(#{<<"configuration">> := ConfigurationMap} = Params) ->
    Configuration = ts_sdk_config_params:parse_config_params(ConfigurationMap),
    Tag = maps:get(<<"tag">>, Params, undefined),
    #{
        configuration => Configuration,
        tag => Tag
    }.
