%%-------------------------------------------------------------------
%% @doc Top level application module
%% @private
%% @end
%%-------------------------------------------------------------------

-module(ldclient_app).

-behaviour(application).

%% Behavior callbacks
-export([start/2]).
-export([stop/1]).

%%===================================================================
%% Behavior callbacks
%%===================================================================

start(_Type, _Args) ->
    ok = ldclient_config:init(),
    ldclient_sup:start_link().

stop(_State) ->
    ldclient:stop_all_instances().
