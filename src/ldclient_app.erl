%%-------------------------------------------------------------------
%% @doc Top level application module
%%
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
    ok = ldclient_settings:init(),
    ldclient_sup:start_link().

stop(_State) ->
    ldclient:stop_all_instances().
