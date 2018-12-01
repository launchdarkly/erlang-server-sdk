%%-------------------------------------------------------------------
%% @doc Top level application module
%%
%% @end
%%-------------------------------------------------------------------

-module(eld_app).

-behaviour(application).

%% Behavior callbacks
-export([start/2]).
-export([stop/1]).

%%===================================================================
%% Behavior callbacks
%%===================================================================

start(_Type, _Args) ->
    ok = eld_settings:init(),
    eld_sup:start_link().

stop(_State) ->
    eld:stop_all().
