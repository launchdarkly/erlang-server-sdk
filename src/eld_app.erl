-module(eld_app).

-behaviour(application).

%% Behavior callbacks
-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    eld_sup:start_link().

stop(_State) ->
    eld:stop().
