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

%% API
-export([get_env/1]).

%%===================================================================
%% Behavior callbacks
%%===================================================================

start(_Type, _Args) ->
    eld_sup:start_link().

stop(_State) ->
    eld:stop().

%%===================================================================
%% API
%%===================================================================

%% @doc Gets application environment variable value
%%
%% This is a convenience function to retrieve application environment variables
%% in one place.
%% @end
-spec get_env(Key :: atom()) -> undefined | {ok, term()}.
get_env(Key) ->
    application:get_env(eld, Key).
