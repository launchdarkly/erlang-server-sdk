%%%-------------------------------------------------------------------
%%% @doc User data type
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(eld_user).

%% API
-export([get_attribute/2]).

%% Types
-type user() :: #{
    key => key()
}.
% TODO define user type

-type key() :: binary().

-export_type([user/0]).
-export_type([key/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec get_attribute(binary(), user()) -> term().
get_attribute(Attribute, User) ->
    maps:get(Attribute, User, undefined).
