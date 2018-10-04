%%%-------------------------------------------------------------------
%%% @doc User data type
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(eld_user).

%% API
-export([get/2]).

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

-spec get(binary(), user()) -> term().
get(Attribute, User) ->
    Attr = get_attribute(Attribute),
    maps:get(Attr, User, undefined).

-spec get_attribute(binary()|atom()) -> binary()|atom().
get_attribute(<<"key">>) -> key;
get_attribute(Attribute) when is_binary(Attribute) -> Attribute;
get_attribute(Attribute) when is_atom(Attribute) -> Attribute.
