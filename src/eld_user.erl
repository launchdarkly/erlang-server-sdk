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
    key       => key(),
    secondary := binary()
}.
% TODO define user type

-type key() :: binary().
-type attribute() :: binary() | atom().

-export_type([user/0]).
-export_type([key/0]).
-export_type([attribute/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec get(binary(), user()) -> term().
get(Attribute, User) ->
    Attr = get_attribute(Attribute),
    maps:get(Attr, User, undefined).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec get_attribute(attribute()) -> attribute().
get_attribute(<<"key">>) -> key;
get_attribute(Attribute) when is_binary(Attribute) -> Attribute;
get_attribute(Attribute) when is_atom(Attribute) -> Attribute.
