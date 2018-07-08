%%%-------------------------------------------------------------------
%%% @doc Flag data type
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(eld_eval).

%% API
-export([flag_key_for_user/2]).

-spec flag_key_for_user(FlagKey :: binary(), term()) -> binary().
flag_key_for_user(FlagKey, User) ->
    Flag = eld_flag:new(eld_storage_server:get(flags, FlagKey)),
    flag_for_user(Flag, User).

-spec flag_for_user(eld_flag:flag(), term()) -> binary().
flag_for_user(#{on := false, off_variation := OffVariation} = Flag, _User) ->
    eld_flag:get_variation(Flag, OffVariation).
