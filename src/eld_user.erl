%%%-------------------------------------------------------------------
%%% @doc User data type
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(eld_user).

%% Types
-type user() :: #{
    key => key()
}. % TODO define user type

-type key() :: binary().

-export_type([user/0]).

-export_type([key/0]).
