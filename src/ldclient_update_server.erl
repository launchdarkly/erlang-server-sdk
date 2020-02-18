%%-------------------------------------------------------------------
%% @doc `ldclient_update_server' module
%%
%% This is a behavior that update processors must implement.
%% @end
%%-------------------------------------------------------------------

-module(ldclient_update_server).

%% `listen' must make the worker start listening for flag and segment updates.
-callback listen(Pid :: pid()) -> ok.
