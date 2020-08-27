-module(http_server_timeout_once).

%% API
-export([start/1]).

start(T) when is_integer(T) ->
    loop(create_state(T)).

loop(#{current := T} = State) ->
    ok = receive
        Pid when is_pid(Pid) -> Pid ! T, ok;
        _ -> ok
    end,
    loop(next_state(State)).

create_state(T) -> #{current => T, timeout => T}.

next_state(#{current := 0, timeout := T} = State) -> State#{current := T};
next_state(State) -> State#{current := 0}.
