-module(truck).
-export([
    queue_start/0,
    queue_loop/1,
    queue_push/2,
    queue_pop/1,
    queue_shutdown/1,
    start/2,
    loop/1,
    load/2,
    dispatch/1,
    shutdown/1
]).
-include("system.hrl").
-record(state, {queue_pid, capacity, weight, shutdown = false}).
-record(queue_state, {trucks = [], waiting = [], shutdown = false}).

queue_start() ->
    {Pid, _} = spawn_monitor(?MODULE, queue_loop, [#queue_state{}]),
    Pid ! {keepalive},
    Pid.

queue_loop(S) ->
    receive
        Message ->
            NewState = queue_handle(S, Message),
            case NewState#queue_state.shutdown of
                false -> queue_loop(NewState);
                true -> ok
            end
    end.

queue_handle(S = #queue_state{waiting = [Pid | Waiting]}, {push, TruckPid}) ->
    util:log("wait queue not empty, giving truck ~p to ~p", [TruckPid, Pid]),
    Pid ! {pop, self(), TruckPid},
    S#queue_state{waiting = Waiting};
queue_handle(S = #queue_state{trucks = Trucks}, {push, TruckPid}) ->
    util:log("appending truck ~p to queue", [TruckPid]),
    S#queue_state{trucks = [TruckPid | Trucks]};
queue_handle(S = #queue_state{trucks = [TruckPid | Trucks]}, {pop, Pid}) ->
    util:log("truck available in queue, giving truck ~p to ~p", [TruckPid, Pid]),
    Pid ! {pop, self(), TruckPid},
    S#queue_state{trucks = Trucks};
queue_handle(S = #queue_state{waiting = Waiting, trucks = []}, {pop, Pid}) ->
    util:log("no trucks available in queue, adding ~p to wait queue", [Pid]),
    S#queue_state{waiting = [Pid | Waiting]};
queue_handle(S = #queue_state{waiting = Waiting}, {keepalive}) ->
    queue_keepalive(Waiting),
    timer:send_after(1000, {keepalive}),
    S;
queue_handle(S, {shutdown, Pid}) ->
    Pid ! ok,
    S#queue_state{shutdown = true}.

queue_keepalive([]) ->
    ok;
queue_keepalive([Pid | R]) ->
    Pid ! {keepalive, self()},
    queue_keepalive(R).

queue_push(Pid, TruckPid) ->
    Pid ! {push, TruckPid},
    ok.

queue_pop(Pid) ->
    Pid ! {pop, self()},
    queue_pop_wait(Pid).

queue_pop_wait(Pid) ->
    receive
        {pop, Pid, TruckPid} -> TruckPid;
        {keepalive, Pid} -> queue_pop_wait(Pid)
    after 5000 -> exit("truck queue failed to respond")
    end.

queue_shutdown(Pid) ->
    Pid ! {shutdown, self()},
    receive
        ok -> ok
    after 5000 -> exit("failed to receive truck queue shutdown response")
    end.

start(QueuePid, Capacity) ->
    {Pid, _} = spawn_monitor(?MODULE, loop, [
        #state{queue_pid = QueuePid, capacity = Capacity, weight = 0}
    ]),
    queue_push(QueuePid, Pid),
    Pid.

loop(S) ->
    receive
        Message ->
            NewState = handle(S, Message),
            case NewState#state.shutdown of
                false -> loop(NewState);
                true -> ok
            end
    end.

handle(S = #state{weight = W, capacity = C}, {load, Pid, Package}) ->
    util:log("trying to load package with ~p", [Package]),
    NewWeight = W + Package#package.weight,
    {Response, NewState} =
        case NewWeight of
            _ when NewWeight > C -> {{full}, S};
            _ when NewWeight < C -> {{ok, true}, S#state{weight = NewWeight}};
            _ -> {{ok, false}, S#state{weight = NewWeight}}
        end,
    Pid ! {self(), Response},
    NewState;
handle(S = #state{queue_pid = QueuePid, weight = W}, {dispatch, Pid}) ->
    Pid ! {self(), ok},
    queue_push(QueuePid, self()),
    util:log("dispatching with ~p weight", [W]),
    S#state{weight = 0};
handle(S, {shutdown}) ->
    S#state{shutdown = true}.

load(Pid, Package) ->
    Pid ! {load, self(), Package},
    receive
        {Pid, Response} -> Response
    after 5000 -> exit("failed to receive load response from truck")
    end.

dispatch(Pid) ->
    Pid ! {dispatch, self()},
    receive
        {Pid, ok} -> ok
    after 5000 -> exit("failed to receive dispatch response from truck")
    end.

shutdown(Pid) ->
    Pid ! {shutdown}.

