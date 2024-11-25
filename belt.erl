-module(belt).
-export([start/2, loop/1]).
-include("system.hrl").

-record(state, {
    factory_pid,
    truck_queue_pid,
    current_truck = undefined,
    current_package = undefined,
    shutdown = false
}).

start(FactoryPid, TruckQueuePid) ->
    {Pid, _} = spawn_monitor(?MODULE, loop, [
        #state{factory_pid = FactoryPid, truck_queue_pid = TruckQueuePid}
    ]),
    Pid.

loop(_ = #state{shutdown = true, current_truck = TruckPid}) ->
    util:log("belt shutting down"),
    case TruckPid of
        undefined ->
            ok;
        Pid ->
            truck:dispatch(Pid),
            ok
    end;
loop(S = #state{factory_pid = FactoryPid, current_package = undefined}) ->
    util:log("requesting package from factory"),
    case factory:request(FactoryPid) of
        {ok, Package} -> loop(S#state{current_package = Package});
        {exhausted} -> loop(S#state{shutdown = true})
    end;
loop(S = #state{truck_queue_pid = TruckQueuePid, current_truck = undefined}) ->
    util:log("requesting truck from queue"),
    TruckPid = truck:queue_pop(TruckQueuePid),
    loop(S#state{current_truck = TruckPid});
loop(S = #state{current_truck = TruckPid, current_package = Package}) ->
    util:log("attempting to load package ~p into truck ~p", [Package, TruckPid]),
    case truck:load(TruckPid, Package) of
        {ok, true} ->
            loop(S#state{current_package = undefined});
        {ok, false} ->
            truck:dispatch(TruckPid),
            loop(S#state{current_package = undefined, current_truck = undefined});
        {full} ->
            truck:dispatch(TruckPid),
            loop(S#state{current_truck = undefined})
    end.

