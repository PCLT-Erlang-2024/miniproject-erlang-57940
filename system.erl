-module(system).
-export([run/1]).
-include("system.hrl").

assert(Cond, _) when Cond =:= true -> ok;
assert(_, Msg) -> exit(Msg).

run(Config) ->
    assert(Config#system_config.num_packages >= 0, "num packages must be >= 0"),
    assert(Config#system_config.num_belts >= 1, "num belts must be >= 1"),
    assert(Config#system_config.num_trucks >= 1, "num trucks must be >= 1"),
    assert(
        Config#system_config.min_package_weight =< Config#system_config.max_package_weight,
        "min package weight must be <= max package weight"
    ),
    assert(Config#system_config.min_package_weight >= 1, "min package weight must be >= 1"),
    assert(
        Config#system_config.max_package_weight =< Config#system_config.min_truck_capacity,
        "max package weight must be <= min truck weight"
    ),
    assert(
        Config#system_config.min_truck_capacity =< Config#system_config.max_truck_capacity,
        "min truck capacity must be <= max truck capacity"
    ),
    assert(Config#system_config.min_truck_capacity >= 1, "min truck capacity must be >= 1"),
    P = factory:generate_packages(#factory_gen_packages_config{
        num_packages = Config#system_config.num_packages,
        min_weight = Config#system_config.min_package_weight,
        max_weight = Config#system_config.max_package_weight
    }),
    F = factory:start(P),
    Q = truck:queue_start(),
    T = [
        truck:start(
            Q,
            util:random_between(
                Config#system_config.min_truck_capacity, Config#system_config.max_truck_capacity
            ),
            Config#system_config.min_truck_dispatch_time,
            Config#system_config.max_truck_dispatch_time
        )
     || _ <- lists:seq(1, Config#system_config.num_trucks)
    ],
    B = [belt:start(F, Q) || _ <- lists:seq(1, Config#system_config.num_belts)],
    util:log("waiting for belts to terminate"),
    wait(length(B)),
    util:log("sending shutdown request to factory"),
    factory:shutdown(F),
    util:log("sending shutdown request to truck queue"),
    truck:queue_shutdown(Q),
    util:log("sending shutdown request to all trucks"),
    lists:foreach(fun(Pid) -> truck:shutdown(Pid) end, T),
    wait(1 + 1 + length(T)).

wait(0) ->
    ok;
wait(N) ->
    receive
        {'DOWN', _MonitorRef, process, Pid, Reason} ->
            util:log("Process ~p terminated with reason: ~p", [Pid, Reason]),
            wait(N - 1)
    end.

