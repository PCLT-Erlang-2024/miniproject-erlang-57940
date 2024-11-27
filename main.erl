-module(main).
-export([main/1]).
-include("system.hrl").

env_integer(Name, Default) -> list_to_integer(os:getenv(Name, integer_to_list(Default))).

main(_) ->
    system:run(#system_config{
        num_packages = env_integer("NUM_PACKAGES", 100),
        num_belts = env_integer("NUM_BELTS", 8),
        num_trucks = env_integer("NUM_TRUCKS", 20),
        min_package_weight = env_integer("MIN_PACKAGE_WEIGHT", 3),
        max_package_weight = env_integer("MAX_PACKAGE_WEIGHT", 3),
        min_truck_capacity = env_integer("MIN_TRUCK_CAPACITY", 20),
        max_truck_capacity = env_integer("MAX_TRUCK_CAPACITY", 20),
        min_truck_dispatch_time = env_integer("MIN_TRUCK_DISPATCH_TIME", 0),
        max_truck_dispatch_time = env_integer("MAX_TRUCK_DISPATCH_TIME", 0)
    }).

