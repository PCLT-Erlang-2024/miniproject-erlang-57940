-record(package, {id, weight}).

-record(factory_gen_packages_config, {num_packages, min_weight, max_weight}).

-record(system_config, {
    num_packages = 1,
    num_belts = 1,
    num_trucks = 1,
    min_package_weight = 5,
    max_package_weight = 5,
    min_truck_capacity = 20,
    max_truck_capacity = 20,
    min_truck_dispatch_time = 0,
    max_truck_dispatch_time = 0
}).

