-module(main).
-export([main/1]).
-include("system.hrl").

main(_) ->
    logger:set_handler_config(
        default,
        formatter,
        {logger_formatter, #{
            template => [time, " ", pid, ": ", msg, "\n"]
        }}
    ),
    system:run(#system_config{num_packages = 10000, num_belts = 8, num_trucks = 20, max_truck_dispatch_time=1000}).
