-module(factory).
-export([start/1, loop/1, request/1, shutdown/1, generate_packages/1]).
-include("system.hrl").

generate_package(_ = #factory_gen_packages_config{min_weight = Min, max_weight = Max}, Id) ->
    #package{
        id = Id,
        weight = util:random_between(Min, Max)
    }.

generate_packages(N) when is_integer(N) ->
    generate_packages(#factory_gen_packages_config{num_packages = N, min_weight = 1, max_weight = 1});
generate_packages(C = #factory_gen_packages_config{num_packages = N}) ->
    generate_packages(C, N, []).
generate_packages(_, 0, L) -> L;
generate_packages(C, N, L) -> generate_packages(C, N - 1, [generate_package(C, N) | L]).

start(PackageList) ->
    {Pid, _} = spawn_monitor(?MODULE, loop, [PackageList]),
    Pid.

loop(PackageList) ->
    receive
        {shutdown, Pid} ->
            Pid ! ok;
        {request, Pid} ->
            {Response, Rem} =
                case PackageList of
                    [] -> {{response, false, undefined}, []};
                    [Package | R] -> {{response, true, Package}, R}
                end,
            Pid ! Response,
            loop(Rem);
        Message ->
            io:format("factory received unknown message: ~p~n", [Message]),
            loop(PackageList)
    end.

request(Pid) ->
    Pid ! {request, self()},
    receive
        {response, true, Package} -> {ok, Package};
        {response, false, _} -> {exhausted}
    after 5000 -> exit("failed to receive factory response")
    end.

shutdown(Pid) ->
    Pid ! {shutdown, self()},
    receive
        ok -> ok
    after 5000 -> exit("failed to receive factory shutdown response")
    end.

