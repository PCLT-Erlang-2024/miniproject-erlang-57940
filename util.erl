-module(util).
-export([random_between/2, log/1, log/2]).

random_between(A, B) ->
    Min = min(A, B),
    Max = max(A, B),
    Min + rand:uniform(Max - Min + 1) - 1.

log(Fmt) -> log(Fmt, []).
log(Fmt, Args) ->
    %% Get the current timestamp
    {Date, {Hour, Min, Sec}} = erlang:localtime(),
    Timestamp = io_lib:format(
        "~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
        [
            element(1, Date),
            element(2, Date),
            element(3, Date),
            Hour,
            Min,
            Sec
        ]
    ),
    %% Get the current process ID
    Pid = self(),
    %% Format the message
    FormattedMsg = io_lib:format(Fmt, Args),
    %% Print to stdout
    io:format("~s [~p] ~s~n", [Timestamp, Pid, lists:flatten(FormattedMsg)]).

