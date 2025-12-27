-module(gleam_log_ffi).
-export([timestamp_iso8601/0, write_stdout/1, write_stderr/1, is_stdout_tty/0]).

%% Get current timestamp in ISO 8601 format with milliseconds
timestamp_iso8601() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:universal_time(),
    {_, _, Micro} = os:timestamp(),
    Millis = Micro div 1000,
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~3..0BZ",
        [Year, Month, Day, Hour, Minute, Second, Millis])).

%% Write a string to stdout with newline
write_stdout(Message) ->
    io:put_chars(standard_io, [Message, $\n]),
    nil.

%% Write a string to stderr with newline
write_stderr(Message) ->
    io:put_chars(standard_error, [Message, $\n]),
    nil.

%% Check if stdout is a TTY
is_stdout_tty() ->
    case io:getopts(standard_io) of
        {error, _} -> false;
        Opts ->
            case lists:keyfind(terminal, 1, Opts) of
                {terminal, true} -> true;
                _ ->
                    %% Fallback: check TERM environment variable
                    case os:getenv("TERM") of
                        false -> false;
                        "dumb" -> false;
                        _ -> true
                    end
            end
    end.
