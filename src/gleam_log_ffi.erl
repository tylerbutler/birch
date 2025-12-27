-module(gleam_log_ffi).
-export([timestamp_iso8601/0, write_stdout/1, write_stderr/1, is_stdout_tty/0,
         get_global_config/0, set_global_config/1, clear_global_config/0]).

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

%% Global configuration storage using persistent_term.
%% persistent_term is ideal for read-heavy, write-rarely data like logging config.
%% It's also thread-safe for concurrent access.

-define(GLOBAL_CONFIG_KEY, gleam_log_global_config).

%% Get the global configuration. Returns {ok, Config} or {error, nil}.
get_global_config() ->
    try persistent_term:get(?GLOBAL_CONFIG_KEY) of
        Config -> {ok, Config}
    catch
        error:badarg -> {error, nil}
    end.

%% Set the global configuration.
set_global_config(Config) ->
    persistent_term:put(?GLOBAL_CONFIG_KEY, Config),
    nil.

%% Clear the global configuration (reset to unset state).
clear_global_config() ->
    try persistent_term:erase(?GLOBAL_CONFIG_KEY) of
        _ -> nil
    catch
        error:badarg -> nil
    end.
