%% Custom :logger formatter that applies birch's formatting to all log events.
%%
%% This module implements the :logger formatter callback interface.
%% It is installed on the default :logger handler by birch's setup functions
%% and uses a stored Gleam formatter closure to format both birch-originated
%% and OTP log events with birch's presentation style.
%%
%% Birch-originated logs are detected by the presence of `birch_logger_name`
%% in the log event metadata. OTP logs extract the logger name from
%% `mfa` or `domain` metadata fields.

-module(birch_logger_formatter).
-export([format/2]).

%% :logger formatter callback.
%% Config must contain: #{format_fn := fun(LogRecord) -> binary()}
-spec format(logger:log_event(), logger:formatter_config()) -> unicode:chardata().
format(#{level := Level, msg := Msg, meta := Meta}, #{format_fn := FormatFn}) ->
    LogRecord = build_log_record(Level, Msg, Meta),
    Formatted = FormatFn(LogRecord),
    [Formatted, "\n"];
format(#{level := Level, msg := Msg}, _Config) ->
    %% Fallback if format_fn is not configured
    Message = format_msg(Msg),
    LevelStr = atom_to_binary(Level, utf8),
    [LevelStr, " ", Message, "\n"].

%% Build a birch LogRecord tuple from a :logger event.
%% LogRecord = {log_record, Timestamp, Level, LoggerName, Message, Metadata, CallerId}
build_log_record(Level, Msg, Meta) ->
    Timestamp = format_timestamp(maps:get(time, Meta, undefined)),
    GleamLevel = erlang_level_to_gleam(Level),
    Message = format_msg(Msg),
    %% Detect birch-originated logs vs OTP logs
    case maps:get(birch_logger_name, Meta, undefined) of
        undefined ->
            %% OTP log - extract name from mfa/domain
            LoggerName = format_logger_name(Meta),
            Metadata = format_metadata(Meta),
            {log_record, Timestamp, GleamLevel, LoggerName, Message, Metadata, none};
        BirchName ->
            %% Birch-originated log - use stored birch metadata
            BirchMeta = maps:get(birch_metadata, Meta, []),
            CallerId = maps:get(birch_caller_id, Meta, none),
            {log_record, Timestamp, GleamLevel, BirchName, Message, BirchMeta, CallerId}
    end.

%% ============================================================================
%% Helper functions
%% ============================================================================

%% Convert Erlang log level atom to Gleam Level type.
erlang_level_to_gleam(emergency) -> fatal;
erlang_level_to_gleam(alert) -> fatal;
erlang_level_to_gleam(critical) -> fatal;
erlang_level_to_gleam(error) -> err;
erlang_level_to_gleam(warning) -> warn;
erlang_level_to_gleam(notice) -> info;
erlang_level_to_gleam(info) -> info;
erlang_level_to_gleam(debug) -> debug;
erlang_level_to_gleam(_) -> info.

%% Format the message from various :logger message formats.
format_msg({string, String}) when is_list(String) ->
    unicode:characters_to_binary(String);
format_msg({string, String}) when is_binary(String) ->
    String;
format_msg({report, Report}) ->
    unicode:characters_to_binary(io_lib:format("~p", [Report]));
format_msg({Format, Args}) when is_list(Format) ->
    unicode:characters_to_binary(io_lib:format(Format, Args));
format_msg(Msg) when is_binary(Msg) ->
    Msg;
format_msg(Msg) when is_list(Msg) ->
    unicode:characters_to_binary(Msg);
format_msg(Msg) ->
    unicode:characters_to_binary(io_lib:format("~p", [Msg])).

%% Format timestamp to ISO 8601.
format_timestamp(undefined) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:universal_time(),
    {_, _, Micro} = os:timestamp(),
    Millis = Micro div 1000,
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~3..0BZ",
        [Year, Month, Day, Hour, Minute, Second, Millis]));
format_timestamp(MicroSecs) when is_integer(MicroSecs) ->
    Secs = MicroSecs div 1000000,
    Micro = MicroSecs rem 1000000,
    Millis = Micro div 1000,
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:system_time_to_universal_time(Secs, second),
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~3..0BZ",
        [Year, Month, Day, Hour, Minute, Second, Millis])).

%% Format logger name from metadata.
format_logger_name(Meta) ->
    case maps:get(mfa, Meta, undefined) of
        {Module, _Function, _Arity} ->
            atom_to_binary(Module, utf8);
        undefined ->
            case maps:get(domain, Meta, undefined) of
                undefined -> <<"erlang">>;
                Domain when is_list(Domain) ->
                    list_to_binary(lists:join(".", [atom_to_list(D) || D <- Domain]));
                _ -> <<"erlang">>
            end
    end.

%% Format metadata to Gleam format.
%% Returns a list of {Key, Value} tuples where both are binaries.
%% Filters out internal :logger metadata and birch-specific keys.
format_metadata(Meta) ->
    InternalKeys = [time, mfa, file, line, gl, pid, domain, report_cb,
                    birch_logger_name, birch_metadata, birch_caller_id],
    lists:filtermap(fun({Key, Value}) ->
        case lists:member(Key, InternalKeys) of
            true -> false;
            false ->
                KeyBin = case Key of
                    K when is_atom(K) -> atom_to_binary(K, utf8);
                    K when is_binary(K) -> K;
                    K -> list_to_binary(io_lib:format("~p", [K]))
                end,
                ValueBin = case Value of
                    V when is_binary(V) -> V;
                    V when is_list(V) ->
                        try unicode:characters_to_binary(V)
                        catch _:_ -> list_to_binary(io_lib:format("~p", [V]))
                        end;
                    V when is_atom(V) -> atom_to_binary(V, utf8);
                    V when is_integer(V) -> integer_to_binary(V);
                    V when is_float(V) -> float_to_binary(V);
                    V -> list_to_binary(io_lib:format("~p", [V]))
                end,
                {true, {KeyBin, ValueBin}}
        end
    end, maps:to_list(Meta)).
