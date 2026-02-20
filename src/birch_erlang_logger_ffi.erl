%% Erlang FFI for birch's Erlang :logger integration.
%%
%% This module provides:
%% 1. Functions to forward birch log messages to Erlang's :logger
%% 2. A :logger **formatter** that formats log events using birch's
%%    formatting pipeline via a Gleam callback function
%%
%% The :logger formatter behavior requires implementing:
%% - format(LogEvent, FmtConfig) -> unicode:chardata()
%%
%% This is the idiomatic OTP approach: the :logger handler controls output
%% (console, file, etc.) while birch controls formatting.

-module(birch_erlang_logger_ffi).

%% API exports
-export([logger_log/2, install_formatter/2, remove_formatter/1]).

%% :logger formatter callback
-export([format/2]).

%% ============================================================================
%% Gleam ErlangLevel type mapping
%% ============================================================================

%% Convert Gleam ErlangLevel type to Erlang atom.
%% Gleam compiles simple custom type variants (no fields) to just atoms.
gleam_level_to_atom(erlang_emergency) -> emergency;
gleam_level_to_atom(erlang_alert) -> alert;
gleam_level_to_atom(erlang_critical) -> critical;
gleam_level_to_atom(erlang_error) -> error;
gleam_level_to_atom(erlang_warning) -> warning;
gleam_level_to_atom(erlang_notice) -> notice;
gleam_level_to_atom(erlang_info) -> info;
gleam_level_to_atom(erlang_debug) -> debug.

%% Convert Erlang log level atom to Gleam Level type.
%% Gleam Level variants are also just atoms.
erlang_level_to_gleam(emergency) -> fatal;
erlang_level_to_gleam(alert) -> fatal;
erlang_level_to_gleam(critical) -> fatal;
erlang_level_to_gleam(error) -> err;
erlang_level_to_gleam(warning) -> warn;
erlang_level_to_gleam(notice) -> info;
erlang_level_to_gleam(info) -> info;
erlang_level_to_gleam(debug) -> debug;
erlang_level_to_gleam(_) -> info.

%% ============================================================================
%% Forward to :logger API
%% ============================================================================

%% Log a message to Erlang's :logger at the specified level.
%% Called from Gleam's erlang_logger module.
-spec logger_log(tuple(), binary()) -> nil.
logger_log(GleamLevel, Message) ->
    Level = gleam_level_to_atom(GleamLevel),
    %% Use logger:log/2 with the level and message
    %% The message is already formatted by Gleam
    logger:log(Level, "~ts", [Message]),
    nil.

%% ============================================================================
%% Install/Remove birch as :logger Formatter
%% ============================================================================

%% Install birch as the formatter on an existing :logger handler.
%% FormatFn is a Gleam function: fn(String, Level, String, String, Metadata) -> String
%% Returns {ok, nil} on success, {error, Reason} on failure.
-spec install_formatter(binary(), function()) -> {ok, nil} | {error, binary()}.
install_formatter(HandlerId, FormatFn) ->
    Id = binary_to_atom(HandlerId, utf8),
    FmtConfig = #{format_fn => FormatFn},
    case logger:update_handler_config(Id, formatter, {?MODULE, FmtConfig}) of
        ok ->
            {ok, nil};
        {error, {not_found, _}} ->
            {error, <<"Handler not found: ", HandlerId/binary>>};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.

%% Remove birch formatter from a :logger handler, restoring OTP's default.
-spec remove_formatter(binary()) -> {ok, nil} | {error, binary()}.
remove_formatter(HandlerId) ->
    Id = binary_to_atom(HandlerId, utf8),
    %% Restore the default OTP formatter (logger_formatter with single_line)
    DefaultFmt = {logger_formatter, #{single_line => true}},
    case logger:update_handler_config(Id, formatter, DefaultFmt) of
        ok ->
            {ok, nil};
        {error, {not_found, _}} ->
            {error, <<"Handler not found: ", HandlerId/binary>>};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.

%% ============================================================================
%% :logger Formatter Callback
%% ============================================================================

%% Called by the :logger handler to format a log event.
%% This is the core of the inverted integration: instead of birch acting as
%% a handler that receives events and routes them, birch acts as a formatter
%% that the existing handler calls to produce output.
%%
%% The format_fn stored in Config is a Gleam function that takes:
%%   (Timestamp, Level, LoggerName, Message, Metadata) -> String
-spec format(logger:log_event(), #{format_fn => function()}) -> unicode:chardata().
format(#{level := Level, msg := Msg, meta := Meta}, #{format_fn := FormatFn}) ->
    Message = format_msg(Msg),
    Timestamp = format_timestamp(maps:get(time, Meta, undefined)),
    LoggerName = format_logger_name(Meta),
    GleamLevel = erlang_level_to_gleam(Level),
    Metadata = format_metadata(Meta),
    %% Call the Gleam format callback
    Formatted = FormatFn(Timestamp, GleamLevel, LoggerName, Message, Metadata),
    [Formatted, $\n];

%% Fallback: no format_fn in config, use a basic format
format(#{level := Level, msg := Msg, meta := Meta}, _Config) ->
    Message = format_msg(Msg),
    Timestamp = format_timestamp(maps:get(time, Meta, undefined)),
    LoggerName = format_logger_name(Meta),
    LevelStr = level_to_string(Level),
    [Timestamp, <<" | ">>, LevelStr, <<" | ">>, LoggerName, <<" | ">>, Message, $\n].

%% ============================================================================
%% Helper functions
%% ============================================================================

%% Format the message from various :logger message formats
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

%% Format timestamp to ISO 8601
format_timestamp(undefined) ->
    %% Get current time if not provided
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:universal_time(),
    {_, _, Micro} = os:timestamp(),
    Millis = Micro div 1000,
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~3..0BZ",
        [Year, Month, Day, Hour, Minute, Second, Millis]));
format_timestamp(MicroSecs) when is_integer(MicroSecs) ->
    %% Convert microseconds since epoch to datetime
    Secs = MicroSecs div 1000000,
    Micro = MicroSecs rem 1000000,
    Millis = Micro div 1000,
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:system_time_to_universal_time(Secs, second),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~3..0BZ",
        [Year, Month, Day, Hour, Minute, Second, Millis])).

%% Format logger name from metadata
format_logger_name(Meta) ->
    case maps:get(mfa, Meta, undefined) of
        {Module, _Function, _Arity} ->
            atom_to_binary(Module, utf8);
        undefined ->
            case maps:get(domain, Meta, undefined) of
                undefined -> <<"erlang">>;
                Domain when is_list(Domain) ->
                    iolist_to_binary(lists:join(".", [atom_to_list(D) || D <- Domain]));
                _ -> <<"erlang">>
            end
    end.

%% Format metadata to Gleam format
%% Returns a list of {Key, Value} tuples where both are binaries
format_metadata(Meta) ->
    %% Filter out internal :logger metadata and convert to Gleam format
    InternalKeys = [time, mfa, file, line, gl, pid, domain, report_cb],
    lists:filtermap(fun({Key, Value}) ->
        case lists:member(Key, InternalKeys) of
            true -> false;
            false ->
                KeyBin = case Key of
                    K when is_atom(K) -> atom_to_binary(K, utf8);
                    K when is_binary(K) -> K;
                    K -> iolist_to_binary(io_lib:format("~p", [K]))
                end,
                ValueBin = case Value of
                    V when is_binary(V) -> V;
                    V when is_list(V) ->
                        try unicode:characters_to_binary(V)
                        catch _:_ -> iolist_to_binary(io_lib:format("~p", [V]))
                        end;
                    V when is_atom(V) -> atom_to_binary(V, utf8);
                    V when is_integer(V) -> integer_to_binary(V);
                    V when is_float(V) -> float_to_binary(V);
                    V -> iolist_to_binary(io_lib:format("~p", [V]))
                end,
                {true, {KeyBin, ValueBin}}
        end
    end, maps:to_list(Meta)).

%% Convert Erlang level atom to display string (fallback formatter only)
level_to_string(emergency) -> <<"FATAL">>;
level_to_string(alert) -> <<"FATAL">>;
level_to_string(critical) -> <<"FATAL">>;
level_to_string(error) -> <<"ERROR">>;
level_to_string(warning) -> <<"WARN ">>;
level_to_string(notice) -> <<"INFO ">>;
level_to_string(info) -> <<"INFO ">>;
level_to_string(debug) -> <<"DEBUG">>;
level_to_string(_) -> <<"INFO ">>.
