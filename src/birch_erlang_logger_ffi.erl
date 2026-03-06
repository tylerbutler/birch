%% Erlang FFI for birch's Erlang :logger integration.
%%
%% This module provides:
%% 1. Direct emission of birch LogRecords to Erlang's :logger
%% 2. A :logger **formatter** that formats log events using birch's
%%    formatting pipeline
%%
%% Architecture:
%% On BEAM, birch sends each LogRecord directly to :logger via emit_to_logger/2.
%% The entire Gleam LogRecord tuple is passed through :logger metadata under
%% the key `birch_log_record`. The formatter detects this key and passes the
%% LogRecord directly to the Gleam format function — no decompose/recompose.
%%
%% OTP/library log events (without birch_log_record) are handled by building
%% a LogRecord from :logger event fields. Structured reports use report_cb
%% when available.
%%
%% The :logger formatter behavior requires implementing:
%% - format(LogEvent, FmtConfig) -> unicode:chardata()

-module(birch_erlang_logger_ffi).

%% API exports
-export([emit_to_logger/2,
         logger_log/2, logger_log_structured/5,
         install_formatter/2, remove_formatter/1,
         is_formatter_configured/0, ensure_initialized/0]).

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
%% This is a 1:1 mapping matching RFC 5424 severity levels.
erlang_level_to_gleam(emergency) -> fatal;
erlang_level_to_gleam(alert) -> alert;
erlang_level_to_gleam(critical) -> critical;
erlang_level_to_gleam(error) -> err;
erlang_level_to_gleam(warning) -> warn;
erlang_level_to_gleam(notice) -> notice;
erlang_level_to_gleam(info) -> info;
erlang_level_to_gleam(debug) -> debug;
erlang_level_to_gleam(_) -> info.

%% ============================================================================
%% Emit LogRecord to :logger (primary API for BEAM integration)
%% ============================================================================

%% Emit a birch LogRecord directly to Erlang's :logger.
%%
%% The entire LogRecord tuple is passed through :logger metadata so the
%% formatter can use it directly without decomposing/recomposing fields.
%% The message is also passed as the :logger message for filter compatibility.
%%
%% LogRecord = {log_record, Timestamp, Level, LoggerName, Message, Metadata, CallerId}
-spec emit_to_logger(tuple(), tuple()) -> nil.
emit_to_logger(GleamLevel, LogRecord) ->
    Level = gleam_level_to_atom(GleamLevel),
    Message = erlang:element(5, LogRecord),
    logger:log(Level, "~ts", [Message], #{
        birch_log_record => LogRecord
    }),
    nil.

%% ============================================================================
%% Initialization
%% ============================================================================

%% Ensure birch formatter is installed on the default :logger handler.
%% Uses persistent_term for fast repeated checks (sub-microsecond).
-spec ensure_initialized() -> boolean().
ensure_initialized() ->
    case persistent_term:get(birch_logger_initialized, false) of
        true -> true;
        false ->
            case is_formatter_configured() of
                true ->
                    persistent_term:put(birch_logger_initialized, true),
                    true;
                false ->
                    false
            end
    end.

%% ============================================================================
%% Legacy: Forward to :logger API
%% ============================================================================

%% Log a pre-formatted message to Erlang's :logger at the specified level.
%% Used by legacy forward_to_beam() handler.
-spec logger_log(tuple(), binary()) -> nil.
logger_log(GleamLevel, Message) ->
    Level = gleam_level_to_atom(GleamLevel),
    logger:log(Level, "~ts", [Message]),
    nil.

%% Log a message with structured birch metadata (legacy API).
-spec logger_log_structured(tuple(), binary(), binary(), list(), tuple()) -> nil.
logger_log_structured(GleamLevel, Message, LoggerName, Metadata, CallerId) ->
    Level = gleam_level_to_atom(GleamLevel),
    logger:log(Level, "~ts", [Message], #{
        birch_logger_name => LoggerName,
        birch_metadata => Metadata,
        birch_caller_id => CallerId
    }),
    nil.

%% ============================================================================
%% Install/Remove birch as :logger Formatter
%% ============================================================================

%% Install birch as the formatter on an existing :logger handler.
%% FormatFn is a Gleam Formatter function: fn(LogRecord) -> String
-spec install_formatter(binary(), function()) -> {ok, nil} | {error, binary()}.
install_formatter(HandlerId, FormatFn) ->
    Result = update_handler_formatter(HandlerId, {?MODULE, #{format_fn => FormatFn}}),
    case Result of
        {ok, nil} ->
            persistent_term:put(birch_logger_initialized, true);
        _ -> ok
    end,
    Result.

%% Remove birch formatter from a :logger handler, restoring OTP's default.
-spec remove_formatter(binary()) -> {ok, nil} | {error, binary()}.
remove_formatter(HandlerId) ->
    persistent_term:put(birch_logger_initialized, false),
    update_handler_formatter(HandlerId, {logger_formatter, #{single_line => true}}).

%% Update the formatter config on a :logger handler.
update_handler_formatter(HandlerId, Formatter) ->
    Id = binary_to_atom(HandlerId, utf8),
    case logger:update_handler_config(Id, formatter, Formatter) of
        ok ->
            {ok, nil};
        {error, {not_found, _}} ->
            {error, <<"Handler not found: ", HandlerId/binary>>};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.

%% Check if the birch formatter is configured on the default :logger handler.
-spec is_formatter_configured() -> boolean().
is_formatter_configured() ->
    case logger:get_handler_config(default) of
        {ok, #{formatter := {birch_erlang_logger_ffi, _}}} -> true;
        _ -> false
    end.

%% ============================================================================
%% :logger Formatter Callback
%% ============================================================================

%% Called by the :logger handler to format a log event.
%%
%% Two paths:
%% 1. Birch-originated: `birch_log_record` in metadata → use it directly
%% 2. OTP/library: Build a LogRecord from :logger event fields
-spec format(logger:log_event(), #{format_fn => function()}) -> unicode:chardata().
format(#{level := Level, msg := Msg, meta := Meta}, #{format_fn := FormatFn}) ->
    LogRecord = case maps:get(birch_log_record, Meta, undefined) of
        undefined ->
            %% OTP/library log — build LogRecord from :logger event
            build_log_record_from_otp(Level, Msg, Meta);
        Record ->
            %% Birch-originated — use the LogRecord directly
            Record
    end,
    Formatted = FormatFn(LogRecord),
    [Formatted, $\n];

%% Fallback: no format_fn in config, use a basic format
format(#{level := Level, msg := Msg, meta := Meta}, _Config) ->
    [format_timestamp(maps:get(time, Meta, undefined)), <<" | ">>,
     level_to_string(Level), <<" | ">>,
     format_logger_name(Meta), <<" | ">>,
     format_msg(Msg, Meta), $\n].

%% Build a birch LogRecord tuple from an OTP :logger event.
%% LogRecord = {log_record, Timestamp, Level, LoggerName, Message, Metadata, CallerId}
build_log_record_from_otp(Level, Msg, Meta) ->
    Timestamp = format_timestamp(maps:get(time, Meta, undefined)),
    GleamLevel = erlang_level_to_gleam(Level),
    LoggerName = format_logger_name(Meta),
    Message = format_msg(Msg, Meta),
    Metadata = format_metadata(Meta),
    {log_record, Timestamp, GleamLevel, LoggerName, Message, Metadata, none}.

%% ============================================================================
%% Helper functions
%% ============================================================================

%% Format the message from various :logger message formats.
%% Accepts the event metadata as a second argument so that `report_cb`
%% can be used for structured OTP reports.
format_msg({report, Report}, Meta) ->
    %% Check for report_cb callback in metadata (OTP structured reports)
    case maps:get(report_cb, Meta, undefined) of
        Fun when is_function(Fun, 1) ->
            %% Single-arg callback: returns chardata directly
            try
                unicode:characters_to_binary(Fun(Report))
            catch
                _:_ ->
                    unicode:characters_to_binary(io_lib:format("~p", [Report]))
            end;
        Fun when is_function(Fun, 2) ->
            %% Two-arg callback: returns {Format, Args}
            try
                {Format, Args} = Fun(Report, #{single_line => true, depth => 30}),
                unicode:characters_to_binary(io_lib:format(Format, Args))
            catch
                _:_ ->
                    unicode:characters_to_binary(io_lib:format("~p", [Report]))
            end;
        _ ->
            unicode:characters_to_binary(io_lib:format("~p", [Report]))
    end;
format_msg({string, String}, _Meta) when is_list(String) ->
    unicode:characters_to_binary(String);
format_msg({string, String}, _Meta) when is_binary(String) ->
    String;
format_msg({Format, Args}, _Meta) when is_list(Format) ->
    unicode:characters_to_binary(io_lib:format(Format, Args));
format_msg(Msg, _Meta) when is_binary(Msg) ->
    Msg;
format_msg(Msg, _Meta) when is_list(Msg) ->
    unicode:characters_to_binary(Msg);
format_msg(Msg, _Meta) ->
    unicode:characters_to_binary(io_lib:format("~p", [Msg])).

%% Format timestamp to ISO 8601
format_timestamp(undefined) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:universal_time(),
    {_, _, Micro} = os:timestamp(),
    Millis = Micro div 1000,
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~3..0BZ",
        [Year, Month, Day, Hour, Minute, Second, Millis]));
format_timestamp(MicroSecs) when is_integer(MicroSecs) ->
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

%% Format metadata to Gleam format.
%% Returns a list of {Key, MetadataValue} tuples where Key is a binary and
%% MetadataValue is a Gleam MetadataValue tagged union ({string_val, Binary},
%% {int_val, Integer}, {float_val, Float}, {bool_val, Boolean}).
%%
%% Filters out internal :logger metadata keys and birch-specific keys.
format_metadata(Meta) ->
    InternalKeys = [time, mfa, file, line, gl, pid, domain, report_cb,
                    birch_log_record, birch_logger_name,
                    birch_metadata, birch_caller_id],
    lists:filtermap(fun({Key, Value}) ->
        case lists:member(Key, InternalKeys) of
            true -> false;
            false ->
                {true, {to_binary_key(Key), to_metadata_value(Value)}}
        end
    end, maps:to_list(Meta)).

to_binary_key(K) when is_atom(K) -> atom_to_binary(K, utf8);
to_binary_key(K) when is_binary(K) -> K;
to_binary_key(K) -> iolist_to_binary(io_lib:format("~p", [K])).

%% Convert an Erlang value to a Gleam MetadataValue tagged union.
%% Gleam compiles StringVal(v) to {string_val, v}, IntVal(v) to {int_val, v}, etc.
%% Note: is_boolean/1 guard must come before is_atom/1 since booleans are atoms in Erlang.
to_metadata_value(V) when is_binary(V) -> {string_val, V};
to_metadata_value(V) when is_boolean(V) -> {bool_val, V};
to_metadata_value(V) when is_integer(V) -> {int_val, V};
to_metadata_value(V) when is_float(V) -> {float_val, V};
to_metadata_value(V) when is_list(V) ->
    BinVal = try unicode:characters_to_binary(V)
             catch _:_ -> iolist_to_binary(io_lib:format("~p", [V]))
             end,
    {string_val, BinVal};
to_metadata_value(V) when is_atom(V) -> {string_val, atom_to_binary(V, utf8)};
to_metadata_value(V) -> {string_val, iolist_to_binary(io_lib:format("~p", [V]))}.

%% Convert Erlang level atom to display string (fallback formatter only)
level_to_string(emergency) -> <<"FATAL   ">>;
level_to_string(alert) -> <<"ALERT   ">>;
level_to_string(critical) -> <<"CRITICAL">>;
level_to_string(error) -> <<"ERROR   ">>;
level_to_string(warning) -> <<"WARN    ">>;
level_to_string(notice) -> <<"NOTICE  ">>;
level_to_string(info) -> <<"INFO    ">>;
level_to_string(debug) -> <<"DEBUG   ">>;
level_to_string(_) -> <<"INFO    ">>.
