%% Erlang FFI for birch's Erlang :logger integration.
%%
%% This module provides:
%% 1. Functions to forward birch records to Erlang's :logger
%% 2. A :logger handler that routes logs to birch handlers
%%
%% The :logger handler behavior requires implementing:
%% - log(LogEvent, Config) -> ok

-module(birch_erlang_logger_ffi).

%% API exports
-export([logger_log/2, logger_log_structured/5,
         install_handler/1, uninstall_handler/1,
         configure_default_handler_formatter/1, is_formatter_configured/0]).

%% :logger handler callbacks
-export([log/2, adding_handler/1, removing_handler/1, changing_config/3]).

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

%% Log structured data to Erlang's :logger, preserving birch metadata.
%% The birch_logger_formatter will detect these fields and reconstruct
%% the LogRecord for formatting.
-spec logger_log_structured(tuple(), binary(), binary(), list(), term()) -> nil.
logger_log_structured(GleamLevel, Message, LoggerName, Metadata, CallerId) ->
    Level = gleam_level_to_atom(GleamLevel),
    logger:log(Level, "~ts", [Message], #{
        birch_logger_name => LoggerName,
        birch_metadata => Metadata,
        birch_caller_id => CallerId
    }),
    nil.

%% ============================================================================
%% Install/Uninstall birch as :logger handler
%% ============================================================================

%% Install birch as a :logger handler.
%% Returns {ok, nil} on success, {error, Reason} on failure.
-spec install_handler(binary()) -> {ok, nil} | {error, binary()}.
install_handler(HandlerId) ->
    Id = binary_to_atom(HandlerId, utf8),
    Config = #{
        config => #{
            %% Store reference to birch config
            birch => true
        },
        level => all,
        formatter => {?MODULE, #{}}
    },
    case logger:add_handler(Id, ?MODULE, Config) of
        ok ->
            {ok, nil};
        {error, {already_exist, _}} ->
            {error, <<"Handler already installed">>};
        {error, Reason} ->
            {error, list_to_binary(io_lib:format("~p", [Reason]))}
    end.

%% Uninstall a :logger handler.
-spec uninstall_handler(binary()) -> {ok, nil} | {error, binary()}.
uninstall_handler(HandlerId) ->
    Id = binary_to_atom(HandlerId, utf8),
    case logger:remove_handler(Id) of
        ok ->
            {ok, nil};
        {error, {not_found, _}} ->
            {error, <<"Handler not found">>};
        {error, Reason} ->
            {error, list_to_binary(io_lib:format("~p", [Reason]))}
    end.

%% ============================================================================
%% :logger handler callbacks
%% ============================================================================

%% Called when a handler is being added.
%% Validates and potentially modifies the configuration.
-spec adding_handler(logger:handler_config()) -> {ok, logger:handler_config()} | {error, term()}.
adding_handler(Config) ->
    {ok, Config}.

%% Called when a handler is being removed.
-spec removing_handler(logger:handler_config()) -> ok.
removing_handler(_Config) ->
    ok.

%% Called when the handler configuration is changed.
-spec changing_config(SetOrUpdate, OldConfig, NewConfig) -> {ok, logger:handler_config()} | {error, term()}
    when SetOrUpdate :: set | update,
         OldConfig :: logger:handler_config(),
         NewConfig :: logger:handler_config().
changing_config(_SetOrUpdate, _OldConfig, NewConfig) ->
    {ok, NewConfig}.

%% Main log callback - receives log events from :logger.
%% Routes them to birch's handlers.
-spec log(logger:log_event(), logger:handler_config()) -> ok.
log(#{level := Level, msg := Msg, meta := Meta}, _Config) ->
    %% Extract message string
    Message = format_msg(Msg),

    %% Extract timestamp
    Timestamp = format_timestamp(maps:get(time, Meta, undefined)),

    %% Extract logger name (use mfa or default)
    LoggerName = format_logger_name(Meta),

    %% Convert level
    GleamLevel = erlang_level_to_gleam(Level),

    %% Convert metadata to Gleam format (list of tuples with binary keys/values)
    Metadata = format_metadata(Meta),

    %% Create LogRecord - must match Gleam's LogRecord type structure
    %% LogRecord(timestamp, level, logger_name, message, metadata, caller_id)
    LogRecord = {log_record, Timestamp, GleamLevel, LoggerName, Message, Metadata, none},

    %% Route to birch handlers via the global config
    route_to_gleam_handlers(LogRecord),

    ok.

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
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~3..0BZ",
        [Year, Month, Day, Hour, Minute, Second, Millis]));
format_timestamp(MicroSecs) when is_integer(MicroSecs) ->
    %% Convert microseconds since epoch to datetime
    Secs = MicroSecs div 1000000,
    Micro = MicroSecs rem 1000000,
    Millis = Micro div 1000,
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:system_time_to_universal_time(Secs, second),
    list_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~3..0BZ",
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
                    list_to_binary(lists:join(".", [atom_to_list(D) || D <- Domain]));
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

%% Route a LogRecord to birch's handlers
%% This reads the global config and sends to all configured handlers
route_to_gleam_handlers(LogRecord) ->
    %% Try to get global config from birch_ffi
    case birch_ffi:get_global_config() of
        {ok, Config} ->
            %% Config is a GlobalConfig record:
            %% {global_config, Level, Handlers, Context, OnError, Sampling}
            {global_config, _Level, Handlers, _Context, _OnError, _Sampling} = Config,
            %% Call each handler with the record
            lists:foreach(fun(Handler) ->
                try
                    %% Handler is a Handler record with a write function
                    %% We need to call handler:handle from Gleam
                    %% For now, use the write function directly if accessible
                    handle_with_handler(Handler, LogRecord)
                catch
                    _:_ -> ok  %% Ignore handler errors
                end
            end, Handlers);
        {error, _} ->
            %% No global config set, use default console output
            format_and_print_record(LogRecord)
    end.

%% Handle a LogRecord with a Handler
%% This is a bit tricky since we need to call Gleam functions
handle_with_handler(Handler, LogRecord) ->
    %% The Handler type is: {handler, Name, MinLevel, Write, Format, ErrorCallback}
    %% But it's opaque, so we need to extract the write function
    case Handler of
        {handler, _Name, MinLevel, Write, _Format, _ErrorCallback} ->
            %% Check min level
            ShouldHandle = case MinLevel of
                {error, nil} -> true;
                {ok, MinLvl} ->
                    %% Compare levels - extract from LogRecord
                    {log_record, _Ts, RecordLevel, _LoggerName, _Msg, _Meta, _CallerId} = LogRecord,
                    compare_levels(RecordLevel, MinLvl)
            end,
            case ShouldHandle of
                true -> Write(LogRecord);
                false -> ok
            end;
        _ ->
            %% Unknown handler format, try calling the write function
            ok
    end.

%% Compare Gleam log levels
compare_levels(RecordLevel, MinLevel) ->
    level_to_int(RecordLevel) >= level_to_int(MinLevel).

%% Gleam Level variants are atoms
level_to_int(trace) -> 0;
level_to_int(debug) -> 1;
level_to_int(info) -> 2;
level_to_int(warn) -> 3;
level_to_int(err) -> 4;
level_to_int(fatal) -> 5;
level_to_int(_) -> 2. %% Default to info

%% Format and print a LogRecord to stdout (fallback)
format_and_print_record({log_record, Timestamp, Level, LoggerName, Message, Metadata, _CallerId}) ->
    LevelStr = case Level of
        trace -> <<"TRACE">>;
        debug -> <<"DEBUG">>;
        info -> <<"INFO">>;
        warn -> <<"WARN">>;
        err -> <<"ERROR">>;
        fatal -> <<"FATAL">>;
        _ -> <<"INFO">>
    end,
    MetaStr = format_metadata_str(Metadata),
    io:format("~ts | ~ts | ~ts | ~ts~ts~n",
              [Timestamp, LevelStr, LoggerName, Message, MetaStr]).

format_metadata_str([]) -> <<>>;
format_metadata_str(Metadata) ->
    Parts = [io_lib:format(" ~ts=~ts", [K, V]) || {K, V} <- Metadata],
    unicode:characters_to_binary([" |" | Parts]).

%% ============================================================================
%% Formatter configuration for :logger's default handler
%% ============================================================================

-define(FORMATTER_CONFIGURED_KEY, birch_formatter_configured).

%% Configure the default :logger handler to use birch_logger_formatter
%% with the given Gleam format function closure.
-spec configure_default_handler_formatter(function()) -> {ok, nil} | {error, binary()}.
configure_default_handler_formatter(FormatFn) ->
    FormatterConfig = #{format_fn => FormatFn},
    case logger:update_handler_config(default, formatter,
            {birch_logger_formatter, FormatterConfig}) of
        ok ->
            persistent_term:put(?FORMATTER_CONFIGURED_KEY, true),
            {ok, nil};
        {error, Reason} ->
            {error, list_to_binary(io_lib:format("~p", [Reason]))}
    end.

%% Check whether the birch formatter has been configured on the default handler.
-spec is_formatter_configured() -> boolean().
is_formatter_configured() ->
    try persistent_term:get(?FORMATTER_CONFIGURED_KEY) of
        true -> true
    catch
        error:badarg -> false
    end.
