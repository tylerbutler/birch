%% OTP :logger handler that bridges to birch's handler system.
%%
%% This module implements the OTP :logger handler behavior, routing log events
%% to birch handlers. It enables a single dispatch path on BEAM: all logs
%% go through OTP :logger, and this handler delegates to birch handlers.
%%
%% The handler stores the list of birch handlers in its OTP :logger config.
%% When a log event arrives:
%% 1. If it has `birch_log_record` metadata → use the LogRecord directly
%% 2. Otherwise → build a LogRecord from OTP event fields
%% Then dispatch to the stored birch handlers via handler:handle_all/2.

-module(birch_handler_bridge).

-include_lib("birch/include/birch@record_LogRecord.hrl").

%% OTP :logger handler callbacks
-export([log/2, adding_handler/1, removing_handler/1]).

%% API for Gleam FFI
-export([install_bridge/1, remove_bridge/0]).

-define(HANDLER_ID, birch_handler_bridge).

%% ============================================================================
%% OTP :logger handler callbacks
%% ============================================================================

%% Called when the handler is added to :logger.
%% Validates that birch_handlers is present in the config.
adding_handler(#{config := #{birch_handlers := _}} = Config) ->
    {ok, Config};
adding_handler(_Config) ->
    {error, {invalid_config, missing_birch_handlers}}.

%% Called when the handler is removed from :logger.
removing_handler(_Config) ->
    ok.

%% Called by :logger for each log event.
%% Routes the event to birch handlers.
%% Wrapped in try/catch to prevent OTP from removing the handler on crash.
log(#{level := Level, msg := Msg, meta := Meta},
    #{config := #{birch_handlers := Handlers}}) ->
    try
        LogRecord = case maps:get(birch_log_record, Meta, undefined) of
            undefined ->
                %% OTP/library log — build LogRecord from :logger event
                birch_erlang_logger_ffi:build_log_record_from_otp(Level, Msg, Meta);
            Record ->
                %% Birch-originated — use the LogRecord directly
                Record
        end,
        'birch@handler':handle_all(Handlers, LogRecord)
    catch
        _:_ -> ok
    end,
    ok;
log(_Event, _Config) ->
    ok.

%% ============================================================================
%% API
%% ============================================================================

%% Install the bridge handler on OTP :logger with the given birch handlers.
%% Silences the default handler to prevent double output.
-spec install_bridge(list()) -> {ok, nil} | {error, binary()}.
install_bridge(Handlers) ->
    %% Remove existing bridge if present (idempotent)
    logger:remove_handler(?HANDLER_ID),
    %% Silence the default handler to prevent double output
    logger:set_handler_config(default, level, none),
    %% Add the bridge handler
    case logger:add_handler(?HANDLER_ID, ?MODULE, #{
        config => #{birch_handlers => Handlers},
        level => all
    }) of
        ok ->
            {ok, nil};
        {error, Reason} ->
            %% Restore default handler if bridge installation fails
            logger:set_handler_config(default, level, all),
            {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.

%% Remove the bridge handler from OTP :logger and restore the default handler.
-spec remove_bridge() -> {ok, nil} | {error, binary()}.
remove_bridge() ->
    case logger:remove_handler(?HANDLER_ID) of
        ok ->
            %% Restore default handler level
            logger:set_handler_config(default, level, all),
            {ok, nil};
        {error, {not_found, _}} ->
            %% Not installed, nothing to do
            {ok, nil};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.
