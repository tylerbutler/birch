%% Test FFI helpers for birch :logger integration tests.
%%
%% Provides ETS-based capture buffers for intercepting formatted output
%% and helpers for sending OTP :logger events directly.

-module(birch_logger_test_ffi).
-export([new_capture_buffer/0, append_to_buffer/2, get_buffer_contents/1,
         sleep/1, otp_logger_warning/1, otp_logger_report_with_cb/0]).

%% Create a new ETS-based capture buffer.
%% Returns an ETS table reference (unnamed, public for cross-process access).
new_capture_buffer() ->
    ets:new(capture_buffer, [set, public]).

%% Append a string to the capture buffer.
append_to_buffer(Buffer, Value) ->
    Key = erlang:unique_integer([monotonic]),
    ets:insert(Buffer, {Key, Value}),
    nil.

%% Get the accumulated contents of the capture buffer as a single string.
get_buffer_contents(Buffer) ->
    Entries = lists:sort(ets:tab2list(Buffer)),
    Values = [V || {_K, V} <- Entries],
    iolist_to_binary(lists:join(<<"\n">>, Values)).

%% Sleep for the given number of milliseconds.
sleep(Ms) ->
    timer:sleep(Ms),
    nil.

%% Send a plain OTP logger:warning message (not from birch).
%% Uses warning level which is above the default :logger threshold (notice).
otp_logger_warning(Message) ->
    logger:warning("~ts", [Message]),
    nil.

%% Send a structured OTP report with a report_cb callback.
%% This simulates OTP-style structured reports (e.g., gen_server crashes).
%% Uses warning level to be above the default :logger threshold.
otp_logger_report_with_cb() ->
    Report = #{message => <<"hello">>, type => test},
    ReportCb = fun(R) ->
        Msg = maps:get(message, R, <<"unknown">>),
        io_lib:format("Test report: ~ts", [Msg])
    end,
    logger:warning(Report, #{report_cb => ReportCb}),
    nil.
