-module(birch_ffi).
-export([is_stdout_tty/0, get_color_depth/0,
         get_global_config/0, set_global_config/1, clear_global_config/0,
         start_async_writer/5, async_send/2, flush_async_writers/0, flush_async_writer/1,
         compress_file_gzip/2, safe_call/1,
         get_scope_context/0, set_scope_context/1, is_scope_context_available/0,
         get_scope_depth/0, set_scope_depth/1,
         get_actor_registry/0, set_actor_registry/1,
         get_caller_id/0]).

%% Note: Time functions are now provided by gleam_time via birch/internal/time.
%% Note: write_stdout, write_stderr, and random_float have been removed.
%% Use gleam/io.println, io.println_error, and gleam/float.random() instead.

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

%% Get terminal color depth (number of colors supported)
%% Returns 16777216 for truecolor, 256 for 256-color, 16 for basic, 0 for none
get_color_depth() ->
    case is_stdout_tty() of
        false -> 0;
        true ->
            ColorTerm = os:getenv("COLORTERM"),
            Term = os:getenv("TERM"),
            case ColorTerm of
                "truecolor" -> 16777216;
                "24bit" -> 16777216;
                _ ->
                    %% Check TERM for 256color
                    case Term of
                        false -> 16;
                        TermStr ->
                            case string:find(TermStr, "256color") of
                                nomatch ->
                                    case string:find(TermStr, "256") of
                                        nomatch -> 16;
                                        _ -> 256
                                    end;
                                _ -> 256
                            end
                    end
            end
    end.

%% ============================================================================
%% Global Configuration Storage
%% ============================================================================

%% Global configuration storage using persistent_term.
%% persistent_term is ideal for read-heavy, write-rarely data like logging config.
%% It's also thread-safe for concurrent access.

-define(GLOBAL_CONFIG_KEY, birch_global_config).

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

%% ============================================================================
%% Async Writer Implementation
%% ============================================================================

%% Registry for async writers (uses persistent_term for fast reads)
-define(ASYNC_REGISTRY_KEY, birch_async_registry).

%% Start an async writer process
%% Overflow: 0=DropOldest, 1=DropNewest, 2=Block
start_async_writer(Name, Callback, QueueSize, _FlushIntervalMs, Overflow) ->
    Self = self(),
    Pid = spawn_link(fun() ->
        %% Register this writer
        register_writer(Name, self()),
        %% Signal that we're ready
        Self ! {async_writer_started, self()},
        %% Enter the writer loop
        async_writer_loop(Callback, QueueSize, Overflow, queue:new(), 0)
    end),
    %% Wait for the writer to be ready
    receive
        {async_writer_started, Pid} -> Pid
    after 5000 ->
        error(async_writer_start_timeout)
    end.

%% Async writer process loop
async_writer_loop(Callback, MaxQueueSize, Overflow, Queue, QueueLen) ->
    receive
        {log, Record} ->
            %% Handle incoming log record
            {NewQueue, NewLen} = enqueue_record(Record, Queue, QueueLen, MaxQueueSize, Overflow),
            %% Process queue if we have records
            {ProcessedQueue, ProcessedLen} = process_queue(Callback, NewQueue, NewLen),
            async_writer_loop(Callback, MaxQueueSize, Overflow, ProcessedQueue, ProcessedLen);

        {flush, From} ->
            %% Flush all pending records
            {FlushedQueue, FlushedLen} = flush_queue(Callback, Queue, QueueLen),
            From ! {flushed, self()},
            async_writer_loop(Callback, MaxQueueSize, Overflow, FlushedQueue, FlushedLen);

        stop ->
            %% Final flush and exit
            _ = flush_queue(Callback, Queue, QueueLen),
            unregister_writer(),
            ok
    end.

%% Enqueue a record with overflow handling
enqueue_record(Record, Queue, QueueLen, MaxQueueSize, Overflow) when QueueLen >= MaxQueueSize ->
    case Overflow of
        0 -> %% DropOldest
            {{value, _}, Q2} = queue:out(Queue),
            {queue:in(Record, Q2), QueueLen};
        1 -> %% DropNewest
            {Queue, QueueLen};
        2 -> %% Block - process queue first, then add
            {Q2, Len2} = flush_queue(fun(_R) -> ok end, Queue, QueueLen),
            {queue:in(Record, Q2), Len2 + 1}
    end;
enqueue_record(Record, Queue, QueueLen, _MaxQueueSize, _Overflow) ->
    {queue:in(Record, Queue), QueueLen + 1}.

%% Process the queue (write records)
process_queue(Callback, Queue, QueueLen) ->
    case queue:out(Queue) of
        {{value, Record}, Q2} ->
            %% Call the Gleam callback
            _ = Callback(Record),
            process_queue(Callback, Q2, QueueLen - 1);
        {empty, _} ->
            {Queue, 0}
    end.

%% Flush all records in the queue
flush_queue(Callback, Queue, _QueueLen) ->
    process_queue(Callback, Queue, queue:len(Queue)).

%% Send a log record to an async writer
async_send(Pid, Record) ->
    Pid ! {log, Record},
    nil.

%% Flush all async writers
flush_async_writers() ->
    Writers = get_all_writers(),
    lists:foreach(fun({_Name, Pid}) ->
        flush_writer(Pid)
    end, Writers),
    nil.

%% Flush a specific async writer by name
flush_async_writer(Name) ->
    case get_writer(Name) of
        {ok, Pid} -> flush_writer(Pid);
        error -> ok
    end,
    nil.

%% Flush a specific writer process
flush_writer(Pid) ->
    Pid ! {flush, self()},
    receive
        {flushed, Pid} -> ok
    after 5000 ->
        ok %% Timeout, don't block forever
    end.

%% Registry functions
register_writer(Name, Pid) ->
    Writers = get_all_writers(),
    NewWriters = lists:keystore(Name, 1, Writers, {Name, Pid}),
    persistent_term:put(?ASYNC_REGISTRY_KEY, NewWriters).

unregister_writer() ->
    case whereis(?MODULE) of
        undefined -> ok;
        _ ->
            Writers = get_all_writers(),
            Self = self(),
            NewWriters = lists:filter(fun({_, Pid}) -> Pid =/= Self end, Writers),
            persistent_term:put(?ASYNC_REGISTRY_KEY, NewWriters)
    end.

get_all_writers() ->
    try
        persistent_term:get(?ASYNC_REGISTRY_KEY)
    catch
        error:badarg -> []
    end.

get_writer(Name) ->
    Writers = get_all_writers(),
    case lists:keyfind(Name, 1, Writers) of
        {Name, Pid} -> {ok, Pid};
        false -> error
    end.

%% ============================================================================
%% File Compression
%% ============================================================================

%% Compress a file using gzip.
%% Reads the source file, compresses it with zlib:gzip, and writes to dest.
compress_file_gzip(SourcePath, DestPath) ->
    try
        %% Read the source file
        case file:read_file(SourcePath) of
            {ok, Data} ->
                %% Compress with gzip
                Compressed = zlib:gzip(Data),
                %% Write to destination
                case file:write_file(DestPath, Compressed) of
                    ok -> {ok, nil};
                    {error, WriteReason} ->
                        {error, iolist_to_binary(io_lib:format("write failed: ~p", [WriteReason]))}
                end;
            {error, ReadReason} ->
                {error, iolist_to_binary(io_lib:format("read failed: ~p", [ReadReason]))}
        end
    catch
        Type:Reason ->
            {error, iolist_to_binary(io_lib:format("compression error: ~p:~p", [Type, Reason]))}
    end.

%% ============================================================================
%% Safe Call (Error Catching)
%% ============================================================================

%% Safely call a function, catching any errors/exceptions.
%% Returns {ok, nil} if successful, {error, ErrorMessage} if failed.
safe_call(Fun) ->
    try
        Fun(),
        {ok, nil}
    catch
        Class:Reason:_Stacktrace ->
            ErrorMsg = format_error(Class, Reason),
            {error, ErrorMsg}
    end.

%% Format an error/exception into a readable string
format_error(error, Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason]));
format_error(throw, Reason) ->
    iolist_to_binary(io_lib:format("throw: ~p", [Reason]));
format_error(exit, Reason) ->
    iolist_to_binary(io_lib:format("exit: ~p", [Reason])).

%% ============================================================================
%% Scoped Context Implementation
%% ============================================================================

%% Key for scope context in process dictionary
-define(SCOPE_CONTEXT_KEY, birch_scope_context).
%% Key for scope depth in process dictionary
-define(SCOPE_DEPTH_KEY, birch_scope_depth).

%% Get the current scope context from the process dictionary.
%% Returns a list of {Key, Value} tuples (Gleam Metadata format).
get_scope_context() ->
    case erlang:get(?SCOPE_CONTEXT_KEY) of
        undefined -> [];
        Context -> Context
    end.

%% Set the scope context in the process dictionary.
set_scope_context(Context) ->
    erlang:put(?SCOPE_CONTEXT_KEY, Context),
    nil.

%% Get the current scope depth (nesting level).
%% Returns 0 if no scope is active.
get_scope_depth() ->
    case erlang:get(?SCOPE_DEPTH_KEY) of
        undefined -> 0;
        Depth -> Depth
    end.

%% Set the scope depth.
set_scope_depth(Depth) ->
    erlang:put(?SCOPE_DEPTH_KEY, Depth),
    nil.

%% Scoped context is always available on Erlang (uses process dictionary).
is_scope_context_available() ->
    true.


%% ============================================================================
%% OTP Actor Registry
%% ============================================================================

%% Key for the actor registry in persistent_term
-define(ACTOR_REGISTRY_KEY, gleam_log_actor_registry).

%% Get the actor registry (returns a Gleam Dict).
%% The registry stores async actor references by name.
get_actor_registry() ->
    try persistent_term:get(?ACTOR_REGISTRY_KEY) of
        Registry -> Registry
    catch
        error:badarg ->
            %% Return empty Gleam Dict
            gleam@dict:new()
    end.

%% Set the actor registry.
set_actor_registry(Registry) ->
    persistent_term:put(?ACTOR_REGISTRY_KEY, Registry),
    nil.

%% ============================================================================
%% Process/Thread ID
%% ============================================================================

%% Get the current process ID as a string.
%% Returns the string representation of self() PID (e.g., "<0.123.0>").
get_caller_id() ->
    list_to_binary(pid_to_list(self())).
