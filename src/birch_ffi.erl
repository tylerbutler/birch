-module(birch_ffi).
-export([is_stdout_tty/0, get_color_depth/0,
         get_global_config/0, set_global_config/1, clear_global_config/0,
         get_cached_default_logger/0, set_cached_default_logger/1, clear_cached_default_logger/0,
         get_scoped_logger/0, set_scoped_logger/1, clear_scoped_logger/0,
         start_async_writer/5, async_send/2, flush_async_writers/0, flush_async_writer/1,
         compress_file_gzip/2, safe_call/1,
         get_scope_context/0, set_scope_context/1, is_scope_context_available/0,
         get_scope_depth/0, set_scope_depth/1,
         run_with_scope_cleanup/5, run_with_scoped_logger_cleanup/2,
         get_actor_registry/0, set_actor_registry/1,
         get_caller_id/0,
         get_file_size_cache/1, set_file_size_cache/2, reset_file_size_cache/1]).

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
%% Global State Storage (Config + Cached Logger)
%% ============================================================================

%% Global state storage using persistent_term.
%% Uses a single key for both config and default logger to reduce GC passes.
%% persistent_term is ideal for read-heavy, write-rarely data like logging config.
%% It's also thread-safe for concurrent access.

-define(STATE_KEY, birch_state).

%% Get the global configuration. Returns {ok, Config} or {error, nil}.
get_global_config() ->
    try persistent_term:get(?STATE_KEY) of
        {nil, _Logger} -> {error, nil};
        {Config, _Logger} -> {ok, Config}
    catch
        error:badarg -> {error, nil}
    end.

%% Set the global configuration (preserves existing logger if set).
set_global_config(Config) ->
    %% Get existing logger or use default
    Logger = case get_cached_default_logger() of
        {ok, L} -> L;
        {error, nil} -> nil
    end,
    persistent_term:put(?STATE_KEY, {Config, Logger}),
    nil.

%% Clear the global configuration only (preserves cached logger).
%% Previously this erased the entire persistent_term key, which also
%% removed the cached logger. Now it only clears config, matching JS behavior.
clear_global_config() ->
    Logger = case get_cached_default_logger() of
        {ok, L} -> L;
        {error, nil} -> nil
    end,
    persistent_term:put(?STATE_KEY, {nil, Logger}),
    nil.

%% ============================================================================
%% Cached Default Logger (part of unified state)
%% ============================================================================

%% Get the cached default logger. Returns {ok, Logger} or {error, nil}.
get_cached_default_logger() ->
    try persistent_term:get(?STATE_KEY) of
        {_Config, nil} -> {error, nil};
        {_Config, Logger} -> {ok, Logger}
    catch
        error:badarg -> {error, nil}
    end.

%% Set the cached default logger (preserves existing config if set).
set_cached_default_logger(Logger) ->
    %% Get existing config or use default
    Config = case get_global_config() of
        {ok, C} -> C;
        {error, nil} -> nil
    end,
    persistent_term:put(?STATE_KEY, {Config, Logger}),
    nil.

%% Clear the cached default logger (reset config only).
clear_cached_default_logger() ->
    %% Get existing config, clear logger
    Config = case get_global_config() of
        {ok, C} -> C;
        {error, nil} -> nil
    end,
    persistent_term:put(?STATE_KEY, {Config, nil}),
    nil.

%% ============================================================================
%% Async Writer Implementation
%% ============================================================================

%% Note: On Erlang, async logging uses OTP actors (see birch/handler/async.gleam
%% and birch/internal/async_actor.gleam). These FFI functions exist because
%% platform.gleam declares them for both targets, but only the JavaScript target
%% calls them at runtime.

%% Registry for async writers (uses ETS table for lock-free updates)
-define(ASYNC_REGISTRY_KEY, birch_async_registry).

%% Ensure the async registry ETS table exists
ensure_async_registry() ->
    case ets:info(?ASYNC_REGISTRY_KEY, name) of
        undefined ->

            ets:new(?ASYNC_REGISTRY_KEY, [set, named_table, public]);
        _ ->
            ok
    end.

%% Start an async writer process.
%% Overflow: 0=DropOldest, 1=DropNewest, 2=Block
%%
%% Note: _FlushIntervalMs is intentionally unused on Erlang. The Erlang process
%% model handles messages sequentially from the mailbox, so records are processed
%% immediately upon receipt rather than batched on a timer. The JavaScript
%% implementation uses this parameter for setTimeout-based batching.
start_async_writer(Name, Callback, QueueSize, _FlushIntervalMs, Overflow) ->
    Self = self(),
    Pid = spawn_link(fun() ->
        register_writer(Name, self()),
        Self ! {async_writer_started, self()},
        async_writer_loop(Name, Callback, QueueSize, Overflow, queue:new(), 0)
    end),
    receive
        {async_writer_started, Pid} -> Pid
    after 5000 ->
        error(async_writer_start_timeout)
    end.

%% Async writer process loop.
%% Name is threaded through the loop for correct unregistration on stop.
async_writer_loop(Name, Callback, MaxQueueSize, Overflow, Queue, QueueLen) ->
    receive
        {log, Record} ->
            {NewQueue, NewLen} = enqueue_record(Callback, Record, Queue, QueueLen, MaxQueueSize, Overflow),
            {ProcessedQueue, ProcessedLen} = process_queue(Callback, NewQueue, NewLen),
            async_writer_loop(Name, Callback, MaxQueueSize, Overflow, ProcessedQueue, ProcessedLen);

        {flush, From} ->
            {FlushedQueue, FlushedLen} = flush_queue(Callback, Queue, QueueLen),
            From ! {flushed, self()},
            async_writer_loop(Name, Callback, MaxQueueSize, Overflow, FlushedQueue, FlushedLen);

        stop ->
            _ = flush_queue(Callback, Queue, QueueLen),
            unregister_writer(Name),
            ok
    end.

%% Enqueue a record with overflow handling
enqueue_record(Callback, Record, Queue, QueueLen, MaxQueueSize, Overflow) when QueueLen >= MaxQueueSize ->
    case Overflow of
        0 -> %% DropOldest
            {{value, _}, Q2} = queue:out(Queue),
            {queue:in(Record, Q2), QueueLen};
        1 -> %% DropNewest
            {Queue, QueueLen};
        2 -> %% Block - process queue first, then add
            {Q2, Len2} = flush_queue(Callback, Queue, QueueLen),
            {queue:in(Record, Q2), Len2 + 1}
    end;
enqueue_record(_Callback, Record, Queue, QueueLen, _MaxQueueSize, _Overflow) ->
    {queue:in(Record, Queue), QueueLen + 1}.

%% Process the queue (write records)
process_queue(Callback, Queue, QueueLen) ->
    case queue:out(Queue) of
        {{value, Record}, Q2} ->
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

%% Registry functions (using ETS table)
register_writer(Name, Pid) ->
    ensure_async_registry(),
    ets:insert(?ASYNC_REGISTRY_KEY, {Name, Pid}),
    ok.

%% Unregister a writer by name (the ETS key).
unregister_writer(Name) ->
    ensure_async_registry(),
    ets:delete(?ASYNC_REGISTRY_KEY, Name),
    ok.

get_all_writers() ->
    ensure_async_registry(),
    ets:tab2list(?ASYNC_REGISTRY_KEY).

get_writer(Name) ->
    ensure_async_registry(),
    case ets:lookup(?ASYNC_REGISTRY_KEY, Name) of
        [{Name, Pid}] -> {ok, Pid};
        [] -> error
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

%% Run a function with scope context, guaranteeing cleanup via try/after.
%% This ensures that if Work() raises an exception, the previous scope context
%% and depth are always restored in the process dictionary.
run_with_scope_cleanup(MergedContext, NewDepth, OldContext, OldDepth, Work) ->
    set_scope_context(MergedContext),
    set_scope_depth(NewDepth),
    try Work() of
        Result -> Result
    after
        set_scope_context(OldContext),
        set_scope_depth(OldDepth)
    end.

%% ============================================================================
%% Scoped Logger Override
%% ============================================================================

-define(SCOPED_LOGGER_KEY, birch_scoped_logger).

%% Get the scoped logger override from the process dictionary.
%% Returns {ok, Logger} or {error, nil}.
get_scoped_logger() ->
    case erlang:get(?SCOPED_LOGGER_KEY) of
        undefined -> {error, nil};
        Logger -> {ok, Logger}
    end.

%% Set the scoped logger override in the process dictionary.
set_scoped_logger(Logger) ->
    erlang:put(?SCOPED_LOGGER_KEY, Logger),
    nil.

%% Clear the scoped logger override from the process dictionary.
clear_scoped_logger() ->
    erlang:erase(?SCOPED_LOGGER_KEY),
    nil.

%% Run a function with a scoped logger override, guaranteeing cleanup via try/after.
%% This ensures that if Work() raises an exception, the previous scoped logger
%% state is always restored in the process dictionary.
run_with_scoped_logger_cleanup(Logger, Work) ->
    Previous = get_scoped_logger(),
    set_scoped_logger(Logger),
    try Work() of
        Result -> Result
    after
        case Previous of
            {ok, Prev} -> set_scoped_logger(Prev);
            {error, _} -> clear_scoped_logger()
        end
    end.

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

%% ============================================================================
%% File Size Cache (per-process, using process dictionary)
%% ============================================================================

%% Key for file size cache in process dictionary
-define(FILE_SIZE_CACHE_KEY, birch_file_size_cache).

%% Get the cached file size for a path.
%% Returns {ok, Size} if cached, {error, nil} if not cached.
get_file_size_cache(Path) ->
    case erlang:get(?FILE_SIZE_CACHE_KEY) of
        undefined ->
            {error, nil};
        Cache ->
            case maps:find(Path, Cache) of
                {ok, Size} -> {ok, Size};
                error -> {error, nil}
            end
    end.

%% Set the cached file size for a path.
set_file_size_cache(Path, Size) ->
    Cache = case erlang:get(?FILE_SIZE_CACHE_KEY) of
        undefined -> #{};
        C -> C
    end,
    NewCache = maps:put(Path, Size, Cache),
    erlang:put(?FILE_SIZE_CACHE_KEY, NewCache),
    nil.

%% Reset (delete) the cached file size for a path.
%% Called after file rotation to start fresh.
reset_file_size_cache(Path) ->
    case erlang:get(?FILE_SIZE_CACHE_KEY) of
        undefined ->
            nil;
        Cache ->
            NewCache = maps:remove(Path, Cache),
            erlang:put(?FILE_SIZE_CACHE_KEY, NewCache),
            nil
    end.
