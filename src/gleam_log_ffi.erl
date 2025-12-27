-module(gleam_log_ffi).
-export([timestamp_iso8601/0, write_stdout/1, write_stderr/1, is_stdout_tty/0]).
-export([start_async_writer/5, async_send/2, flush_async_writers/0, flush_async_writer/1]).

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

%% ============================================================================
%% Async Writer Implementation
%% ============================================================================

%% Registry for async writers (uses persistent_term for fast reads)
-define(ASYNC_REGISTRY_KEY, gleam_log_async_registry).

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
            {Q2, Len2} = flush_queue(fun(R) -> ok end, Queue, QueueLen),
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
