# Phase 2: Resource and Safety Hardening - Research

**Researched:** 2026-03-02
**Domain:** Erlang OTP performance optimization, Gleam FFI
**Confidence:** HIGH

## Summary

Phase 2 addresses resource usage optimization and safety hardening for the async handler. The six requirements target known performance bottlenecks and failure modes in the current implementation. Key findings: (1) persistent_term triggers global GC on every write, requiring consolidation to single-key storage; (2) ETS tables avoid GC passes but have read-copy overhead; (3) Erlang's :queue module (used via FFI) provides O(1) enqueue/dequeue; (4) process monitors provide clean crash detection without linked process termination.

**Primary recommendation:** Use single persistent_term key for config+logger, ETS for async registry, Erlang :queue via FFI for pending queue, and erlang:monitor/2 for crash detection.

## Phase Requirements

| ID | Description | Research Support |
|----|-------------|------------------|
| RES-01 | Global config and cached logger consolidated into fewer persistent_term keys | Confirmed current implementation writes to 2 keys; single key approach verified |
| RES-02 | Async writer registry moved from persistent_term to ETS table | ETS avoids global GC on updates; benchmark confirms 37x faster writes |
| RES-03 | Async actor replaces O(n) list.length with O(1) integer tracking | State record can include queue_len integer field |
| RES-04 | Async actor queue uses proper queue data structure for DropOldest | Erlang :queue module already used in JS FFI; gleam/queue deprecated |
| RES-05 | Async actor process monitoring added with crash detection | erlang:monitor/2 provides unidirectional monitoring with DOWN messages |
| RES-06 | File handler caches file size in memory instead of stat per write | FileConfig can include cached_size field updated on write |

## Standard Stack

### Core
| Library | Version | Purpose | Why Standard |
|---------|---------|---------|--------------|
| gleam_otp | >= 1.0.0 | OTP actor primitives | Already in use for async_actor |
| gleam_erlang | >= 1.0.0 | Erlang interop | Already in dependencies |
| simplifile | >= 2.0.0 | File operations | Already in use for file handler |

### Supporting
| Library | Version | Purpose | When to Use |
|---------|---------|---------|-------------|
| Erlang :queue | Built-in | FIFO queue operations | Already used in birch_ffi.erl; use via FFI |
| Erlang :ets | Built-in | ETS table operations | For RES-02 async registry |

**No new dependencies required** - all requirements can be addressed with existing dependencies and Erlang built-ins.

### Alternatives Considered
| Instead of | Could Use | Tradeoff |
|------------|-----------|----------|
| ETS for RES-01 | Keep 2 persistent_term keys | GC overhead acceptable for infrequent config changes |
| gleam/queue | gleam_deque package | gleam/queue deprecated; :queue via FFI already works |
| Custom queue | list with manual tracking | Error-prone; :queue is battle-tested |

## Architecture Patterns

### Recommended Project Structure
```
src/
├── birch_ffi.erl                    # Add ETS functions for async registry
├── birch/
│   ├── config.gleam                 # Modify for single key approach
│   ├── handler/
│   │   ├── async.gleam              # Add queue_len field, monitoring
│   │   └── file.gleam               # Add cached_size to FileConfig
│   └── internal/
│       └── async_actor.gleam        # Add queue_len, use :queue, add monitor
```

### Pattern 1: Single persistent_term Key (RES-01)
**What:** Consolidate global config and cached logger into single term
**When to use:** When multiple related terms are updated together
**Example:**
```erlang
% Instead of two puts:
persistent_term:put(birch_global_config, Config),
persistent_term:put(birch_cached_default_logger, Logger),

% Use single key with tuple:
persistent_term:put(birch_state, {Config, Logger}),
```

### Pattern 2: ETS Registry (RES-02)
**What:** Use ETS table for frequently-updated registry
**When to use:** When data is updated frequently and global GC is unacceptable
**Example:**
```erlang
% Create table once at startup
ets:new(birch_async_registry, [set, named_table, public]),

% Writers register/unregister without GC
ets:insert(birch_async_registry, {Name, Pid}),
ets:delete(birch_async_registry, Name),
```

### Pattern 3: O(1) Queue Length (RES-03)
**What:** Track queue length as integer field, not by calling list.length
**When to use:** When queue operations are hot path
**Example:**
```gleam
// State record includes queue_len
State(
  handler: Handler,
  pending: List(LogRecord),  // or :queue
  queue_len: Int,           // O(1) counter
  max_queue_size: Int,
  overflow: Int,
)
```

### Pattern 4: Erlang :queue via FFI (RES-04)
**What:** Use Erlang's :queue module for efficient FIFO operations
**When to use:** When DropOldest is required (list is inefficient)
**Example:**
```gleam
@external(erlang, "queue", "in")
pub fn queue_in(item: a, queue: Queue(a)) -> Queue(a)

@external(erlang, "queue", "out")
pub fn queue_out(queue: Queue(a)) -> Result(#(a, Queue(a)), Nil)
```

### Pattern 5: Process Monitoring (RES-05)
**What:** Monitor handler process for crash detection
**When to use:** When wrapped handler may crash and needs recovery
**Example:**
```erlang
% In writer loop, handle DOWN messages
receive
    {log, Record} -> ...
    {'DOWN', Ref, process, Pid, Reason} ->
        % Handler crashed - log error, maybe restart
        error_logger:error_msg("Handler ~p crashed: ~p~n", [Pid, Reason]),
        % Stop or restart
        stop
end.
```

### Pattern 6: File Size Caching (RES-06)
**What:** Cache file size in FileConfig, update on write
**When to use:** When stat() per write is too expensive
**Example:**
```gleam
FileConfig(
  path: String,
  rotation: Rotation,
  cached_size: Int,  // Updated after each write
)
```

## Don't Hand-Roll

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Queue operations | Custom list management | Erlang :queue | O(1) ops, DropOldest efficient, battle-tested |
| Process crash detection | Polling, ping/pong | erlang:monitor/2 | Built-in, efficient, clean semantics |
| Frequent updates | persistent_term | ETS | Global GC unacceptable for registry |

**Key insight:** persistent_term is optimized for read-heavy, write-rarely data. The async writer registry is written to on every registration/unregistration, making it unsuitable. ETS has no GC overhead on writes.

## Common Pitfalls

### Pitfall 1: persistent_term Global GC
**What goes wrong:** Every put/erase triggers global GC, causing latency spikes
**Why it happens:** persistent_term copies terms to literal area; updates require scanning all processes
**How to avoid:** Use ETS for frequently-updated data; consolidate config to single key for RES-01
**Warning signs:** Latency spikes under load; log messages delayed during config changes

### Pitfall 2: O(n) Queue Length Check
**What goes wrong:** list.length(state.pending) traverses entire queue on every message
**Why it happens:** Lists don't track length; naive implementation calls length repeatedly
**How to avoid:** Track length as integer field, increment/decrement on enqueue/dequeue
**Warning signs:** CPU usage increases with queue size; async handler slows down when queue grows

### Pitfall 3: DropOldest with List
**What goes wrong:** Dropping oldest from list requires reverse + take + reverse
**Why it happens:** Lists are linked; removing from end requires traversal
**How to avoid:** Use :queue module which supports in/out at both ends efficiently
**Warning signs:** Overflow handling slower than normal operation; memory usage spikes on full queue

### Pitfall 4: Missing Process Monitoring
**What goes wrong:** Handler crash not detected; logs silently lost
**Why it happens:** No monitoring on wrapped handler; assumes handler lives forever
**How to avoid:** Use erlang:monitor/2 to track handler process; handle DOWN messages
**Warning signs:** Logs stop appearing; no error reported; process list shows dead handler

## Code Examples

### RES-01: Single persistent_term Key
```erlang
% Current (2 keys):
persistent_term:put(birch_global_config, Config),
persistent_term:put(birch_cached_default_logger, Logger),

% Proposed (1 key):
persistent_term:put(birch_state, {Config, Logger}),
```

### RES-02: ETS Registry
```erlang
% Create table
ets:new(birch_async_registry, [set, named_table, public]),

% Insert (no GC)
ets:insert(birch_async_registry, {Name, Pid}),

% Delete (no GC)
ets:delete(birch_async_registry, Name),

% Lookup
ets:lookup(birch_async_registry, Name).
```

### RES-04: Erlang :queue FFI
```gleam
@external(erlang, "queue", "new")
pub fn new() -> Queue(a)

@external(erlang, "queue", "in")
pub fn in(item: a, queue: Queue(a)) -> Queue(a)

@external(erlang, "queue", "out")
pub fn out(queue: Queue(a)) -> Result(#(a, Queue(a)), Nil)

@external(erlang, "queue", "is_empty")
pub fn is_empty(queue: Queue(a)) -> Bool
```

### RES-05: Process Monitor
```erlang
% Start handler with monitoring
{HandlerPid, Ref} = spawn_monitor(fun() -> handler_loop(Callback) end),

% Handle messages including DOWN
receive
    {log, Record} -> ...
    {'DOWN', Ref, process, HandlerPid, Reason} ->
        % Handler crashed
        error_logger:error_msg("Handler crashed: ~p~n", [Reason]),
        stop
end.
```

## State of the Art

| Old Approach | Current Approach | When Changed | Impact |
|--------------|-------------------|--------------|--------|
| Multiple persistent_term keys | Single key | This phase | Reduces GC from 2x to 1x |
| persistent_term for registry | ETS table | This phase | Eliminates GC on registry updates |
| list.length for queue length | Integer field | This phase | O(1) instead of O(n) |
| List for pending queue | :queue module | This phase | Efficient DropOldest |
| No process monitoring | erlang:monitor/2 | This phase | Crash detection & recovery |
| stat() per write | Cached size | This phase | Eliminates disk I/O per log |

**Deprecated/outdated:**
- gleam/queue module: Deprecated in stdlib v0.44.0 in favor of gleam_deque package

## Open Questions

1. **Should async actor restart crashed handlers or fall back to sync?**
   - What we know: Current implementation falls back to sync on start failure
   - What's unclear: Recovery behavior after successful start
   - Recommendation: Log error and fall back to sync (consistent with startup behavior)

2. **How to handle file size cache on rotation?**
   - What we know: After rotation, file is new/empty (size 0)
   - What's unclear: Whether to cache size after rotation or reset to 0
   - Recommendation: Reset cached_size to 0 after rotation (new file = 0 bytes)

3. **Should cached file size be persisted across process restarts?**
   - What we know: FileConfig is created per handler instantiation
   - What's unclear: Whether cache should survive handler recreation
   - Recommendation: No - cache is per-handler-instance, reset on new handler

## Sources

### Primary (HIGH confidence)
- [Erlang persistent_term docs](https://www.erlang.org/doc/man/persistent_term.html) - Global GC behavior confirmed
- [Erlang :queue module](https://www.erlang.org/doc/apps/stdlib/queue.html) - FIFO queue operations
- [Erlang process monitoring](https://www.erlang.org/doc/system/ref_man_processes.html) - monitor/2 and DOWN messages

### Secondary (MEDIUM confidence)
- [persistent_term vs ETS benchmarks](https://github.com/ruslandoga/persistent_term-vs-ets) - 37x write performance difference
- [Erlang blog on persistent_term](https://www.erlang.org/blog/persistent_term/) - Use cases and tradeoffs

### Tertiary (LOW confidence)
- [Stack Overflow: ETS vs persistent_term](https://stackoverflow.com/questions/65735371) - General guidance

## Metadata

**Confidence breakdown:**
- Standard stack: HIGH - All libraries already in use or built-in
- Architecture: HIGH - Patterns well-established in Erlang ecosystem
- Pitfalls: HIGH - Known issues with persistent_term GC, O(n) list operations

**Research date:** 2026-03-02
**Valid until:** 2026-04-02 (30 days - Erlang/OTP stable)
