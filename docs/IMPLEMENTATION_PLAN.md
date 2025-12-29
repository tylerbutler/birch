# gleam_log Implementation Plan

**Last Updated:** December 2025
**Based on:** PRD v1.0

**See Also:** [GLIMT_PATTERNS.md](./GLIMT_PATTERNS.md) - Patterns from glimt library to consider adopting

---

## Current Status Summary

### Completed (Phase 1 Core + Some Phase 2)

| Feature | Status | Location |
|---------|--------|----------|
| LogLevel type and filtering | ✅ Complete | `src/gleam_log/level.gleam` |
| LogRecord type | ✅ Complete | `src/gleam_log/record.gleam` |
| Logger with context | ✅ Complete | `src/gleam_log/logger.gleam` |
| Console handler (sync, colored) | ✅ Complete | `src/gleam_log/handler/console.gleam` |
| Human-readable formatter | ✅ Complete | `src/gleam_log/formatter.gleam` |
| Global default logger | ✅ Complete | `src/gleam_log.gleam` |
| Cross-platform FFI | ✅ Complete | `src/gleam_log_ffi.{erl,mjs}` |
| JSON formatter | ✅ Complete | `src/gleam_log/handler/json.gleam` |
| File handler | ✅ Complete | `src/gleam_log/handler/file.gleam` |
| Size-based rotation | ✅ Complete | `src/gleam_log/handler/file.gleam` |
| Multiple handlers per logger | ✅ Complete | `src/gleam_log/logger.gleam` |
| Handler min_level filter | ✅ Complete | `src/gleam_log/handler.gleam` |
| Null handler | ✅ Complete | `src/gleam_log/handler.gleam` |
| Lazy evaluation | ✅ Complete | `src/gleam_log/logger.gleam` |
| Global configuration API | ✅ Complete | `src/gleam_log/config.gleam` |
| Runtime level changes | ✅ Complete | `src/gleam_log.gleam` |
| Async console handler | ✅ Complete | `src/gleam_log/handler/async.gleam` |
| Erlang :logger backend | ✅ Complete | `src/gleam_log/erlang_logger.gleam` |

### Remaining Work

| Phase | Feature | Priority |
|-------|---------|----------|
| 3 | Time-based rotation | P2 |
| 3 | Sampling/rate limiting | P2 |
| 3 | Scoped context | P2 |
| 3 | Handler error callbacks | P2 |
| 3 | Compression of rotated files | P2 |

---

## Phase 2: Production Features

### 2.1 Global Configuration API

**Priority:** P1
**Effort:** Medium
**Dependencies:** None

#### Description

Implement a global configuration system that allows users to configure the default logger and application-wide settings via a builder pattern.

#### Requirements (FR-7)

```gleam
log.configure([
  log.level(log.Debug),
  log.handlers([
    handler.console(handler.ConsoleConfig(color: True, target: Stdout)),
    handler.file(handler.FileConfig(
      path: "/var/log/myapp.log",
      rotation: handler.SizeRotation(max_bytes: 10_000_000, keep: 5),
    )),
  ]),
])
```

#### Implementation Steps

1. **Create config module** (`src/gleam_log/config.gleam`)
   - Define `ConfigOption` type for builder pattern
   - Define `GlobalConfig` type to hold runtime configuration
   - Implement option constructors: `level()`, `handlers()`, `context()`

2. **Add platform-specific global state**
   - **Erlang:** Use persistent_term or application environment
   - **JavaScript:** Use module-level variable or globalThis
   - Add FFI functions in `platform.gleam`:
     - `get_global_config() -> GlobalConfig`
     - `set_global_config(GlobalConfig) -> Nil`

3. **Update gleam_log.gleam**
   - Add `configure(List(ConfigOption)) -> Nil` function
   - Modify `default_logger()` to read from global config
   - Add `get_config() -> GlobalConfig` for inspection

4. **Add tests**
   - Configuration application
   - Default fallback behavior
   - Multiple handler configuration

#### Files to Create/Modify

| File | Action |
|------|--------|
| `src/gleam_log/config.gleam` | Create |
| `src/gleam_log/internal/platform.gleam` | Modify - add global state FFI |
| `src/gleam_log_ffi.erl` | Modify - add Erlang global state |
| `src/gleam_log_ffi.mjs` | Modify - add JS global state |
| `src/gleam_log.gleam` | Modify - add configure function |
| `test/gleam_log_test.gleam` | Modify - add config tests |

#### Parallel Subagent Opportunities

- **Agent A:** Implement Erlang FFI for global state
- **Agent B:** Implement JavaScript FFI for global state
- **Agent C:** Implement config.gleam types and builders

---

### 2.2 Runtime Level Changes

**Priority:** P1
**Effort:** Small
**Dependencies:** 2.1 (Global Configuration API)

#### Description

Allow changing the global log level at runtime without restarting the application. This is essential for debugging production issues.

#### Requirements

- Change level without application restart
- Apply to new log calls immediately
- Thread-safe on Erlang (atomic reads/writes)

#### Implementation Steps

1. **Add level change functions**
   - `set_level(Level) -> Nil` - global level
   - `set_logger_level(name: String, level: Level) -> Nil` - named logger

2. **Implement named logger registry** (optional enhancement)
   - Store named loggers in global state
   - Allow retrieving and modifying by name

3. **Add tests**
   - Level change takes effect immediately
   - Concurrent access safety (Erlang)

#### Files to Modify

| File | Action |
|------|--------|
| `src/gleam_log/config.gleam` | Modify - add level setters |
| `src/gleam_log.gleam` | Modify - expose set_level |
| `test/gleam_log_test.gleam` | Modify - add runtime level tests |

---

### 2.3 Async Console Handler

**Priority:** P1
**Effort:** Large
**Dependencies:** None (can be done in parallel with 2.1)

> **Pattern Reference:** See [GLIMT_PATTERNS.md](./GLIMT_PATTERNS.md#1-actor-based-async-instances) for the Actor-based pattern from glimt using gleam_otp Subjects.

#### Description

Provide non-blocking log output to prevent I/O from blocking application logic. Critical for high-throughput applications.

#### Requirements (FR-16)

- On Erlang: Log records sent to handler actor (gen_server/process)
- On JavaScript: Batch writes with microtask/setTimeout scheduling
- Configurable queue size with overflow behavior

#### Design Decision: gleam_otp vs Custom FFI

**Option A: Use gleam_otp Subject (glimt approach)**
- Pros: Battle-tested OTP patterns, natural backpressure, clean API
- Cons: Adds gleam_otp dependency, Erlang-only for full functionality

**Option B: Custom FFI process**
- Pros: No new dependencies, more control
- Cons: Re-implementing OTP patterns, more code to maintain

**Recommendation:** Use gleam_otp for Erlang target with graceful fallback for JavaScript.

#### Implementation Steps

1. **Define async configuration types**

   ```gleam
   pub type AsyncConfig {
     AsyncConfig(
       queue_size: Int,           // Max pending messages
       overflow: OverflowBehavior, // What to do when full
       flush_interval_ms: Int,    // Batch flush interval
     )
   }

   pub type OverflowBehavior {
     DropOldest
     DropNewest
     Block
   }
   ```

2. **Implement Erlang async handler (using gleam_otp Subject)**
   - Use `Subject(LogRecord)` for message passing
   - Actor receives messages and batch writes
   - Handle overflow according to config
   - Provide `flush()` function for graceful shutdown

3. **Implement JavaScript async handler**
   - Use array as queue
   - Schedule flushes with setTimeout/setImmediate
   - Handle overflow

4. **Create async handler wrapper**
   - `async_console(AsyncConfig) -> Handler`
   - `async_file(FileConfig, AsyncConfig) -> Handler`
   - Generic `make_async(Handler, AsyncConfig) -> Handler`

5. **Add flush mechanism**
   - `flush() -> Nil` - wait for all pending logs
   - Critical for clean shutdown

#### Files to Create/Modify

| File | Action |
|------|--------|
| `src/gleam_log/handler/async.gleam` | Create |
| `src/gleam_log/internal/platform.gleam` | Modify - add async FFI |
| `src/gleam_log_ffi.erl` | Modify - add Erlang process/queue |
| `src/gleam_log_ffi.mjs` | Modify - add JS queue/scheduler |
| `test/gleam_log_test.gleam` | Modify - add async tests |

#### Parallel Subagent Opportunities

- **Agent A:** Implement Erlang gen_server-style async handler
- **Agent B:** Implement JavaScript async handler
- **Agent C:** Implement Gleam wrapper and tests

---

## Phase 3: Advanced Features

### 3.1 Time-Based Rotation

**Priority:** P2
**Effort:** Medium
**Dependencies:** None

#### Description

Add time-based log rotation (daily, hourly) as an alternative or complement to size-based rotation.

#### Requirements (FR-15)

| Strategy | Description |
|----------|-------------|
| Hourly | Rotate every hour |
| Daily | Rotate at midnight |
| Combined | Rotate on size OR time, whichever first |

#### Implementation Steps

1. **Extend Rotation type**

   ```gleam
   pub type Rotation {
     NoRotation
     SizeRotation(max_bytes: Int, max_files: Int)
     TimeRotation(interval: TimeInterval, max_files: Int)
     CombinedRotation(max_bytes: Int, interval: TimeInterval, max_files: Int)
   }

   pub type TimeInterval {
     Hourly
     Daily
   }
   ```

2. **Add timestamp tracking**
   - Store last rotation time in file metadata or separate tracking file
   - FFI function to get file modification time

3. **Implement rotation check**
   - Compare current time to last rotation
   - Calculate if interval has passed

4. **Update rotation naming**
   - Include timestamp in rotated file names
   - Example: `app.log.2024-12-26T14` (hourly)

#### Files to Modify

| File | Action |
|------|--------|
| `src/gleam_log/handler/file.gleam` | Modify - add time rotation |
| `src/gleam_log/internal/platform.gleam` | Modify - add time utils |
| `src/gleam_log_ffi.erl` | Modify - add time FFI |
| `src/gleam_log_ffi.mjs` | Modify - add time FFI |
| `test/gleam_log_test.gleam` | Modify - add rotation tests |

---

### 3.2 Sampling and Rate Limiting

**Priority:** P2
**Effort:** Medium
**Dependencies:** 2.1 (Global Configuration)

#### Description

For high-volume scenarios, support probabilistic sampling and rate limiting to prevent log flooding.

#### Requirements (FR-17)

```gleam
log.configure([
  log.sampling(log.SampleConfig(
    level: log.Debug,
    rate: 0.1,  // Log 10% of debug messages
  )),
])
```

#### Implementation Steps

1. **Define sampling configuration**

   ```gleam
   pub type SampleConfig {
     SampleConfig(
       level: Level,      // Apply to this level and below
       rate: Float,       // 0.0 to 1.0 (probability)
     )
   }

   pub type RateLimitConfig {
     RateLimitConfig(
       max_per_second: Int,
       burst_size: Int,    // Token bucket burst
     )
   }
   ```

2. **Implement sampling logic**
   - Use random number generation (platform FFI)
   - Apply before level check for efficiency

3. **Implement rate limiting**
   - Token bucket algorithm
   - Per-logger or global rate limits
   - Track state in global config

4. **Add to configuration API**
   - `log.sampling(SampleConfig)`
   - `log.rate_limit(RateLimitConfig)`

#### Files to Create/Modify

| File | Action |
|------|--------|
| `src/gleam_log/sampling.gleam` | Create |
| `src/gleam_log/config.gleam` | Modify - add sampling options |
| `src/gleam_log/internal/platform.gleam` | Modify - add random FFI |
| `src/gleam_log_ffi.erl` | Modify - add Erlang random |
| `src/gleam_log_ffi.mjs` | Modify - add JS random |
| `test/gleam_log_test.gleam` | Modify - add sampling tests |

---

### 3.3 Scoped Context

**Priority:** P2
**Effort:** Large
**Dependencies:** 2.1 (Global Configuration)

#### Description

Support temporary context for request/operation scope that automatically applies to all logs within the scope.

#### Requirements (FR-10)

```gleam
log.with_scope([#("request_id", id)], fn() {
  // All logs in this block include request_id
  log.info("processing")
  do_work()
  log.info("complete")
})
```

#### Platform Considerations

- **Erlang:** Use process dictionary
- **JavaScript:**
  - Node.js: AsyncLocalStorage
  - Browser/Deno/Bun: Explicit passing only

#### Implementation Steps

1. **Define scope API**

   ```gleam
   pub fn with_scope(context: Metadata, work: fn() -> a) -> a
   pub fn get_scope_context() -> Metadata
   ```

2. **Implement Erlang scoped context**
   - Use `erlang:put/get` for process dictionary
   - Stack-based to allow nested scopes

3. **Implement JavaScript scoped context**
   - Node.js: Use AsyncLocalStorage
   - Fallback: Return empty context (log warning at configure time)

4. **Integrate with logging**
   - Merge scope context in `logger.log()`
   - Priority: call metadata > scope context > logger context

5. **Add detection/configuration**
   - `is_scoped_context_available() -> Bool`
   - Warn user if unavailable on JS platform

#### Files to Create/Modify

| File | Action |
|------|--------|
| `src/gleam_log/scope.gleam` | Create |
| `src/gleam_log/internal/platform.gleam` | Modify - add scope FFI |
| `src/gleam_log_ffi.erl` | Modify - add process dict ops |
| `src/gleam_log_ffi.mjs` | Modify - add AsyncLocalStorage |
| `src/gleam_log/logger.gleam` | Modify - read scope context |
| `test/gleam_log_test.gleam` | Modify - add scope tests |

#### Parallel Subagent Opportunities

- **Agent A:** Implement Erlang scoped context (process dictionary)
- **Agent B:** Implement JavaScript scoped context (AsyncLocalStorage)
- **Agent C:** Implement Gleam API and integration

---

### 3.4 Erlang :logger Backend

**Priority:** P2
**Effort:** Large
**Dependencies:** 2.1 (Global Configuration)

#### Description

Provide optional integration with Erlang's built-in :logger system, allowing gleam_log to receive logs from OTP applications and forward logs to :logger handlers.

#### Requirements

- **Receive from :logger:** gleam_log as a :logger handler
- **Forward to :logger:** gleam_log sending to :logger backend
- Must not break JavaScript target

#### Implementation Steps

1. **Create Erlang-only module**

   ```gleam
   // Only available on Erlang target
   @target(erlang)
   pub fn install_logger_handler() -> Result(Nil, String)

   @target(erlang)
   pub fn forward_to_logger() -> Handler
   ```

2. **Implement :logger handler behavior**
   - Erlang callback module implementing logger handler
   - Convert :logger metadata to gleam_log format
   - Route to gleam_log handlers

3. **Implement :logger backend**
   - Create handler that forwards to :logger
   - Convert gleam_log records to :logger format

4. **Handle JavaScript gracefully**
   - Provide stub functions that return errors
   - Clear documentation about Erlang-only feature

#### Files to Create/Modify

| File | Action |
|------|--------|
| `src/gleam_log/erlang_logger.gleam` | Create (target: erlang) |
| `src/gleam_log_logger_handler.erl` | Create - Erlang handler module |
| `test/erlang_logger_test.gleam` | Create - Erlang-only tests |

---

### 3.5 Handler Error Callbacks

**Priority:** P2
**Effort:** Small
**Dependencies:** None

#### Description

Allow users to be notified when handlers fail, for monitoring and alerting.

#### Requirements

- Handler failures don't crash the application (already implemented)
- User can provide callback for failures
- Callback receives error details

#### Implementation Steps

1. **Define error callback type**

   ```gleam
   pub type ErrorCallback = fn(HandlerError) -> Nil

   pub type HandlerError {
     HandlerError(
       handler_name: String,
       error: String,
       record: LogRecord,
     )
   }
   ```

2. **Add to handler configuration**
   - `handler.with_error_callback(Handler, ErrorCallback) -> Handler`

3. **Update handler.handle()**
   - Catch errors from write function
   - Call error callback if present

4. **Add global error callback option**
   - `log.configure([log.on_error(callback)])`

#### Files to Modify

| File | Action |
|------|--------|
| `src/gleam_log/handler.gleam` | Modify - add error callback |
| `src/gleam_log/config.gleam` | Modify - add global callback |
| `test/gleam_log_test.gleam` | Modify - add error callback tests |

---

### 3.6 Compression of Rotated Files

**Priority:** P2
**Effort:** Medium
**Dependencies:** None

#### Description

Optionally compress rotated log files to save disk space.

#### Requirements

- Compress rotated files with gzip
- Configurable: on/off
- Non-blocking compression

#### Implementation Steps

1. **Extend rotation configuration**

   ```gleam
   pub type Rotation {
     // ... existing variants ...
     SizeRotation(max_bytes: Int, max_files: Int, compress: Bool)
   }
   ```

2. **Implement compression**
   - **Erlang:** Use zlib module
   - **JavaScript:** Use zlib (Node.js) or pako library

3. **Update rotation logic**
   - After rotation, spawn compression task
   - Rename .log.1 to .log.1.gz after compression
   - Handle .gz files in cleanup

4. **Add async compression**
   - Don't block logging during compression
   - Use separate process/worker

#### Files to Modify

| File | Action |
|------|--------|
| `src/gleam_log/handler/file.gleam` | Modify - add compression |
| `src/gleam_log/internal/platform.gleam` | Modify - add compression FFI |
| `src/gleam_log_ffi.erl` | Modify - add zlib usage |
| `src/gleam_log_ffi.mjs` | Modify - add zlib/pako usage |
| `test/gleam_log_test.gleam` | Modify - add compression tests |

---

## Implementation Order & Dependencies

```
Phase 2 (do these first, can be parallelized):
┌─────────────────────┐     ┌─────────────────────┐
│ 2.1 Global Config   │     │ 2.3 Async Handler   │
│     API             │     │                     │
└─────────┬───────────┘     └─────────────────────┘
          │
          ▼
┌─────────────────────┐
│ 2.2 Runtime Level   │
│     Changes         │
└─────────────────────┘

Phase 3 (can be done in any order after 2.1):
┌───────────────┐  ┌───────────────┐  ┌───────────────┐
│ 3.1 Time      │  │ 3.5 Error     │  │ 3.6 Compress  │
│ Rotation      │  │ Callbacks     │  │ Rotated       │
└───────────────┘  └───────────────┘  └───────────────┘

┌───────────────┐  ┌───────────────┐
│ 3.2 Sampling  │  │ 3.3 Scoped    │
│ (needs 2.1)   │  │ Context       │
└───────────────┘  └───────────────┘

┌───────────────┐
│ 3.4 Erlang    │
│ :logger       │
└───────────────┘
```

---

## Recommended Session Workflows

### Session 1: Global Configuration API (2.1)

**Estimated Tasks:** 5-7
**Parallel Opportunities:** FFI for Erlang and JavaScript can be done in parallel

1. Create `src/gleam_log/config.gleam` with types
2. **Parallel:**
   - Add Erlang global state FFI (`gleam_log_ffi.erl`)
   - Add JavaScript global state FFI (`gleam_log_ffi.mjs`)
3. Update `platform.gleam` with FFI declarations
4. Implement `configure()` in `gleam_log.gleam`
5. Add tests
6. Run `just check` to verify both targets

### Session 2: Runtime Level Changes (2.2)

**Estimated Tasks:** 3-4
**Parallel Opportunities:** Limited (depends on 2.1)

1. Add `set_level()` to config module
2. Expose in main API
3. Add tests
4. Verify thread safety on Erlang

### Session 3: Async Handler (2.3)

**Estimated Tasks:** 6-8
**Parallel Opportunities:** Erlang and JavaScript implementations

1. Create `src/gleam_log/handler/async.gleam` types
2. **Parallel:**
   - Implement Erlang gen_server process for async writing
   - Implement JavaScript queue with setTimeout batching
3. Add platform FFI declarations
4. Implement `make_async()` wrapper
5. Add `flush()` function
6. Add tests (may need special handling for async)
7. Run `just check`

### Session 4: Phase 3 Features (pick any)

Each Phase 3 feature can be its own session:

- **3.1 Time Rotation:** ~4 tasks
- **3.2 Sampling:** ~5 tasks (parallel: Erlang/JS random FFI)
- **3.3 Scoped Context:** ~6 tasks (parallel: Erlang/JS implementations)
- **3.4 Erlang :logger:** ~4 tasks (Erlang only)
- **3.5 Error Callbacks:** ~3 tasks
- **3.6 Compression:** ~5 tasks (parallel: Erlang/JS compression FFI)

---

## Testing Strategy

### For Each Feature

1. **Unit tests** - Test individual functions in isolation
2. **Integration tests** - Test feature works end-to-end
3. **Cross-platform tests** - Run on both Erlang and JavaScript
4. **Property tests** - Add to `test/property_test.gleam` where applicable

### Running Tests

```bash
just test          # Both targets
just test-erlang   # Erlang only
just test-js       # JavaScript only
just check         # Format + both targets
```

### Test File Locations

| Feature | Test Location |
|---------|---------------|
| All current features | `test/gleam_log_test.gleam` |
| Property tests | `test/property_test.gleam` |
| Erlang-only features | `test/erlang_logger_test.gleam` (new) |

---

## Notes for AI Agents

### When Working on FFI

1. **Always update both** `gleam_log_ffi.erl` AND `gleam_log_ffi.mjs`
2. Keep behavior identical across platforms where possible
3. Document platform differences in code comments
4. Test on both targets before considering complete

### When Adding New Handlers

1. Follow existing handler pattern in `handler/console.gleam`
2. Use `handler.new()` to create handlers
3. Implement formatter separately if complex
4. Add to handler exports if public

### When Modifying Global State

1. Be careful with concurrent access on Erlang
2. Use `persistent_term` or ETS for Erlang global state
3. Consider using module-level let for JS (or WeakMap for Node.js)
4. Always provide getter functions for testing

### Validation Commands

Always run before completing work:

```bash
just format       # Format code
just check        # Format check + tests on both targets
```

---

## Additional Enhancements (from glimt patterns)

These are lower-priority enhancements inspired by patterns in the glimt library. See [GLIMT_PATTERNS.md](./GLIMT_PATTERNS.md) for detailed analysis.

### E.1 JSON Serializer Builder Pattern

**Priority:** P2
**Effort:** Small
**Dependencies:** None

Add a builder pattern for customizing JSON output format:

```gleam
let custom_handler =
  json.standard_builder()
  |> json.add_custom(fn(_) { [#("service", json.string("my-app"))] })
  |> json.build()
  |> json.handler_with_formatter()
```

**Files:** `src/gleam_log/handler/json.gleam`

### E.2 Error Result Convenience Functions

**Priority:** P3
**Effort:** Small
**Dependencies:** None

Add convenience functions for logging with Result errors:

```gleam
pub fn error_result(logger, message, result: Result(a, e), metadata) -> Nil
pub fn fatal_result(logger, message, result: Result(a, e), metadata) -> Nil
```

**Files:** `src/gleam_log/logger.gleam`, `src/gleam_log.gleam`

### E.3 Time Provider for Testing

**Priority:** P3
**Effort:** Small
**Dependencies:** None

Allow injecting custom timestamp function for deterministic tests:

```gleam
let test_logger =
  logger.new("test")
  |> logger.with_time_provider(fn() { "2024-01-01T00:00:00.000Z" })
```

**Files:** `src/gleam_log/logger.gleam`

### E.4 Process/Caller ID Tracking

**Priority:** P3
**Effort:** Small
**Dependencies:** None

Optionally include caller process ID (Erlang) or thread ID (JS) in log records for debugging concurrent applications.

**Files:** `src/gleam_log/record.gleam`, `src/gleam_log/internal/platform.gleam`, FFI files
