# Phase 3: API Surface and Idiomatic Gleam Audit - Context

**Gathered:** 2026-03-05
**Status:** Ready for planning

<domain>
## Phase Boundary

Make the public API reflect idiomatic Gleam conventions and prepare it for 1.0 stabilization. This covers naming consistency (API-01), use/result.try pipelines (API-02), opaque type boundaries (API-03), builder pattern consistency (API-04), and module-by-module review (API-05). No new features, no deprecated item removal (that's APIV2-01).

</domain>

<decisions>
## Implementation Decisions

### Naming Convention (API-01)
- **Bare getters** across all modules: `name()`, `level()`, `handlers()`, `context()` — not `get_level()`, `get_handlers()`
- Rename existing `get_*` functions to bare form: `get_level` -> `level`, `get_handlers` -> `handlers`, `get_context` -> `context`, `get_error_callback` -> `error_callback`, `get_on_error` -> `on_error`, `get_metadata` -> `metadata`, `get_caller_id` -> `caller_id`
- **Keep `with_` setter/builder pattern** — already consistent and idiomatic Gleam
- **Keep `logger_` prefix** on birch.gleam convenience functions (logger_info, logger_debug, etc.) — necessary to distinguish from module-level default-logger functions (info, debug, etc.)
- Predicates keep existing style: `should_log()`, `should_handle()`, `is_healthy()`, `is_available()`

### Opaque Type Boundaries (API-03)
- **Make LogRecord opaque** — full accessor coverage exists (with_metadata, get_metadata, with_caller_id, get_caller_id, new). Users should not pattern match on fields directly.
- **Make GlobalConfig opaque** — complex type with handlers/callbacks. Already has with_level/get_level. Add any missing accessors.
- **Make SampleConfig opaque** — for consistency. Add level() and rate() accessor functions.
- Types that stay non-opaque: Level (enum, needs pattern matching), MetadataValue (enum, needs pattern matching), OverflowBehavior (enum), Rotation (enum), FileConfig (simple record), ConsoleConfig (simple record)

### Deprecated API Removal (Scope Decision)
- **Do NOT remove deprecated items in this phase** — focus is naming/opaque/idiomatic patterns only
- Deprecated type aliases (LogLevel, LogHandler, LogMetadata, TimestampFormatter, Config) stay
- Deprecated _m suffix functions (trace_m, debug_m, etc.) stay
- Removal deferred to APIV2-01 (future milestone)

### Claude's Discretion
- Where to apply `use`/`result.try` pipelines (API-02) — Claude identifies chained Result-handling patterns and applies idiomatic pipelines
- Builder pattern consistency fixes (API-04) — Claude reviews all builder steps and ensures they follow the same chaining convention
- Module-by-module non-idiomatic pattern identification (API-05) — Claude conducts full review and fixes issues found
- Whether getter renames need deprecated aliases for transition — Claude decides based on pre-1.0 status (likely no, since pre-1.0)

</decisions>

<specifics>
## Specific Ideas

- Bare getter style matches gleam_stdlib patterns (list.length, option.values, etc.)
- The audit should produce a module-by-module checklist showing what was reviewed and what changed (per success criteria #5)
- Pre-1.0 means no backward-compatibility obligation for renames — just rename directly

</specifics>

<code_context>
## Existing Code Insights

### Modules to Audit (public API surface)
- `birch.gleam` — Main entry point, ~250 lines of public API, re-exports from submodules
- `birch/logger.gleam` — Logger type (opaque), builder pattern, log functions
- `birch/handler.gleam` — Handler type (opaque), handler interface
- `birch/handler/console.gleam` — Console handler, config, box drawing utilities
- `birch/handler/file.gleam` — File handler, rotation config
- `birch/handler/json.gleam` — JSON handler, JsonBuilder (opaque)
- `birch/handler/async.gleam` — Async wrapper, AsyncConfig
- `birch/config.gleam` — GlobalConfig, ConfigOption (opaque), SampleConfig
- `birch/level.gleam` — Level type, comparison functions
- `birch/level_formatter.gleam` — LevelFormatter (opaque), formatting options
- `birch/record.gleam` — LogRecord, MetadataValue, Metadata type alias
- `birch/meta.gleam` — Metadata helper constructors
- `birch/formatter.gleam` — Formatter type, format functions
- `birch/sampling.gleam` — SampleConfig, TokenBucket (opaque), rate limiting
- `birch/scope.gleam` — Scoped context functions
- `birch/erlang_logger.gleam` — OTP integration, ErlangLevel type

### Known Naming Inconsistencies
- `handler.name()` (bare) vs `logger.get_level()` (get_ prefix)
- `config.get_level()` vs `config.get_on_error()` (both get_ prefix)
- `record.get_metadata()` vs `record.get_caller_id()` (both get_ prefix)
- `handler.get_error_callback()` (get_ prefix)

### Established Patterns
- Builder: `with_*()` pattern used consistently across Logger, Handler, AsyncConfig, JsonBuilder
- Opaque types: Logger, Handler, ConfigOption, LevelFormatter, JsonBuilder, TokenBucket already opaque
- Non-opaque: LogRecord, GlobalConfig, SampleConfig, Level, MetadataValue — first three will become opaque

### Integration Points
- Renaming getters affects: all test files, all examples (17 examples in examples/), internal usage across modules
- Making types opaque affects: test files that pattern match on LogRecord fields, any code constructing GlobalConfig directly

</code_context>

<deferred>
## Deferred Ideas

- Remove 15+ deprecated functions and type aliases — APIV2-01 (future milestone)
- Cross-platform FFI behavioral parity audit — APIV2-02 (future milestone)

</deferred>

---

*Phase: 03-api-surface-and-idiomatic-gleam-audit*
*Context gathered: 2026-03-05*
