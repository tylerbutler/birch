# Phase 3: API Surface and Idiomatic Gleam Audit - Research

**Researched:** 2026-03-05
**Domain:** Gleam API design, naming conventions, opaque type patterns
**Confidence:** HIGH

## Summary

Phase 3 is a refactoring pass to make birch's public API idiomatic Gleam before 1.0 stabilization. The work is entirely within the existing codebase -- no new dependencies, no architectural changes. The three main workstreams are: (1) rename `get_*` accessors to bare getters, (2) make LogRecord/GlobalConfig/SampleConfig opaque with accessor functions, and (3) audit all modules for non-idiomatic patterns including `use`/`result.try` opportunities.

The biggest risk is the opaque type transitions for LogRecord and GlobalConfig. LogRecord fields are accessed directly in 6+ modules (formatter.gleam, handler.gleam, handler/console.gleam, handler/json.gleam, erlang_logger.gleam, and within record.gleam itself). GlobalConfig fields are accessed directly in birch.gleam (build_default_logger, default_config, should_sample). These all need accessor functions added before making the types opaque.

**Primary recommendation:** Split work into three sequential plans: (1) getter renames + test updates, (2) opaque type transitions with accessor functions, (3) use/result.try + builder consistency + module-by-module audit.

<user_constraints>
## User Constraints (from CONTEXT.md)

### Locked Decisions
- **Bare getters** across all modules: `name()`, `level()`, `handlers()`, `context()` -- not `get_level()`, `get_handlers()`
- Rename existing `get_*` functions to bare form: `get_level` -> `level`, `get_handlers` -> `handlers`, `get_context` -> `context`, `get_error_callback` -> `error_callback`, `get_on_error` -> `on_error`, `get_metadata` -> `metadata`, `get_caller_id` -> `caller_id`
- **Keep `with_` setter/builder pattern** -- already consistent and idiomatic Gleam
- **Keep `logger_` prefix** on birch.gleam convenience functions
- Predicates keep existing style: `should_log()`, `should_handle()`, `is_healthy()`, `is_available()`
- **Make LogRecord opaque** with full accessor coverage
- **Make GlobalConfig opaque** with accessors; add any missing accessors
- **Make SampleConfig opaque** with `level()` and `rate()` accessor functions
- Types that stay non-opaque: Level, MetadataValue, OverflowBehavior, Rotation, FileConfig, ConsoleConfig
- **Do NOT remove deprecated items** -- focus is naming/opaque/idiomatic patterns only
- Pre-1.0 means no backward-compatibility obligation for renames -- just rename directly

### Claude's Discretion
- Where to apply `use`/`result.try` pipelines (API-02) -- identify chained Result-handling patterns
- Builder pattern consistency fixes (API-04) -- review all builder steps
- Module-by-module non-idiomatic pattern identification (API-05) -- full review
- Whether getter renames need deprecated aliases for transition -- likely no since pre-1.0

### Deferred Ideas (OUT OF SCOPE)
- Remove 15+ deprecated functions and type aliases -- APIV2-01 (future milestone)
- Cross-platform FFI behavioral parity audit -- APIV2-02 (future milestone)
</user_constraints>

<phase_requirements>
## Phase Requirements

| ID | Description | Research Support |
|----|-------------|-----------------|
| API-01 | API naming consistency resolved | Comprehensive inventory of all `get_*` functions across 6 modules; exact rename mapping documented |
| API-02 | Idiomatic `use`/`result.try` pipelines applied | Analysis of current Result-handling patterns; identified candidates in birch.gleam, handler.gleam, file.gleam |
| API-03 | Opaque type boundaries reviewed and tightened | Full inventory of direct field accesses on LogRecord (6+ modules), GlobalConfig (birch.gleam), SampleConfig; missing accessor functions identified |
| API-04 | Builder patterns follow Gleam conventions consistently | Audit of AsyncConfig, JsonBuilder, ConsoleConfig builder patterns; all use consistent `with_*` style already |
| API-05 | Module-by-module review for non-idiomatic patterns | Module list with known issues per module documented below |
</phase_requirements>

## Architecture Patterns

### Getter Rename Inventory

Complete list of `get_*` functions to rename to bare form:

| Module | Current | New | Callers |
|--------|---------|-----|---------|
| `logger.gleam` | `get_level(logger)` | `level(logger)` | Tests (6 refs), birch.gleam |
| `logger.gleam` | `get_handlers(logger)` | `handlers(logger)` | Tests (4 refs) |
| `logger.gleam` | `get_context(logger)` | `context(logger)` | Tests (3 refs) |
| `config.gleam` | `get_level(config)` | `level(config)` | birch.gleam (1 ref) -- **CONFLICT with `level(lvl)` ConfigOption constructor** |
| `config.gleam` | `get_on_error(config)` | `on_error(config)` | **CONFLICT with `on_error(callback)` ConfigOption constructor** -- already deprecated, skip rename |
| `handler.gleam` | `get_error_callback(handler)` | `error_callback(handler)` | Tests (1 ref) |
| `record.gleam` | `get_metadata(record, key)` | `metadata(record, key)` | Tests (5 refs), property tests (3 refs) |
| `record.gleam` | `get_caller_id(record)` | `caller_id(record)` | Tests (2 refs) |
| `level_formatter.gleam` | `get_target_width(formatter)` | `target_width(formatter)` | No external callers found |
| `birch.gleam` | `get_level()` | -- | **Keep as-is**: module-level `get_level()` is a getter on global state, not on a type; bare `level()` would shadow the Level type import. Also distinct from `set_level()`. |
| `birch.gleam` | `get_config()` | -- | **Keep as-is**: returns global config, part of configure/get/reset trio |
| `birch.gleam` | `get_scoped_logger()` | -- | **Keep as-is**: returns scoped logger from platform storage |
| `birch.gleam` | `get_scope_context()` | -- | **Keep as-is**: returns scope context from platform storage |

**Critical issue -- config.gleam naming conflict:** `config.get_level(cfg)` cannot simply become `config.level(cfg)` because `config.level(lvl)` already exists as a ConfigOption constructor. Options:
1. Since GlobalConfig is becoming opaque, the accessor needs a unique name. Use `config.level(cfg)` for the accessor and rename the ConfigOption constructor to something else (but that's the established pattern).
2. Keep `config.get_level()` as the sole exception.
3. Since `config.get_on_error()` is already deprecated, just leave config getters as-is since the config module is mostly internal.

**Recommendation:** The config module's public getters (`get_level`, `get_on_error`) are used almost exclusively internally. Since GlobalConfig is becoming opaque and the ConfigOption constructors use bare names (`level`, `handlers`, `context`), keep config getters with `get_` prefix to avoid conflicts. Only rename getters on Logger, Handler, Record, and LevelFormatter types.

### Opaque Type Transition Plan

#### LogRecord (record.gleam)

**Current state:** Non-opaque with 6 fields directly accessed across 6+ modules.

**Accessor functions needed** (new ones marked with *):

| Field | Accessor | Status |
|-------|----------|--------|
| `timestamp` | `timestamp(record)` | * NEW |
| `level` | `level(record)` | * NEW |
| `logger_name` | `logger_name(record)` | * NEW |
| `message` | `message(record)` | * NEW |
| `metadata` | `metadata(record)` | * NEW (replaces `get_metadata` which takes a key; need both a full metadata accessor and the key-lookup one) |
| `caller_id` | `caller_id(record)` | Rename from `get_caller_id` |

**Direct field access sites to update:**

| File | Accesses | Count |
|------|----------|-------|
| `formatter.gleam` | `record.level`, `record.metadata`, `record.timestamp`, `record.logger_name`, `record.message` | 7 |
| `handler.gleam` | `record.level` | 1 |
| `handler/console.gleam` | `record.level`, `record.timestamp`, `record.metadata`, `record.logger_name`, `record.message` | 10 |
| `handler/json.gleam` | `r.timestamp`, `r.level`, `r.logger_name`, `r.message`, `r.metadata` | 5 |
| `erlang_logger.gleam` | `record.level`, `r.level`, `r.message`, `r.logger_name`, `r.metadata`, `r.caller_id` | 8 |
| `record.gleam` (internal) | `record.metadata`, `record.caller_id` | 3 |

**Note:** The `record.gleam` module itself can still use field access internally (within the opaque type's defining module). Only external modules need to use accessors.

**Metadata accessor design:** Need two functions:
- `metadata(record) -> Metadata` -- returns all metadata (NEW)
- `metadata_value(record, key) -> Result(MetadataValue, Nil)` -- renamed from `get_metadata(record, key)` or keep as `metadata(record, key)` with arity overload (Gleam doesn't support overloading, so needs different name)

**Recommendation:** Use `all_metadata(record) -> Metadata` for the full list and rename `get_metadata` to `metadata(record, key)` for key lookup.

#### GlobalConfig (config.gleam)

**Current state:** Non-opaque, fields accessed in birch.gleam.

**Direct field access sites:**
- `birch.gleam` line 225: `config.GlobalConfig(level: ..., handlers: ..., ...)` constructor in `default_config()`
- `birch.gleam` lines 268-270: `cfg.level`, `cfg.handlers`, `cfg.context` in `build_default_logger()`
- `birch.gleam` lines 302-304: Same pattern in `new()`
- `birch.gleam` line 442: `cfg.sampling` in `should_sample()`
- `config.gleam` lines 110-117: Internal `apply_option` -- OK (same module)

**Accessor functions needed:**

| Field | Accessor | Status |
|-------|----------|--------|
| `level` | `get_level(config)` | EXISTS (keep with get_ prefix due to naming conflict) |
| `handlers` | `handlers(config)` | * NEW |
| `context` | `context(config)` | * NEW -- **CONFLICT with `context(ctx)` ConfigOption constructor** |
| `on_error` | `get_on_error(config)` | EXISTS (deprecated, but functionally needed) |
| `sampling` | `sampling(config)` | * NEW -- **CONFLICT with `sampling(config)` ConfigOption constructor** |

**Naming conflicts are severe.** The ConfigOption constructors use the same bare names. Solutions:
1. Prefix ConfigOption constructors with `config_` (breaking change, but pre-1.0)
2. Keep GlobalConfig non-opaque (simplest)
3. Use different naming for config accessors: `get_level`, `get_handlers`, etc. (back to get_ prefix)

**Recommendation:** Since ConfigOption constructors already use the bare names, and GlobalConfig accessors would conflict, the cleanest approach is to keep `get_` prefix on GlobalConfig accessors specifically. Add `get_handlers()`, `get_context()`, `get_sampling()` with `get_` prefix. This is a principled exception: the config module has a builder pattern (ConfigOption) that claims the bare names.

**Constructor replacement:** `default_config()` in birch.gleam constructs GlobalConfig directly. Need a constructor in config.gleam:
```gleam
pub fn new(level, handlers, context, on_error, sampling) -> GlobalConfig
```

#### SampleConfig (config.gleam)

**Current state:** Non-opaque, fields accessed in sampling.gleam.

**Direct field access sites:**
- `sampling.gleam` lines 69-70: `sample_config.level`, `sample_config.rate`

**New accessor functions:**
- `level(config) -> Level` -- **CONFLICT with `config.level(lvl)` ConfigOption constructor** (but SampleConfig is in the same module)
- `rate(config) -> Float`

**Recommendation:** Since SampleConfig lives in config.gleam alongside ConfigOption, there's a naming clash. Move SampleConfig accessors to sampling.gleam module where they won't conflict, or prefix them: `sample_level(config)`, `sample_rate(config)`. Better: define accessors in sampling.gleam as `sampling.level(config)` and `sampling.rate(config)`.

### use/result.try Candidates (API-02)

Searched all source files for chained Result-handling patterns. Findings:

**birch.gleam -- `default_logger()` function (lines 277-289):**
```gleam
case scoped_logger.get_scoped_logger() {
  Ok(lgr) -> lgr
  Error(Nil) ->
    case get_cached_default_logger() {
      Ok(lgr) -> lgr
      Error(Nil) -> {
        let lgr = build_default_logger(get_config())
        set_cached_default_logger(lgr)
        lgr
      }
    }
}
```
This is a fallback chain, not a `result.try` candidate. The nested cases are semantically "try this, then try that, then default". `result.lazy_or` or `option.lazy_or` pattern. Not a good `use` candidate because each branch returns a value (not threading through Results).

**handler.gleam -- `handle()` function (lines 88-113):**
```gleam
case should_handle(handler, record.level) {
  True -> {
    case platform.safe_call(fn() { handler.write(record) }) {
      Ok(Nil) -> Nil
      Error(error_msg) -> {
        case handler.error_callback {
          Some(callback) -> ...
          None -> Nil
        }
      }
    }
  }
  False -> Nil
}
```
This is error handling with side effects, not a pipeline. Not a `use` candidate.

**file.gleam -- various functions:**
Result handling here is for I/O errors with fallbacks. Not pipeline-style.

**Conclusion for API-02:** There are very few (possibly zero) places in the current codebase where `use`/`result.try` would improve readability. The existing code uses `case` for control flow branching, which is appropriate. The phase plan should document this finding rather than force unnecessary refactoring.

### Builder Pattern Audit (API-04)

| Type | Builder Functions | Return Type Consistent? | Notes |
|------|-------------------|------------------------|-------|
| Logger | `new()`, `with_level()`, `with_handler()`, `with_handlers()`, `with_context()`, `with_time_provider()`, `with_caller_id_capture()` | YES -- all return Logger | Clean |
| Handler | `new()`, `with_min_level()`, `with_error_callback()` | YES -- all return Handler | Clean |
| AsyncConfig | `config()`, `with_queue_size()`, `with_flush_interval()`, `with_overflow()` | YES -- all return AsyncConfig | Clean |
| JsonBuilder | `builder()`, `add_field()`, `add_timestamp()`, `add_level()`, `add_logger()`, `add_message()`, `add_metadata()`, `add_custom()` | YES -- all return JsonBuilder | Clean |
| GlobalConfig | `with_level()` | Only one builder fn | Other fields set through ConfigOption pattern |
| LevelFormatter | Constructor functions only | N/A | Uses factory pattern, not builder |

**Conclusion for API-04:** Builder patterns are already consistent. No changes needed.

### Module-by-Module Audit Checklist (API-05)

| Module | Getter Renames | Opaque Changes | use/result.try | Builder | Other Issues |
|--------|---------------|----------------|----------------|---------|-------------|
| `birch.gleam` | `get_level()` keep as-is | Update field access on GlobalConfig | None found | N/A | Direct GlobalConfig construction needs constructor |
| `logger.gleam` | `get_level` -> `level`, `get_handlers` -> `handlers`, `get_context` -> `context` | None (already opaque) | None found | Clean | None |
| `handler.gleam` | `get_error_callback` -> `error_callback` | None (already opaque) | None | Clean | None |
| `config.gleam` | Keep `get_level`, `get_on_error`; add `get_handlers`, `get_context`, `get_sampling` | Make GlobalConfig + SampleConfig opaque | None | ConfigOption pattern fine | Need constructor fn, need SampleConfig accessors |
| `record.gleam` | `get_metadata` -> `metadata`, `get_caller_id` -> `caller_id` | Make LogRecord opaque; add `timestamp`, `level`, `logger_name`, `message`, `all_metadata` accessors | None | N/A | None |
| `formatter.gleam` | None | Update to use LogRecord accessors | None | N/A | None |
| `handler/console.gleam` | None | Update to use LogRecord accessors | None | N/A | None |
| `handler/json.gleam` | None | Update to use LogRecord accessors | None | Builder clean | None |
| `handler/file.gleam` | None | None | None | N/A | None |
| `handler/async.gleam` | None | None | None | Builder clean | None |
| `level.gleam` | None | Stays non-opaque (enum) | None | N/A | None |
| `level_formatter.gleam` | `get_target_width` -> `target_width` | None (already opaque) | None | Factory pattern OK | None |
| `meta.gleam` | None | None | None | N/A | None |
| `sampling.gleam` | None | Add `level()`, `rate()` for SampleConfig | None | N/A | Direct field access on SampleConfig |
| `scope.gleam` | None | None | None | N/A | None |
| `erlang_logger.gleam` | None | Update to use LogRecord accessors | None | N/A | Deprecated functions access LogRecord fields directly |

## Don't Hand-Roll

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Naming convention enforcement | Manual grep for naming violations | Module-by-module checklist with `gleam build` verification | Compiler catches name errors; checklist ensures completeness |
| Opaque type migration | Manual field access search | `gleam build` after making type opaque | Compiler produces errors for every external field access |

**Key insight:** Making a type opaque and running `gleam build` is the most reliable way to find all sites that need updating. The compiler will produce an error for every direct field access from outside the defining module.

## Common Pitfalls

### Pitfall 1: Naming Conflicts in config.gleam
**What goes wrong:** ConfigOption constructors (`level`, `handlers`, `context`, `sampling`, `on_error`) use bare names, conflicting with bare-style accessors for GlobalConfig.
**Why it happens:** ConfigOption and GlobalConfig share the same module namespace.
**How to avoid:** Keep `get_` prefix for GlobalConfig accessors as a principled exception. Document the reason.
**Warning signs:** Gleam compiler errors about duplicate function names.

### Pitfall 2: LogRecord Field Access Breakage Cascade
**What goes wrong:** Making LogRecord opaque breaks 6+ modules simultaneously.
**Why it happens:** LogRecord fields are accessed directly across formatter, handler, console, json, and erlang_logger modules.
**How to avoid:** Add all accessor functions FIRST while LogRecord is still non-opaque, update all call sites to use accessors, THEN make it opaque. Verify with `gleam build` at each step.
**Warning signs:** Build errors after making type opaque.

### Pitfall 3: Test Breakage Volume
**What goes wrong:** Renaming getters breaks 20+ test call sites and 6+ example files.
**Why it happens:** Tests extensively use `logger.get_level()`, `record.get_metadata()`, etc.
**How to avoid:** Use find-and-replace for each rename. Run `just check` (format + tests on both targets) after each module's renames.
**Warning signs:** Test compilation failures.

### Pitfall 4: Metadata Accessor Naming
**What goes wrong:** `get_metadata(record, key)` lookup by key vs. needing a full metadata accessor.
**Why it happens:** When LogRecord becomes opaque, formatters need to read all metadata, not just by key.
**How to avoid:** Add `all_metadata(record) -> Metadata` for full list, rename `get_metadata(record, key)` to `metadata(record, key)` for key lookup.
**Warning signs:** Formatters can't access metadata list after opaque transition.

### Pitfall 5: Dual-Target Build
**What goes wrong:** Changes compile on Erlang but break on JavaScript (or vice versa).
**Why it happens:** `@target(erlang)` and `@target(javascript)` blocks may have different patterns.
**How to avoid:** Always run `just test` (both targets) after changes, not just `gleam test`.
**Warning signs:** CI failure on one target.

## Code Examples

### Bare Getter Pattern (gleam_stdlib reference)

```gleam
// Idiomatic Gleam: bare getter on opaque type
pub fn name(logger: Logger) -> String {
  logger.name
}

// Idiomatic Gleam: with_ setter/builder
pub fn with_level(logger: Logger, level: Level) -> Logger {
  Logger(..logger, min_level: level)
}
```

### LogRecord Accessor Functions (to add)

```gleam
/// Get the timestamp from a log record.
pub fn timestamp(record: LogRecord) -> String {
  record.timestamp
}

/// Get the level from a log record.
pub fn level(record: LogRecord) -> Level {
  record.level
}

/// Get the logger name from a log record.
pub fn logger_name(record: LogRecord) -> String {
  record.logger_name
}

/// Get the message from a log record.
pub fn message(record: LogRecord) -> String {
  record.message
}

/// Get all metadata from a log record.
pub fn all_metadata(record: LogRecord) -> Metadata {
  record.metadata
}
```

### GlobalConfig Constructor (to add in config.gleam)

```gleam
/// Create a new GlobalConfig with all fields specified.
pub fn new_config(
  level level: Level,
  handlers handlers: List(Handler),
  context context: Metadata,
  on_error on_error: Option(ErrorCallback),
  sampling sampling: Option(SampleConfig),
) -> GlobalConfig {
  GlobalConfig(
    level: level,
    handlers: handlers,
    context: context,
    on_error: on_error,
    sampling: sampling,
  )
}
```

### SampleConfig Accessors (to add in sampling.gleam)

```gleam
/// Get the level threshold from a sample configuration.
pub fn sample_level(sample_config: SampleConfig) -> Level {
  sample_config.level
}

/// Get the sampling rate from a sample configuration.
pub fn rate(sample_config: SampleConfig) -> Float {
  sample_config.rate
}
```

## Validation Architecture

### Test Framework
| Property | Value |
|----------|-------|
| Framework | gleeunit 1.0+ |
| Config file | gleam.toml (test deps section) |
| Quick run command | `gleam test` |
| Full suite command | `just test` (both Erlang + JS targets) |

### Phase Requirements -> Test Map
| Req ID | Behavior | Test Type | Automated Command | File Exists? |
|--------|----------|-----------|-------------------|-------------|
| API-01 | Getter renames compile and return correct values | unit | `gleam test` | Existing tests need updating |
| API-02 | use/result.try patterns (if any found) | unit | `gleam test` | Existing tests cover |
| API-03 | Opaque types enforce encapsulation; accessors work | unit | `gleam test` | Existing tests need updating to use accessors |
| API-04 | Builder chains return correct types | unit | `gleam test` | Existing tests cover |
| API-05 | All modules compile and pass tests after audit | integration | `just test` | Existing test suite |

### Sampling Rate
- **Per task commit:** `gleam test` (Erlang target, fast)
- **Per wave merge:** `just test` (both targets)
- **Phase gate:** `just check` (format + tests on both targets)

### Wave 0 Gaps
None -- existing test infrastructure covers all phase requirements. Tests need updating (call sites change) but no new test files or framework changes needed.

## Open Questions

1. **config.gleam accessor naming**
   - What we know: ConfigOption constructors use bare names, conflicting with GlobalConfig accessors
   - What's unclear: Whether to keep `get_` prefix for config or rename ConfigOption constructors
   - Recommendation: Keep `get_` prefix for GlobalConfig accessors as a principled exception (documented above)

2. **LogRecord metadata accessor naming**
   - What we know: Need both "all metadata" and "lookup by key" accessors
   - What's unclear: Best names that don't collide
   - Recommendation: `all_metadata(record)` for full list, `metadata(record, key)` for key lookup

3. **birch.gleam `get_level()` / `get_config()` / `get_scoped_logger()` / `get_scope_context()`**
   - What we know: These are module-level functions on global state, not type accessors
   - What's unclear: Whether the "bare getter" convention applies to global state getters
   - Recommendation: Keep `get_` prefix -- these are semantically different from type field accessors. They query global/process state and pair with setters (`set_level`/`configure`/`with_logger`/`with_scope`).

## Sources

### Primary (HIGH confidence)
- Direct codebase analysis of all 16 public modules in birch
- Gleam stdlib naming conventions (verified via gleam_stdlib source patterns)
- Gleam language documentation on opaque types

### Secondary (MEDIUM confidence)
- gleam-patterns skill file in .claude/skills/ (project-specific patterns)

## Metadata

**Confidence breakdown:**
- Standard stack: HIGH - no new dependencies, pure refactoring
- Architecture: HIGH - all changes identified from direct code analysis
- Pitfalls: HIGH - naming conflicts verified against actual code, field access inventory complete

**Research date:** 2026-03-05
**Valid until:** 2026-04-05 (stable, no external dependency changes)
