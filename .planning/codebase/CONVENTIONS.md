# Coding Conventions

**Analysis Date:** 2025-02-27

## Naming Patterns

**Files:**
- All lowercase with underscores: `console.gleam`, `handler.gleam`, `time.gleam`
- Plural names for modules containing collections: `handlers.gleam` would be unusual; instead handlers are in `handler.gleam`
- Module files match primary type they define: `logger.gleam` exports `Logger` type

**Functions:**
- Lowercase with underscores: `with_level()`, `should_log()`, `format_metadata()`
- Getter functions: `name()`, `get_level()`, `get_handlers()` (both patterns used)
- Setter/builder functions: `with_*()` pattern for immutable updates (e.g., `with_level()`, `with_context()`, `with_handler()`)
- Predicates: `should_log()`, `should_handle()`, `is_caller_id_capture_enabled()`, `is_scoped_context_available()`
- Lazy evaluation variants: `*_lazy()` suffix (e.g., `debug_lazy()`, `info_lazy()`)
- Metadata variants: `*_m()` suffix for old deprecated functions (e.g., `trace_m()`, `debug_m()`)

**Variables:**
- Lowercase with underscores: `log_level`, `handler_name`, `use_color`, `min_level`
- Private implementation functions prefixed with underscore or nested: `fn get_timestamp()` (private), `fn merge_metadata()`
- Short names for common patterns: `h` for handler, `lgr` for logger, `r` for record

**Types:**
- PascalCase: `Logger`, `Handler`, `LogRecord`, `LogLevel`, `LogStyle`, `ConsoleConfig`
- Variant constructors: `PascalCase` with no underscores (e.g., `Trace`, `Debug`, `Info`, `Stdout`, `Stderr`, `Success`, `Fail`)
- Config types use `*Config` suffix: `ConsoleConfig`, `SampleConfig`, `GlobalConfig`
- Function types use descriptive names: `TimestampFormatter`, `TimeProvider`, `Formatter` (for `fn(LogRecord) -> String`)

## Code Style

**Formatting:**
- Gleam's built-in formatter (runs via `just format`)
- 2-space indentation (enforced by `.editorconfig`)
- EditorConfig for consistency across editors

**Linting:**
- Gleam's built-in warnings (no external linter)
- Compiler warnings are treated as errors in CI
- Dead code elimination enforced during build

## Import Organization

**Order:**
1. Module documentation (doc comments starting with `////`)
2. Relative imports from `birch/*` modules (internal)
3. External imports (`gleam/`, `simplifile`, `gleam_json`)
4. Platform-specific imports with `@target()` guards

**Pattern:**
```gleam
//// Module documentation
////
//// Brief description of what this module does.

import birch/handler.{type Handler}
import birch/level.{type Level}
import gleam/list
import gleam/option.{type Option, None, Some}

@target(erlang)
import birch/erlang_logger

@target(javascript)
import birch/handler/console
```

**Path Aliases:**
- No path aliases used; direct module paths throughout
- Relative imports within `src/` directory structure

**Type Imports:**
- Type-only imports use `type` prefix: `import birch/level.{type Level}`
- This clarifies at import time what is a type vs. function

## Error Handling

**Patterns:**
- Use `Result` types for fallible operations: `Result(Config, Nil)` for config retrieval
- Log-level comparisons via integer conversion for performance: `level.gte(a, b)` compares `to_int()` values
- Handler errors never crash: wrapped in `platform.safe_call()` which returns `Result(Nil, String)` on failure
- Errors converted to metadata: `extract_error_metadata()` converts `Result(a, e)` → `Metadata` for logging

**Error Callbacks:**
- Type: `fn(HandlerError) -> Nil` - non-fatal, callback invoked post-error
- Handler failures trigger optional error callbacks without crashing application
- Error callbacks stored as `Option(ErrorCallback)` on handlers

## Logging

**Framework:** Built-in `gleam/io` for platform output
- `io.println()` for stdout
- `io.println_error()` for stderr
- Structured via custom `LogRecord` type

**Patterns:**
- Module functions use `////` doc comments (triple slash)
- Function documentation includes purpose, example usage, and platform notes
- Doc comments before every public function
- Example code blocks in doc strings marked with triple backticks

**Metadata Structure:**
- List of tuples: `List(#(String, String))` conceptually, but values are `MetadataValue` enum
- MetadataValue variants: `StringVal(String)`, `IntVal(Int)`, `FloatVal(Float)`, `BoolVal(Bool)`
- Metadata helpers in `meta` module: `meta.string()`, `meta.int()`, `meta.float()`, `meta.bool()`
- Metadata filtering in output: keys starting with underscore (`_*`) hidden from console output

## Comments

**When to Comment:**
- Every public function must have `////` doc comment
- Complex algorithms explained (e.g., color selection using hash modulo)
- Non-obvious logic (e.g., why `Err` instead of `Error` to avoid Result conflict)
- Section headers using repeated `// =...` (e.g., `// ============================================================================`)

**JSDoc/TSDoc:**
- Not applicable (Gleam uses `////` for documentation)
- Doc strings include type signatures automatically
- Example usage blocks encouraged in doc comments

## Function Design

**Size:** Functions typically 5-30 lines; longer functions split into helpers
- Example: `format_simple()` delegates to `format_record_simple()` and `build_format_fn()`
- Helpers prefixed with underscore or nested in module: `fn emit_record()`, `fn merge_metadata()`

**Parameters:**
- Named parameters used throughout: `name: String`, `write: fn(String) -> Nil`, `format: Formatter`
- Type annotations required on all parameters
- Builder pattern: each `with_*()` takes full object and modified field

**Return Values:**
- Explicit return types on all functions
- Nil return for side effects (handler writes)
- Option/Result for operations that may not have values
- Type functions return new instances of opaque types (immutability pattern)

## Module Design

**Exports:**
- All public functions listed after module doc comment as implicit exports
- Opaque types: `pub opaque type Logger { ... }` hides internal structure
- Type exports via `import birch/level.{type Level}` pattern

**Barrel Files:**
- Main entry point `src/birch.gleam` re-exports convenience functions from submodules
- Re-exports marked with `@deprecated()` for old API variants
- Example: `pub type LogLevel = Level` (deprecated alias)

**Internal Modules:**
- Marked in `gleam.toml`: `internal_modules = ["birch/internal", "birch/internal/*", "birch/sampling", "birch/handler/async"]`
- Not part of public API but used internally
- Examples: `birch/internal/platform.gleam`, `birch/internal/ansi.gleam`

## Pattern-Specific Conventions

**Builder Pattern:**
- Method chaining for configuration: `logger.new() |> logger.with_level() |> logger.with_context()`
- Each builder function: `fn(Type, NewValue) -> Type` returns modified instance
- Used for: Logger config, Handler config, JSON formatter setup

**Immutability:**
- All data structures immutable
- Updates via spread syntax: `Logger(..logger, min_level: min_level)`
- Never mutate input; return new instance

**Cross-Platform Code:**
- Use `@target(erlang)` and `@target(javascript)` attributes for conditional compilation
- Erlang FFI in `birch_ffi.erl`, JavaScript FFI in `birch_ffi.mjs`
- Platform utilities in `birch/internal/platform.gleam` (abstracts differences)

**Type Conversions:**
- Level represented as integer internally for comparison: `level.to_int()` for fast >= checks
- Metadata values explicitly typed: `MetadataValue` enum supports String, Int, Float, Bool
- Never use string coercion; explicit conversion functions

## Performance Considerations

**Lazy Evaluation:**
- Functions like `debug_lazy(message_fn)` only call function if level is enabled
- Avoids expensive string formatting for filtered-out logs
- Sampling functions check before executing handler code

**Level Filtering:**
- Integer comparison (`gte()` using `to_int()`) faster than string comparison
- Handler filter checks before calling write function
- Double-check pattern: global level → handler level → write

**Caching:**
- Default logger cached in FFI storage (`get_cached_default_logger()`)
- Invalidated on config changes (`clear_cached_default_logger()`)
- Reduces allocation for common-path module-level logging

---

*Convention analysis: 2025-02-27*
