# Testing Patterns

**Analysis Date:** 2025-02-27

## Test Framework

**Runner:**
- `gleeunit` (>= 1.0.0) - Gleam's standard test framework
- Config: No explicit config file; uses gleeunit defaults
- Both Erlang and JavaScript targets supported

**Assertion Library:**
- `gleeunit/should` - Fluent assertion DSL
- Pattern: `value |> should.equal(expected)`, `value |> should.be_true`

**Run Commands:**
```bash
just test              # Run tests on both Erlang and JavaScript targets
just test-erlang       # Run on Erlang only
just test-js           # Run on JavaScript only
gleam test             # Direct gleam test (Erlang)
gleam test --target javascript  # Direct gleam test (JavaScript)
```

## Test File Organization

**Location:**
- Co-located with source code in `test/` directory parallel to `src/`
- Test files mirror module structure: `src/birch/logger.gleam` → `test/birch_test.gleam`
- Integration tests in `test/integration/` subdirectory
- Fixtures in `test/integration/fixtures/`

**Naming:**
- Test files: `*_test.gleam` suffix
- Test functions: `*_test` suffix (e.g., `level_ordering_test()`, `logger_creation_test()`)
- Property tests in separate file: `test/property_test.gleam`
- Integration fixture modules: `json_fixture.gleam`, `console_fixture.gleam`, `metadata_fixture.gleam`

**Structure:**
```
test/
├── birch_test.gleam              # Main unit tests
├── time_test.gleam               # Time module tests
├── erlang_logger_test.gleam      # OTP logger integration tests
├── property_test.gleam           # Property-based tests (qcheck)
├── integration/
│   └── fixtures/
│       ├── json_fixture.gleam
│       ├── console_fixture.gleam
│       └── metadata_fixture.gleam
```

## Test Structure

**Suite Organization:**
```gleam
pub fn main() {
  gleeunit.main()
}

// ============================================================================
// Level Tests
// ============================================================================

pub fn level_ordering_test() {
  level.compare(level.Trace, level.Debug)
  |> should.equal(order.Lt)
}

pub fn level_gte_test() {
  level.gte(level.Info, level.Debug)
  |> should.be_true

  level.gte(level.Debug, level.Info)
  |> should.be_false
}
```

**Patterns:**
- `pub fn main()` required - calls `gleeunit.main()`
- Section headers with repeated `// =...` for organization
- Descriptive function names that explain what's being tested
- Each test function returns unit with no parameters (gleeunit requirement)
- Assertions via `|> should.*` chaining

## Mocking

**Framework:**
- No explicit mocking library used
- Mocking via handler test utilities: `handler.null()` for no-op handler
- Custom test doubles created as needed

**Patterns:**

**Null Handler Pattern:**
```gleam
pub fn handler_null_test() {
  let h = handler.null()

  handler.name(h)
  |> should.equal("null")

  // null handler silently discards logs
  handler.handle(h, test_record)
}
```

**Custom Write Function for Verification:**
```gleam
pub fn logger_with_handler_test() {
  // Store logged records in a list via custom handler
  let messages = []  // Erlang process dict or mutable state needed for capture
  let h = handler.new(
    name: "test",
    write: fn(msg) { /* capture message */ },
    format: formatter.simple,
  )

  logger.info(lgr, "Test message", [])
  // Verify handler received the record
}
```

**Time Provider for Deterministic Testing:**
```gleam
pub fn logger_custom_timestamp_test() {
  let custom_formatter = fn(_ts) { "CUSTOM_TIME" }

  let lgr =
    logger.new("test")
    |> logger.with_custom_timestamp(custom_formatter)
    |> logger.with_handlers([handler.null()])

  // The logger uses custom formatter instead of system time
  logger.info(lgr, "Test message", [])
}
```

**What to Mock:**
- Time providers (for deterministic timestamps)
- Handlers (use `handler.null()` or custom capture functions)
- Platform-specific operations via `@target()` guards in tests

**What NOT to Mock:**
- Core logging logic (test the actual behavior)
- Level comparison functions (test against actual values)
- Formatter output (verify exact string output)
- Metadata handling (test actual list operations)

## Fixtures and Factories

**Test Data:**
```gleam
// Creating test records
pub fn test_record_creation_test() {
  let r = record.new(
    timestamp: "2024-12-26T10:30:45.123Z",
    level: level.Info,
    logger_name: "test",
    message: "Hello",
    metadata: [meta.string("key", "value")],
  )

  r.timestamp
  |> should.equal("2024-12-26T10:30:45.123Z")
}

// Creating test loggers
pub fn test_logger_creation_test() {
  let lgr =
    logger.new("test")
    |> logger.with_level(level.Debug)
    |> logger.with_context([meta.string("service", "api")])

  logger.get_level(lgr)
  |> should.equal(level.Debug)
}
```

**Location:**
- Fixtures defined inline in test functions
- No separate fixtures module (minimal test setup)
- Metadata helpers: `meta.string()`, `meta.int()`, `meta.float()`, `meta.bool()`

**Factory Functions (used as convenience):**
- `handler.null()` - Create no-op handler
- `logger.new()` - Create logger with defaults
- `logger.silent()` - Create logger with no handlers
- `record.new()` - Create log record

## Coverage

**Requirements:** No explicit coverage target enforced
- Tests run on both Erlang and JavaScript targets
- Coverage reports generated locally but not enforced for commits

**View Coverage:**
```bash
just coverage          # Run coverage on both targets with reporting
just coverage-erlang   # Erlang target coverage (using cover module)
just coverage-js       # JavaScript coverage (using c8)
just coverage-erlang-lcov  # Erlang with LCOV export for CI services
just coverage-js-report    # Generate JS HTML coverage report
```

**Known Limitations:**
- Erlang coverage maps to generated code, not Gleam source
- Line numbers in coverage reports don't match source files
- Use module-level percentages locally; hosted services (Codecov) don't align with Gleam
- Coverage reports show unexpected modules if stale artifacts exist (run `rm -rf build/dev && just build`)

## Test Types

**Unit Tests:**
- Scope: Individual functions and types in isolation
- Approach: Direct function calls with known inputs, verify outputs
- Location: `test/birch_test.gleam`, `test/time_test.gleam`
- Examples:
  - Level ordering: `level.compare()` returns correct `Order`
  - Record creation: `record.new()` produces correct struct
  - Logger configuration: `with_level()` updates minimum log level
  - Handler filtering: `should_handle()` respects handler's min level

**Integration Tests:**
- Scope: Multiple components working together
- Approach: Create logger with handlers, emit log, verify behavior
- Location: `test/integration/` (compiled fixtures for Node.js/Deno/Bun)
- Run: `just test-integration-node`, `just test-integration-deno`, `just test-integration-all`
- Examples:
  - JavaScript runtime integration: fixtures compiled to JS, executed on Node/Deno
  - OTP logger integration: Erlang-only tests in `test/erlang_logger_test.gleam`

**E2E Tests:**
- Framework: Not used for traditional E2E
- Alternative: Example programs (`examples/01-quick-start` through `examples/17-*`)
- Run: `just test-examples` compiles and executes examples

**Property-Based Tests:**
- Framework: `qcheck` (>= 1.0.0)
- Location: `test/property_test.gleam`
- Generator pattern: `qcheck.from_generators()` with list of alternatives
- Usage: `use value <- qcheck.given(generator)` for test input generation
- Examples:
  - Level roundtrip: `to_string_lowercase() → from_string()` returns same level
  - Level ordering: All `to_int()` values strictly increasing
  - Comparison consistency: `gte()` consistent with `compare()`

## Common Patterns

**Assertion Patterns:**

**Equality:**
```gleam
value |> should.equal(expected)

level.to_string(level.Trace)
|> should.equal("TRACE")
```

**Boolean Predicates:**
```gleam
condition |> should.be_true
condition |> should.be_false

level.gte(level.Info, level.Debug)
|> should.be_true
```

**String Containment:**
```gleam
formatted
|> string.contains("expected text")
|> should.be_true

formatted
|> string.contains("2024-12-26T10:30:45.123Z")
|> should.be_true
```

**Async Testing:**
```gleam
// Async handler returns Nil immediately
// Handler buffer managed by async actor
let handler = async.handler(json.handler())

logger.info(lgr, "Message", [])
// Handler writes asynchronously; no wait pattern in tests
// Tests verify handler creation/configuration, not async completion
```

**Error Testing:**
```gleam
pub fn error_result_test() {
  let result: Result(Int, String) = Error("Connection failed")

  logger.error_result(lgr, "Database error", result, [])
  // Error value converted to metadata and logged
  // Verify metadata contains error via handler capture
}

// Parsing errors
pub fn level_from_string_invalid_test() {
  level.from_string("invalid")
  |> should.equal(Error(Nil))
}
```

**Result Pattern Testing:**
```gleam
pub fn result_test() {
  case result {
    Ok(value) -> value |> should.equal(expected)
    Error(_) -> should.fail("Expected Ok")
  }
}
```

## Test Execution Flow

**Standard Invocation:**
```bash
# Both targets
just test

# Outputs:
# Erlang: Running Gleam tests
# JavaScript: Running tests on Node.js (default)
```

**Watch Mode (requires watchexec):**
```bash
just watch-test     # Watches and reruns tests on changes
```

**CI Integration:**
- GitHub Actions runs `just test-erlang` and `just test-js` separately
- Format check: `just format-check`
- All must pass before merge

---

*Testing analysis: 2025-02-27*
