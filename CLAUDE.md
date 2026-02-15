# CLAUDE.md - AI Assistant Guide for birch

This document provides essential context for AI assistants working with the birch codebase.

## Project Overview

**birch** is a logging library for Gleam with cross-platform support (Erlang and JavaScript targets), structured logging with metadata, multiple output handlers, and lazy evaluation for performance.

The name "birch" comes from birch trees, whose white bark gleams in the light.

### Key Features
- Cross-platform: Works on both Erlang (BEAM) and JavaScript targets
- Zero-configuration startup with sensible defaults
- Structured logging with key-value metadata
- Multiple handlers: console (with colors), file (with rotation), JSON
- Lazy evaluation to avoid expensive string formatting when logs are filtered
- Named loggers with persistent context

## Codebase Structure

```
src/
├── birch.gleam                  # Main public API - re-exports and convenience functions
├── birch/
│   ├── config.gleam             # Configuration types and defaults
│   ├── erlang_logger.gleam      # OTP logger integration (Erlang-only)
│   ├── level.gleam              # Level type (Trace/Debug/Info/Warn/Err/Fatal)
│   ├── level_formatter.gleam    # Level formatting and presentation options
│   ├── record.gleam             # LogRecord type and Metadata
│   ├── logger.gleam             # Logger type with handlers and context
│   ├── handler.gleam            # Handler interface and null handler
│   ├── formatter.gleam          # Format functions (human_readable, simple)
│   ├── sampling.gleam           # Log sampling for high-volume scenarios
│   ├── scope.gleam              # Scoped logging context
│   ├── handler/
│   │   ├── async.gleam          # Async/buffered handler wrapper
│   │   ├── console.gleam        # Console output with color support
│   │   ├── json.gleam           # JSON-formatted output
│   │   └── file.gleam           # File output with size-based rotation
│   └── internal/
│       └── platform.gleam       # Cross-platform FFI declarations
├── birch_ffi.erl                # Erlang FFI implementation
└── birch_ffi.mjs                # JavaScript FFI implementation

examples/                         # 17 runnable examples (01-quick-start through 17-handler-errors)

test/
├── birch_test.gleam             # Unit tests using gleeunit
├── erlang_logger_test.gleam     # OTP logger integration tests
├── integration/                 # JavaScript runtime integration tests
└── property_test.gleam          # Property tests (pending qcheck 1.0+ migration)

docs/
├── PRD.md                       # Product Requirements Document
└── IMPLEMENTATION_PLAN.md       # Implementation phases and status
```

### Internal Modules

The `birch/internal` directory contains internal implementation details that are not part of the public API. These are marked in `gleam.toml`:

```toml
internal_modules = ["birch/internal", "birch/internal/*"]
```

## Development Commands

This project uses [just](https://just.systems/) as a task runner. Run `just` to see all available commands.

### Common Tasks

```bash
just              # List all available commands
just build        # Build for Erlang target (alias: b)
just build-js     # Build for JavaScript target
just build-all    # Build for both targets
just test         # Run tests on both targets (alias: t)
just test-erlang  # Run tests on Erlang only
just test-js      # Run tests on JavaScript only
just format       # Format source code (alias: f)
just format-check # Check formatting
just check        # Run format-check + config-sync + tests on both targets (alias: c)
just check-quick  # Run format-check + config-sync + Erlang tests only
just docs         # Generate documentation (alias: d)
just deps         # Download dependencies
just clean        # Remove build artifacts
```

### Integration Tests (JavaScript Target)

Integration tests run compiled Gleam fixtures on Node.js, Deno, and Bun:

```bash
just test-integration-node  # Run on Node.js
just test-integration-deno  # Run on Deno
just test-integration-bun   # Run on Bun (via Node.js test runner)
just test-integration       # Run on Node.js (default)
just test-integration-all   # Run on all three runtimes
```

### Examples

17 runnable examples in `examples/` directory:

```bash
just test-examples           # Test all examples on Erlang
just test-examples-node      # Test all examples on Node.js
just test-examples-all       # Test on Erlang + Node.js
just test-example-erlang 01-quick-start  # Test specific example
just demo                    # Run console handler demo (shows all presentation options)
```

### Local CI Validation

```bash
just ci                      # Full CI validation (uses Docker/act)
just ci-host                 # Run CI without Docker (uses local tools)
just check-full              # Format, strict build, tests, examples, integration
```

### Code Coverage

```bash
just coverage                # Run coverage on both Erlang and JavaScript
just coverage-js             # JavaScript tests with coverage (c8)
just coverage-erlang         # Erlang tests with coverage (cover)
just coverage-erlang-lcov    # Erlang coverage with LCOV export (for Codecov)
just coverage-js-report      # Generate JS coverage HTML report
```

### Watch Mode (requires watchexec)

```bash
just watch        # Watch and rebuild on changes
just watch-test   # Watch and run tests on changes
```

### Direct Gleam Commands

You can also use gleam commands directly:

```bash
gleam build                       # Build for Erlang
gleam build --target javascript   # Build for JavaScript
gleam test                        # Run tests on Erlang
gleam test --target javascript    # Run tests on JavaScript
gleam format src test             # Format code
gleam format --check src test     # Check formatting
gleam docs build                  # Generate documentation
gleam deps download               # Download dependencies
```

## Architecture Patterns

### Logger Creation and Configuration

Loggers are immutable and configured via method chaining:

```gleam
import birch as log
import birch/level
import birch/handler/json

let logger = log.new("myapp.database")
  |> log.with_level(level.Debug)
  |> log.with_context([#("service", "db")])
  |> log.with_handler(json.handler())
```

### Handler Interface

Handlers receive `LogRecord` values and output formatted strings. The handler interface:

```gleam
handler.new(
  name: "custom",
  write: fn(message: String) -> Nil,
  format: fn(LogRecord) -> String,
)
```

### Level Filtering

Log levels are compared using integer values for fast filtering:
- Trace (0) < Debug (1) < Info (2) < Warn (3) < Err (4) < Fatal (5)

Note: `Err` is used instead of `Error` to avoid conflict with Result's Error constructor.

### Metadata

Metadata uses `List(#(String, String))` - both keys and values are strings for cross-platform compatibility.

## Cross-Platform FFI

Platform-specific operations are in `src/birch/internal/platform.gleam`:

- `timestamp_iso8601()` - Get current time in ISO 8601 format
- `write_stdout(message)` / `write_stderr(message)` - Output functions
- `is_stdout_tty()` - TTY detection for color support

Implementations:
- **Erlang**: `src/birch_ffi.erl` - Uses `calendar`, `io`, and `os` modules
- **JavaScript**: `src/birch_ffi.mjs` - Supports Node.js, Deno, Bun, and browser fallbacks

## Testing

### Test Framework
- **gleeunit** - Standard test runner
- **qcheck** - Property-based testing

### Running Tests
Tests must pass on both targets:
```bash
just test          # Run on both targets
just test-erlang   # Erlang only
just test-js       # JavaScript only
```

### Test Structure
Tests are organized by module:
- `test/birch_test.gleam` - Unit tests
  - Level tests
  - Record tests
  - Formatter tests
  - Logger tests
  - Handler tests
  - JSON handler tests
  - Main API tests
- `test/time_test.gleam` - Internal time module tests
- `test/property_test.gleam` - Property-based tests using qcheck
  - Level property tests (ordering, comparison, roundtrip)
  - Record property tests (metadata operations)
  - Formatter property tests (output invariants)

## Dependencies

### Runtime
- `gleam_stdlib` (>= 0.44.0)
- `gleam_json` (>= 2.0.0) - For JSON formatter
- `simplifile` (>= 2.0.0) - For file operations
- `gleam_otp` (>= 1.0.0) - For async handler and OTP integration
- `gleam_erlang` (>= 1.0.0) - For Erlang-specific features

### Development
- `gleeunit` (>= 1.0.0) - Testing
- `qcheck` (>= 1.0.0) - Property testing

## CI/CD

GitHub Actions (`.github/workflows/ci.yml`) runs on push/PR to main using `just` tasks:
- Tests on both Erlang and JavaScript targets (`just test-erlang`, `just test-js`)
- Format checking (`just format-check`)
- Documentation build (`just docs`)

## Commit Configuration

Single source of truth pattern for commit types and changelog generation:
- `commit-types.json` - defines commit types, changelog groups, and excluded scopes
- `commit-config-gen` - Go CLI tool that generates configs (install: `go install github.com/tylerbutler/commit-config-gen@latest`)

### Workflow
```bash
just g                    # Regenerate configs after editing commit-types.json
just changelog-preview    # Preview unreleased changelog
just changelog            # Generate CHANGELOG.md
```

### Scope Exclusions
Commits with scopes in `excluded_scopes` (e.g., `fix(ci)`, `chore(deps)`) are hidden from changelog.
Edit `commit-types.json` to add/remove excluded scopes.

## Conventions

### Naming
- Use `Err` instead of `Error` for the error log level
- Handler names include their type (e.g., `"file:/var/log/app.log"`)
- Logger names use dot notation for hierarchy (e.g., `"myapp.database"`)

### API Design
- Module-level functions (e.g., `log.info("msg", [])`) use a default logger
- Logger-specific functions use the `logger` module directly (e.g., `logger.info(lgr, "msg", [])`)
- All logging functions accept metadata as the last argument
- Lazy variants use `_lazy` suffix (e.g., `debug_lazy`)

### Error Handling
- Handler failures don't crash the application
- File handler logs errors to stderr on write failure
- Null handler provided for testing/disabling output

## Common Tasks

### Adding a New Handler
1. Create a new file in `src/birch/handler/`
2. Implement formatting using `formatter.Formatter` type
3. Use `handler.new()` to create the handler
4. Add tests in `test/birch_test.gleam`

### Adding a New Log Level
1. Add variant to `Level` type in `level.gleam`
2. Update `to_int`, `from_string`, `to_string`, `to_string_lowercase`
3. Add convenience functions in `logger.gleam` and `birch.gleam`
4. Add color mapping in `console.gleam`
5. Add tests

### Modifying FFI
When changing platform-specific code:
1. Update both `birch_ffi.erl` AND `birch_ffi.mjs`
2. Ensure behavior is consistent across platforms
3. Test on both Erlang and JavaScript targets

## Tool Versions

Specified in `.tool-versions` (source of truth, required by CI):
- Erlang: 27.2.1
- Gleam: 1.14.0
- just: 1.38.0

`.mise.toml` adds dev tools (act, bun, deno, node) and inherits erlang/gleam/just from `.tool-versions`.

## Known Limitations

1. **Coverage maps to generated code**: Coverage reports show compiled Erlang/JS code, not Gleam source. Line numbers don't match. Use `just coverage` for module-level percentages locally. Hosted services (Codecov, Coveralls) don't work with Gleam due to path/line mismatches.
2. **Scoped context on JS**: JavaScript lacks Erlang's process dictionary; scoped context requires explicit passing or AsyncLocalStorage (Node.js only)
3. **File handler rotation**: Only size-based rotation is implemented; time-based rotation is a future feature (see PRD Phase 3)
4. **Stale build artifacts**: If coverage shows unexpected modules (e.g., 0% coverage for non-existent files), run `rm -rf build/dev && just build` to clean stale artifacts

## References

- [Product Requirements Document](docs/PRD.md) - Detailed requirements and design decisions
- [README.md](README.md) - User-facing documentation with examples
- [Gleam Documentation](https://hexdocs.pm/birch/) - API documentation (after publishing)
