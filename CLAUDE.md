# CLAUDE.md - AI Assistant Guide for gleam_log

This document provides essential context for AI assistants working with the gleam_log codebase.

## Project Overview

**gleam_log** is a modern, production-ready logging library for Gleam. It provides cross-platform support (Erlang and JavaScript targets), structured logging with metadata, multiple output handlers, and lazy evaluation for performance.

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
├── gleam_log.gleam              # Main public API - re-exports and convenience functions
├── gleam_log/
│   ├── level.gleam              # LogLevel type (Trace/Debug/Info/Warn/Err/Fatal)
│   ├── record.gleam             # LogRecord type and Metadata
│   ├── logger.gleam             # Logger type with handlers and context
│   ├── handler.gleam            # Handler interface and null handler
│   ├── formatter.gleam          # Format functions (human_readable, simple)
│   ├── handler/
│   │   ├── console.gleam        # Console output with color support
│   │   ├── json.gleam           # JSON-formatted output
│   │   └── file.gleam           # File output with size-based rotation
│   └── internal/
│       └── platform.gleam       # Cross-platform FFI declarations
├── gleam_log_ffi.erl            # Erlang FFI implementation
└── gleam_log_ffi.mjs            # JavaScript FFI implementation

test/
├── gleam_log_test.gleam         # Unit tests using gleeunit
└── property_test.gleam          # Property tests (pending qcheck 1.0+ migration)

docs/
└── PRD.md                       # Product Requirements Document
```

### Internal Modules

The `gleam_log/internal` directory contains internal implementation details that are not part of the public API. These are marked in `gleam.toml`:

```toml
internal_modules = ["gleam_log/internal", "gleam_log/internal/*"]
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
just check        # Run format-check + tests on both targets (alias: c)
just check-quick  # Run format-check + Erlang tests only
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
let logger = gleam_log.new("myapp.database")
  |> gleam_log.with_level(level.Debug)
  |> gleam_log.with_context([#("service", "db")])
  |> gleam_log.with_handler(json.handler())
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

Platform-specific operations are in `src/gleam_log/internal/platform.gleam`:

- `timestamp_iso8601()` - Get current time in ISO 8601 format
- `write_stdout(message)` / `write_stderr(message)` - Output functions
- `is_stdout_tty()` - TTY detection for color support

Implementations:
- **Erlang**: `src/gleam_log_ffi.erl` - Uses `calendar`, `io`, and `os` modules
- **JavaScript**: `src/gleam_log_ffi.mjs` - Supports Node.js, Deno, Bun, and browser fallbacks

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
- `test/gleam_log_test.gleam` - Unit tests
  - Level tests
  - Record tests
  - Formatter tests
  - Logger tests
  - Handler tests
  - JSON handler tests
  - Main API tests
- `test/property_test.gleam` - Property-based tests using qcheck
  - Level property tests (ordering, comparison, roundtrip)
  - Record property tests (metadata operations)
  - Formatter property tests (output invariants)

## Dependencies

### Runtime
- `gleam_stdlib` (>= 0.44.0)
- `gleam_json` (>= 2.0.0) - For JSON formatter
- `simplifile` (>= 2.0.0) - For file operations

### Development
- `gleeunit` (>= 1.0.0) - Testing
- `qcheck` (>= 1.0.0) - Property testing

## CI/CD

GitHub Actions (`.github/workflows/ci.yml`) runs on push/PR to main using `just` tasks:
- Tests on both Erlang and JavaScript targets (`just test-erlang`, `just test-js`)
- Format checking (`just format-check`)
- Documentation build (`just docs`)

## Conventions

### Naming
- Use `Err` instead of `Error` for the error log level
- Handler names include their type (e.g., `"file:/var/log/app.log"`)
- Logger names use dot notation for hierarchy (e.g., `"myapp.database"`)

### API Design
- Module-level functions (e.g., `log.info()`) use a default logger
- Logger-specific functions are prefixed with `logger_` (e.g., `logger_info`)
- Metadata variants use `_m` suffix (e.g., `info_m` vs `info`)
- Lazy variants use `_lazy` suffix (e.g., `debug_lazy`)

### Error Handling
- Handler failures don't crash the application
- File handler logs errors to stderr on write failure
- Null handler provided for testing/disabling output

## Common Tasks

### Adding a New Handler
1. Create a new file in `src/gleam_log/handler/`
2. Implement formatting using `formatter.Formatter` type
3. Use `handler.new()` to create the handler
4. Add tests in `test/gleam_log_test.gleam`

### Adding a New Log Level
1. Add variant to `Level` type in `level.gleam`
2. Update `to_int`, `from_string`, `to_string`, `to_string_lowercase`
3. Add convenience functions in `logger.gleam` and `gleam_log.gleam`
4. Add color mapping in `console.gleam`
5. Add tests

### Modifying FFI
When changing platform-specific code:
1. Update both `gleam_log_ffi.erl` AND `gleam_log_ffi.mjs`
2. Ensure behavior is consistent across platforms
3. Test on both Erlang and JavaScript targets

## Tool Versions

Specified in `.tool-versions`:
- Erlang: 27.2.1
- Gleam: 1.14.0
- just: 1.38.0

## Known Limitations

1. **No code coverage**: Gleam compiles to Erlang source (not abstract format), making `cover` integration challenging
2. **Scoped context on JS**: JavaScript lacks Erlang's process dictionary; scoped context requires explicit passing or AsyncLocalStorage (Node.js only)
3. **File handler rotation**: Only size-based rotation is implemented; time-based rotation is a future feature (see PRD Phase 3)

## References

- [Product Requirements Document](docs/PRD.md) - Detailed requirements and design decisions
- [README.md](README.md) - User-facing documentation with examples
- [Gleam Documentation](https://hexdocs.pm/gleam_log/) - API documentation (after publishing)
