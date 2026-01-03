# Development Guide

This document covers development setup, testing, and guidelines for contributors.

## Prerequisites

Ensure you have the correct tool versions installed (see `.tool-versions`):
- Erlang: 27.2.1
- Gleam: 1.14.0
- just: 1.38.0

## Commands

This project uses [just](https://just.systems/) as a task runner. Run `just` to see all available commands.

```bash
just deps         # Download dependencies
just build        # Build for Erlang target (alias: b)
just build-js     # Build for JavaScript target
just build-all    # Build for both targets
just test         # Run tests on both targets (alias: t)
just test-erlang  # Run tests on Erlang only
just test-js      # Run tests on JavaScript only
just format       # Format source code (alias: f)
just format-check # Check formatting without modifying
just check        # Run format-check + tests on both targets (alias: c)
just check-quick  # Run format-check + Erlang tests only
just docs         # Generate documentation (alias: d)
just watch        # Watch and rebuild on changes (requires watchexec)
just watch-test   # Watch and run tests on changes
```

> [!NOTE]
> You can also use `gleam` commands directly (e.g., `gleam build`, `gleam test --target javascript`).

## Project Structure

```
src/
├── birch.gleam                  # Main public API
├── birch/
│   ├── level.gleam              # LogLevel type
│   ├── record.gleam             # LogRecord type and Metadata
│   ├── logger.gleam             # Logger type with handlers and context
│   ├── handler.gleam            # Handler interface
│   ├── formatter.gleam          # Format functions
│   ├── config.gleam             # Global configuration
│   ├── sampling.gleam           # Sampling and rate limiting
│   ├── scope.gleam              # Scoped context
│   ├── handler/
│   │   ├── console.gleam        # Console output with colors
│   │   ├── json.gleam           # JSON-formatted output
│   │   ├── file.gleam           # File output with rotation
│   │   └── async.gleam          # Async (non-blocking) handler
│   └── internal/                # Internal modules (not public API)
├── birch_ffi.erl                # Erlang FFI implementation
└── birch_ffi.mjs                # JavaScript FFI implementation

test/
├── birch_test.gleam             # Unit tests
└── property_test.gleam          # Property-based tests (qcheck)
```

## Cross-Platform FFI

When modifying platform-specific code:

1. Update both `birch_ffi.erl` AND `birch_ffi.mjs`
2. Ensure behavior is consistent across platforms
3. Test on both Erlang and JavaScript targets

## Adding a New Handler

1. Create a new file in `src/birch/handler/`
2. Implement formatting using `formatter.Formatter` type
3. Use `handler.new()` to create the handler
4. Add tests in `test/birch_test.gleam`
5. Document with module-level and function doc comments

## Code Style

- Follow Gleam's built-in formatter (`just format`)
- Use doc comments (`///`) for all public functions and types
- Use `Err` instead of `Error` for the error log level (avoids Result conflict)
