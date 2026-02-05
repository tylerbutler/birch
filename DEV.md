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

## Code Coverage

This project has code coverage for both Erlang and JavaScript targets.

### Local Coverage

```bash
just coverage              # Run coverage on both Erlang and JavaScript
just coverage-js           # JavaScript only (uses c8/V8 native coverage)
just coverage-erlang       # Erlang only (uses Erlang's cover tool)
just coverage-js-report    # Generate HTML report (after coverage-js)
```

### How It Works

**JavaScript (c8)**: Uses V8's native code coverage via [c8](https://github.com/bcoe/c8). Runs gleeunit tests and integration tests, outputs to `coverage/lcov.info`.

**Erlang (cover)**: Uses a custom escript (`scripts/gleam_cover.escript`) that:
1. Compiles BEAM files with Erlang's [cover](https://www.erlang.org/doc/apps/tools/cover.html) tool
2. Runs tests via EUnit (not gleeunit's `main()` which calls `halt()`)
3. Collects per-module line coverage
4. Optionally exports to LCOV format (`--lcov` flag)

> [!NOTE]
> Erlang coverage line numbers refer to the **generated Erlang code** in `build/dev/erlang/birch/_gleam_artefacts/*.erl`, not the original Gleam source files.

### Why No Hosted Coverage (Codecov/Coveralls)?

Hosted coverage services like Codecov and Coveralls don't work well with Gleam because:

1. **Path mismatch**: Coverage reports contain paths to compiled artifacts (`build/dev/javascript/*.mjs`, `build/dev/erlang/*.erl`), not Gleam source files
2. **Files not in repo**: The compiled files don't exist in the repository
3. **Line number mismatch**: Line numbers in compiled Erlang/JS code don't correspond to Gleam source lines

This is a fundamental limitation of transpiled languages. Elixir works better with these tools because it has tighter BEAM runtime integration, while Gleam compiles via intermediate Erlang source.

**Use local coverage instead** - `just coverage` provides accurate module-level percentages.

### Reusing for Other Gleam Projects

The `scripts/gleam_cover.escript` is generic and can be copied to any Gleam project:

1. Copy `scripts/gleam_cover.escript` to your project
2. Add to your justfile:
   ```just
   coverage-erlang: build
       escript scripts/gleam_cover.escript

   coverage-erlang-lcov: build
       escript scripts/gleam_cover.escript --lcov
   ```
3. The script auto-detects the project name from `gleam.toml` and finds all `*_test` modules

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
