# Copilot Instructions for birch

birch is a logging library for Gleam with cross-platform support (Erlang BEAM and JavaScript targets). Version 0.3.0, pre-1.0 — the API is still evolving.

## Build, Test, and Lint

This project uses [just](https://just.systems/) as a task runner. Run `just` to list all commands.

```bash
just build              # Build for Erlang (alias: b)
just build-js           # Build for JavaScript
just build-all          # Build for both targets

just test               # Run tests on both targets (alias: t)
just test-erlang        # Erlang tests only
just test-js            # JavaScript tests only

just format             # Format source code (alias: f)
just format-check       # Check formatting without modifying

just check              # Format-check + tests on both targets (alias: c)
just check-quick        # Format-check + Erlang tests only (faster iteration)
```

Gleam has no built-in single-test-function runner. To run tests for a specific module:

```bash
gleam test                        # All Erlang tests
gleam test --target javascript    # All JS tests
```

### Integration Tests and Examples

```bash
just test-integration       # JS integration tests (Node.js)
just test-integration-all   # JS integration tests on Node.js, Deno, and Bun

just test-examples          # Run all 17 examples on Erlang
just test-examples-all      # Run all examples on Erlang + Node.js
just test-example-erlang 01-quick-start  # Run a specific example
```

## Architecture

### Dual-Target Design

Every feature must work on both Erlang and JavaScript. Platform-specific operations are declared in `src/birch/internal/platform.gleam` using `@external` annotations pointing to two FFI implementations:

- `src/birch_ffi.erl` — Erlang implementation
- `src/birch_ffi.mjs` — JavaScript implementation (supports Node.js, Deno, Bun, and browser)

**When modifying FFI code, always update both files and test on both targets.** Behavioral consistency across platforms is critical.

### Module Layout

- `src/birch.gleam` — Main public API with re-exports and convenience functions
- `src/birch/logger.gleam` — Core `Logger` type (immutable, configured via pipe chaining)
- `src/birch/handler.gleam` — Handler interface: `handler.new(name:, write:, format:)`
- `src/birch/level.gleam` — Log levels: Trace(0) < Debug(1) < Info(2) < Warn(3) < Err(4) < Fatal(5)
- `src/birch/handler/` — Built-in handlers: `console`, `file`, `json`, `async`
- `src/birch/internal/` — Internal modules, not part of the public API

### Handler Pattern

All handlers follow the same pattern — create via `handler.new()` with a name, a write function, and a format function:

```gleam
handler.new(
  name: "custom",
  write: fn(message: String) -> Nil { io.println(message) },
  format: fn(record: LogRecord) -> String { formatter.human_readable(record) },
)
```

### Logger Configuration

Loggers are immutable values configured via pipe chaining:

```gleam
let logger = log.new("myapp.database")
  |> log.with_level(level.Debug)
  |> log.with_context([#("service", "db")])
  |> log.with_handler(json.handler())
```

## Key Conventions

### Naming

- **`Err` not `Error`** for the error log level — avoids conflict with Gleam's `Result.Error` constructor
- **`_m` suffix** for metadata variants (e.g., `info_m` takes metadata, `info` does not)
- **`_lazy` suffix** for lazy evaluation variants (e.g., `debug_lazy` takes a callback)
- **`logger_` prefix** for logger-specific functions (e.g., `logger_info` vs module-level `info`)
- **`with_*`** for builder/configuration methods
- Logger names use **dot notation** for hierarchy (e.g., `"myapp.database"`)

### Metadata

Always `List(#(String, String))` — both keys and values are strings for cross-platform compatibility.

### Internal Modules

Modules under `birch/internal/` and some others (`birch/sampling`, `birch/handler/async`) are marked internal in `gleam.toml` and are not part of the public API.

### Error Handling

Handler failures must not crash the application. The file handler logs errors to stderr on write failure. A null handler is provided for testing.

## Commit and Changelog Conventions

### Commits

Uses [conventional commits](https://www.conventionalcommits.org/) enforced by commitlint:

```
feat: add new handler type
fix: correct timestamp format on Deno
```

Valid types: `feat`, `fix`, `perf`, `refactor`, `docs`, `style`, `test`, `build`, `ci`, `chore`, `revert`. Subject must be lowercase, max 100 characters, no trailing period.

### Changelog

Uses [changie](https://changie.dev/) — **do not edit `CHANGELOG.md` directly**. Create fragment files:

```bash
changie new  # Interactive prompt for kind and body
```

Or create YAML fragments manually in `.changes/unreleased/`. Available kinds: `Added`, `Changed`, `Deprecated`, `Fixed`, `Performance`, `Removed`, `Reverted`, `Dependencies`, `Security`.

## Tool Versions

Specified in `.tool-versions` (source of truth):

- Erlang: 27.2.1
- Gleam: 1.14.0
- just: 1.38.0
