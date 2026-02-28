# Codebase Structure

**Analysis Date:** 2026-02-27

## Directory Layout

```
birch/
├── src/
│   ├── birch.gleam                       # Main public API - re-exports and convenience functions
│   ├── birch_ffi.erl                     # Erlang platform FFI (TTY, async, compression, safe call, scope)
│   ├── birch_ffi.mjs                     # JavaScript platform FFI (same as above)
│   ├── birch_erlang_logger_ffi.erl       # OTP :logger integration FFI (Erlang only)
│   ├── birch_erlang_logger_ffi.mjs       # OTP :logger integration FFI (JavaScript stub)
│   └── birch/
│       ├── config.gleam                  # Global configuration types and persistent storage
│       ├── erlang_logger.gleam           # OTP :logger integration (Erlang-only module)
│       ├── formatter.gleam               # Log formatting (human_readable, simple)
│       ├── handler.gleam                 # Handler interface and error handling
│       ├── level.gleam                   # LogLevel type (RFC 5424 levels + Trace)
│       ├── level_formatter.gleam         # Level presentation (simple, label, colored)
│       ├── logger.gleam                  # Logger type and per-logger logging functions
│       ├── meta.gleam                    # Metadata constructors (ergonomic string/int/float/bool)
│       ├── record.gleam                  # LogRecord type (timestamp, level, message, metadata)
│       ├── sampling.gleam                # Sampling configuration and probability logic
│       ├── scope.gleam                   # Scoped context execution (request-scoped metadata)
│       ├── handler/
│       │   ├── async.gleam               # Async/buffered handler with background queue
│       │   ├── console.gleam             # Console output with colors and presentation styles
│       │   ├── file.gleam                # File output with size/time-based rotation
│       │   └── json.gleam                # JSON-formatted output
│       └── internal/                     # Private implementation details (not re-exported)
│           ├── ansi.gleam                # ANSI color/formatting codes
│           ├── async_actor.gleam         # Async queue management (Gleam OTP actor)
│           ├── platform.gleam            # Platform abstraction (FFI declarations)
│           ├── scoped_logger.gleam       # Scoped logger override (process dict/AsyncLocalStorage)
│           └── time.gleam                # Time utilities (ISO 8601 formatting)
│
├── test/
│   ├── birch_test.gleam                  # Unit tests (levels, records, formatters, loggers, handlers)
│   ├── birch_logger_test_ffi.erl         # Test FFI for Erlang
│   ├── birch_logger_test_ffi.mjs         # Test FFI for JavaScript
│   ├── erlang_logger_test.gleam          # OTP :logger integration tests (Erlang-only)
│   ├── property_test.gleam               # Property-based tests (qcheck)
│   ├── time_test.gleam                   # Time module tests
│   └── integration/
│       ├── fixtures/
│       │   ├── console_fixture.gleam     # Console handler integration test fixtures
│       │   ├── json_fixture.gleam        # JSON handler integration test fixtures
│       │   └── metadata_fixture.gleam    # Metadata and scoped context fixtures
│       └── run_js_tests.sh               # Node.js/Deno/Bun test runner
│
├── examples/                             # 17 runnable examples demonstrating features
│   ├── 01-quick-start/                   # Basic logging
│   ├── 02-log-levels/                    # Log level filtering
│   ├── 03-metadata/                      # Metadata attachment
│   ├── 04-named-loggers/                 # Logger creation and naming
│   ├── 05-console-handler/               # Console output styles
│   ├── 06-json-handler/                  # JSON-formatted output
│   ├── 07-file-handler/                  # File writing with rotation
│   ├── 08-custom-handlers/               # Creating custom handlers
│   ├── 09-global-config/                 # Global configuration
│   ├── 10-lazy-evaluation/               # Lazy message evaluation
│   ├── 11-error-helpers/                 # error_result/fatal_result convenience
│   ├── 12-scoped-context/                # Request-scoped metadata
│   ├── 13-async-handler/                 # Async/buffered output
│   ├── 14-sampling/                      # Probabilistic filtering
│   ├── 15-testing-support/               # Testing patterns
│   ├── 16-erlang-logger/                 # OTP :logger integration
│   └── 17-handler-errors/                # Error callback monitoring
│
├── demo/                                 # Interactive demo app
│   ├── src/
│   │   └── birch_demo_console.gleam      # Demo showing console presentation options
│   └── gleam.toml
│
├── docs/
│   ├── PRD.md                            # Product Requirements Document
│   ├── IMPLEMENTATION_PLAN.md            # Implementation phases
│   └── adr/                              # Architecture Decision Records
│
├── docs-site/                            # Astro + Starlight documentation site
│   ├── src/
│   │   └── content/                      # Markdown documentation
│   └── astro.config.mjs
│
├── .github/workflows/
│   └── ci.yml                            # GitHub Actions CI pipeline
│
├── gleam.toml                            # Gleam package manifest (version 0.3.0)
├── manifest.toml                         # Dependency lock file
├── justfile                              # Task runner (just)
├── CLAUDE.md                             # AI assistant guide
├── README.md                             # User-facing documentation
├── CHANGELOG.md                          # Release notes
└── .planning/
    └── codebase/                         # GSD codebase mapping documents
```

## Directory Purposes

**src/:**
- Contains all public library code
- Gleam modules are organized by functionality (logger, handlers, formatters, levels)
- FFI files (`birch_ffi.*`) bridge to Erlang/JavaScript runtime

**src/birch/:**
- Core public modules: logger, handler, record, level, config
- Formatters and handlers for output customization
- Metadata and scope utilities for ergonomic use

**src/birch/handler/:**
- Pluggable output handlers (console, file, JSON, async)
- Each handler is a module implementing formatter + write function pattern

**src/birch/internal/:**
- Marked as internal in gleam.toml, not exported to public API
- Platform abstractions, time utilities, scope context management
- ANSI color codes, async actor implementation

**test/:**
- Unit tests organized by module
- Integration tests in subdirectory with platform-specific fixtures
- Property tests for level ordering, metadata operations, formatters

**examples/:**
- 17 runnable examples from basic to advanced features
- Each example demonstrates a specific capability
- Can be tested via `just test-examples` in main directory

**demo/:**
- Standalone app for interactive console output demo
- Shows all presentation options and styling

**docs/ and docs-site/:**
- Product requirements, implementation plans, architecture records
- Astro/Starlight documentation deployed to ccl.tylerbutler.com (note: domain name in CLAUDE.md is outdated, actual deploy target is birch site)

## Key File Locations

**Entry Points:**
- `src/birch.gleam`: Main public API, module-level logging functions, global config
- `src/birch/logger.gleam`: Named logger creation and per-logger logging
- `src/birch/handler.gleam`: Handler interface and built-in null handler

**Configuration:**
- `src/birch/config.gleam`: GlobalConfig type, builder options, persistent storage
- `gleam.toml`: Package metadata, dependencies, internal module declarations
- `.tool-versions`: Tool versions (Erlang 27.2.1, Gleam 1.14.0, just 1.38.0)

**Core Logic:**
- `src/birch/logger.gleam`: Log emission, level checking, metadata merging
- `src/birch/record.gleam`: LogRecord construction and manipulation
- `src/birch/level.gleam`: Level ordering, parsing, string conversion
- `src/birch/scope.gleam`: Scoped context execution and retrieval

**Platform Integration:**
- `src/birch/internal/platform.gleam`: FFI declarations (all platforms)
- `src/birch_ffi.erl`: Erlang implementation (TTY, async, compression, scope)
- `src/birch_ffi.mjs`: JavaScript implementation (Node.js, Deno, Bun)
- `src/birch/erlang_logger.gleam`: OTP :logger integration

**Output Handlers:**
- `src/birch/handler/console.gleam`: Console output (simple and fancy styles)
- `src/birch/handler/file.gleam`: File output with rotation strategies
- `src/birch/handler/json.gleam`: JSON-formatted output
- `src/birch/handler/async.gleam`: Buffered async output

**Formatting:**
- `src/birch/formatter.gleam`: human_readable and simple formatters
- `src/birch/level_formatter.gleam`: Level presentation (simple, label, colored)
- `src/birch/internal/ansi.gleam`: ANSI codes for colors and styling

**Testing:**
- `test/birch_test.gleam`: Comprehensive unit tests
- `test/property_test.gleam`: Property-based tests
- `test/erlang_logger_test.gleam`: OTP integration tests
- `test/integration/fixtures/*.gleam`: JavaScript runtime fixtures

## Naming Conventions

**Files:**
- Gleam modules: `lowercase_with_underscores.gleam`
- FFI files: `birch_ffi.erl`, `birch_ffi.mjs` (named after function module path)
- Opaque types: No suffix convention (e.g., `Logger`, `Handler`)
- Public records: CamelCase (e.g., `LogRecord`, `GlobalConfig`, `ConsoleConfig`)
- Test files: `module_test.gleam` or `module_spec.gleam`

**Modules:**
- Root: `birch` (main entry point)
- Submodules: `birch/logger`, `birch/handler`, `birch/config` (lowercase, slash separated)
- Handlers: `birch/handler/console`, `birch/handler/file` (descriptive names)
- Internal: `birch/internal/platform`, `birch/internal/ansi` (private implementation)

**Types:**
- Logger: `Logger` opaque type, functions like `new()`, `with_level()`, `trace()`, `debug()`
- Handlers: Named by output type: `ConsoleConfig`, `FileConfig`, handler constructors
- Config: `GlobalConfig`, `SampleConfig`, `ConfigOption` opaque
- Levels: `Level` type with variants `Trace`, `Debug`, `Info`, `Notice`, `Warn`, `Err`, `Critical`, `Alert`, `Fatal`
- Records: `LogRecord` for events, `Metadata` as `List(#(String, MetadataValue))`

**Functions:**
- Module functions: All lowercase snake_case (e.g., `human_readable()`, `with_handler()`)
- Builder methods: Start with `with_*` (e.g., `with_context()`, `with_level()`)
- Getters: Start with `get_*` or bare name (e.g., `get_handlers()` or `name()`)
- Convenience: Bare level names (e.g., `info()`, `error()`) for simple logging
- Metadata constructors: Bare type names (e.g., `meta.string()`, `meta.int()`)
- Lazy variants: Suffix `_lazy` (e.g., `debug_lazy()`)
- Error variants: Suffix `_result` (e.g., `error_result()`)
- Custom formatters: Suffix `_formatter` (e.g., `simple_formatter()`, `label_formatter()`)

## Where to Add New Code

**New Handler:**
1. Create `src/birch/handler/myhandler.gleam`
2. Implement `config()` function returning handler config type
3. Implement `handler()` and/or `handler_with_formatter()` functions
4. Use `handler.new()` to wrap formatter + write function
5. Add tests to `test/birch_test.gleam` in handler section
6. Add example to `examples/XX-myhandler/`

**New Formatter:**
1. Create function in `src/birch/formatter.gleam` or dedicated module
2. Signature: `fn(LogRecord) -> String`
3. Can compose with level_formatter for styled output
4. Test by adding to formatter tests in `test/birch_test.gleam`

**New Configuration Option:**
1. Add variant to `ConfigOption` opaque type in `src/birch/config.gleam`
2. Add builder function (e.g., `pub fn option_name(value) -> ConfigOption { ... }`)
3. Add handling in `apply_options()` function
4. Export from `src/birch.gleam` as `config_option_name()`
5. Document in CLAUDE.md and README.md

**New Log Level:**
1. Add variant to `Level` type in `src/birch/level.gleam`
2. Update `to_int()` with numeric value (must maintain ordering)
3. Update `from_string()` for case-insensitive parsing
4. Update `to_string()` for uppercase display
5. Update `to_string_lowercase()` if needed
6. Add color mapping in `src/birch/handler/console.gleam` if colored output wanted
7. Add convenience function in `src/birch/logger.gleam` (e.g., `my_level()`)
8. Add re-export in `src/birch.gleam`
9. Update tests in `test/birch_test.gleam` and `test/property_test.gleam`

**Cross-Platform Feature:**
1. Add Gleam code with conditional `@target` attributes if behavior differs
2. Add FFI declaration in `src/birch/internal/platform.gleam`
3. Implement `birch_ffi.erl` with Erlang code
4. Implement `birch_ffi.mjs` with JavaScript code (Node.js/Deno/Bun compatible)
5. Test on both targets: `just test-erlang` and `just test-js`

**Test Coverage:**
- Unit tests: `test/birch_test.gleam` (organized by module being tested)
- Property tests: `test/property_test.gleam` (qcheck for invariants)
- Integration tests: `test/integration/fixtures/*.gleam` (compiled, run on actual runtimes)
- Add example: `examples/XX-feature-name/src/birch_example_XX_feature_name.gleam`

## Special Directories

**build/:**
- Purpose: Gleam build artifacts (compiled Erlang and JavaScript)
- Generated: Yes (via `gleam build`)
- Committed: No (.gitignore)

**coverage/:**
- Purpose: Code coverage reports from `just coverage`
- Generated: Yes (via `gleam test --coverage`, c8 for JS)
- Committed: No (.gitignore)

**node_modules/:**
- Purpose: npm dependencies (Node.js test runner, c8, other dev tools)
- Generated: Yes (via `npm install` / `pnpm install`)
- Committed: No (.gitignore)

**.planning/codebase/:**
- Purpose: GSD (Get Shit Done) codebase mapping documents
- Generated: Yes (via `/gsd:map-codebase` command)
- Committed: Yes (reference documents for other GSD commands)

**.changes/:**
- Purpose: Unreleased changelog entries (changie format)
- Generated: Yes (via `changie new` on PR)
- Committed: Yes (.changes/unreleased/ for pending entries)

---

*Structure analysis: 2026-02-27*
