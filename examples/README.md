# birch Examples

This directory contains comprehensive examples demonstrating birch logging library features. Examples are organized into progressive groups, from fundamentals to advanced features.

## Running Examples

Each example is a self-contained Gleam project. To run an example:

```bash
cd examples/01-quick-start
gleam deps download
gleam run                         # Run on Erlang/BEAM
gleam run --target javascript     # Run on JavaScript
```

To run tests:

```bash
gleam test                        # Test on Erlang/BEAM
gleam test --target javascript    # Test on JavaScript
```

## Example Index

### Group 1: Fundamentals

| # | Example | Description |
|---|---------|-------------|
| 01 | [quick-start](./01-quick-start/) | Zero-config logging with `log.info()`, `log.debug()`, etc. |
| 02 | [log-levels](./02-log-levels/) | All six log levels, level filtering, `level.from_string()` |
| 03 | [metadata](./03-metadata/) | Structured logging with `_m` variants and key-value pairs |
| 04 | [named-loggers](./04-named-loggers/) | Component loggers with `log.new()` and `logger_` functions |

### Group 2: Handlers

| # | Example | Description |
|---|---------|-------------|
| 05 | [console-handler](./05-console-handler/) | Console configuration, colors, stdout/stderr targets |
| 06 | [json-handler](./06-json-handler/) | JSON output with builder pattern and custom fields |
| 07 | [file-handler](./07-file-handler/) | File output with size and time-based rotation |
| 08 | [custom-handlers](./08-custom-handlers/) | Implementing custom handlers with `handler.new()` |

### Group 3: Configuration & Performance

| # | Example | Description |
|---|---------|-------------|
| 09 | [global-config](./09-global-config/) | Application-wide configuration with `configure()` |
| 10 | [lazy-evaluation](./10-lazy-evaluation/) | Performance optimization with `debug_lazy()`, `info_lazy()` |
| 11 | [error-helpers](./11-error-helpers/) | Convenient error logging with `error_result()`, `fatal_result()` |

### Group 4: Advanced Features

| # | Example | Description |
|---|---------|-------------|
| 12 | [scoped-context](./12-scoped-context/) | Request-scoped metadata with `with_scope()` |
| 13 | [async-handler](./13-async-handler/) | Non-blocking logging with async handlers |
| 14 | [sampling](./14-sampling/) | Probabilistic sampling and rate limiting |
| 15 | [testing-support](./15-testing-support/) | Time providers, caller ID capture, null handler |

### Group 5: Platform-Specific

| # | Example | Description | Targets |
|---|---------|-------------|---------|
| 16 | [erlang-logger](./16-erlang-logger/) | OTP logger integration | BEAM only |
| 17 | [handler-errors](./17-handler-errors/) | Error callbacks and graceful degradation | BEAM + JS |

## Platform Support

Most examples work on both Erlang/BEAM and JavaScript targets. Platform-specific features are noted in the individual example READMEs.

| Target | Notes |
|--------|-------|
| **Erlang/BEAM** | Full feature support |
| **JavaScript (Node.js)** | Full support including scoped context via AsyncLocalStorage |
| **JavaScript (Deno/Bun)** | Most features; scoped context may not propagate to async operations |
| **JavaScript (Browser)** | Basic logging; file handler not available |

## Learning Path

If you're new to birch, we recommend going through the examples in order:

1. Start with **01-quick-start** to see birch in action
2. Learn about **02-log-levels** to understand filtering
3. Add structure with **03-metadata**
4. Organize with **04-named-loggers**
5. Then explore handlers and advanced features as needed

## Contributing

Each example should:
- Be self-contained with its own `gleam.toml`
- Include a `README.md` explaining what it demonstrates
- Include tests that verify the example works
- Work on both BEAM and JavaScript targets (unless platform-specific)
