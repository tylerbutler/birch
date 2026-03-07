---
name: gleam-patterns
description: Gleam coding patterns and idioms used in this project. Reference for consistent code style.
user-invocable: false
---

# Gleam Patterns for birch

Background knowledge for writing idiomatic Gleam code in this project.

## Result Handling

Use `Result(value, error)` for fallible operations:

```gleam
// Good: Use result.map, result.try for chaining
pub fn parse_level(s: String) -> Result(Level, Nil) {
  s
  |> string.lowercase
  |> level.from_string
}

// Good: Pattern match for control flow
case level.from_string(input) {
  Ok(lvl) -> configure_level(lvl)
  Error(Nil) -> use_default()
}
```

## Pipe Operator

Use `|>` for readable data transformation:

```gleam
// Good: Clear data flow
metadata
|> list.filter(fn(pair) { !string.starts_with(pair.0, "_") })
|> list.map(format_pair)
|> string.join(" ")

// Avoid: Nested function calls
string.join(list.map(list.filter(metadata, ...), ...), " ")
```

## Type Aliases

Re-export types for API convenience:

```gleam
// In birch.gleam - re-export for users
pub type LogLevel = Level
pub type LogHandler = Handler
pub type LogMetadata = Metadata
```

## Naming Conventions

| Pattern | Example | Use For |
|---------|---------|---------|
| `with_*` | `with_level`, `with_context` | Builder methods that return modified copy |
| `*_m` | `info_m`, `debug_m` | Variants that take metadata |
| `*_lazy` | `debug_lazy` | Lazy evaluation variants |
| `logger_*` | `logger_info` | Logger-specific functions (vs module-level) |
| `Err` | `level.Err` | Error level (avoids Result.Error conflict) |

## Handler Pattern

All handlers follow this structure:

```gleam
pub fn handler() -> Handler {
  handler.new(
    name: "handler-name",
    write: write_fn,
    format: format_fn,
  )
}

fn write_fn(message: String) -> Nil { ... }
fn format_fn(record: LogRecord) -> String { ... }
```

## Configuration Pattern

Use option lists for configuration:

```gleam
// Define options as a type
pub type ConfigOption {
  Level(Level)
  Handlers(List(Handler))
  Context(Metadata)
}

// Apply with fold
pub fn apply_options(config: Config, options: List(ConfigOption)) -> Config {
  list.fold(options, config, apply_option)
}
```

## Cross-Platform FFI

When adding FFI functions:

1. Declare in `src/birch/internal/platform.gleam`:
   ```gleam
   @external(erlang, "birch_ffi", "my_function")
   @external(javascript, "../birch_ffi.mjs", "my_function")
   pub fn my_function() -> ReturnType
   ```

2. Implement in both:
   - `src/birch_ffi.erl` (Erlang)
   - `src/birch_ffi.mjs` (JavaScript)

3. Test on both targets: `just test`

## Testing Pattern

Tests use gleeunit with `should` assertions:

```gleam
pub fn feature_works_test() {
  let result = some_function()

  result
  |> should.equal(expected_value)
}

pub fn feature_handles_edge_case_test() {
  some_function("")
  |> should.equal(Error(Nil))
}
```

## Metadata

Always use `List(#(String, String))` for metadata:

```gleam
// Keys and values are both strings for cross-platform compatibility
let metadata = [#("user_id", "123"), #("action", "login")]
```

## Imports

Order imports consistently:

```gleam
// 1. Project imports (birch/*)
import birch/level
import birch/record

// 2. Standard library
import gleam/list
import gleam/string

// 3. External dependencies
import simplifile
```
