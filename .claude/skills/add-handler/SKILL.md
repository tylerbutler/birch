---
name: add-handler
description: Scaffold a new birch logging handler
argument: handler name (e.g., "syslog", "kafka")
disable-model-invocation: true
---

# Add Handler Skill

Create a new handler in `src/birch/handler/` following the project pattern.

## Steps

1. **Create the handler file**: `src/birch/handler/{argument}.gleam`
   - Use `console.gleam` or `json.gleam` as a reference
   - Import required modules: `birch/handler`, `birch/record`, `birch/formatter`

2. **Implement the handler**:
   - Create a public `handler()` function that returns `handler.Handler`
   - Use `handler.new()` with:
     - `name`: A descriptive name (e.g., `"{argument}"`)
     - `write`: Output function (`fn(String) -> Nil`)
     - `format`: Formatting function using `formatter.Formatter` type

3. **Add tests**: In `test/birch_test.gleam`
   - Follow the existing test organization pattern
   - Add a new section: `// {Argument} handler tests`
   - Test the handler output format
   - Test edge cases (empty messages, special characters, metadata)

4. **Update exports** (if needed): Consider whether to re-export from `birch.gleam`

## Reference Files

- `src/birch/handler/console.gleam` - Simple console output with colors
- `src/birch/handler/json.gleam` - JSON formatted output
- `src/birch/handler/file.gleam` - File output with rotation
- `src/birch/handler.gleam` - Handler type and `new()` function

## Handler Template

```gleam
import birch/handler.{type Handler}
import birch/record.{type LogRecord}
import birch/formatter

/// Creates a new {argument} handler
pub fn handler() -> Handler {
  handler.new(
    name: "{argument}",
    write: write,
    format: format,
  )
}

fn write(message: String) -> Nil {
  // Output implementation
}

fn format(record: LogRecord) -> String {
  // Format implementation using formatter functions
  formatter.human_readable(record)
}
```
