# 10 Lazy Evaluation

This example demonstrates lazy evaluation for performance optimization.

## What You'll Learn

- Using `debug_lazy()` and `info_lazy()`
- When to use lazy vs regular logging
- Performance benefits

## Running the Example

```bash
gleam run                         # Erlang/BEAM
gleam run --target javascript     # JavaScript
```

## The Problem

Regular logging always evaluates the message:

```gleam
// This ALWAYS calls expensive_operation(), even if debug is filtered!
log.debug("Result: " <> expensive_operation())
```

## The Solution: Lazy Evaluation

With lazy logging, the function is only called if the log will be emitted:

```gleam
// This only calls expensive_operation() if debug level is enabled
log.debug_lazy(fn() { "Result: " <> expensive_operation() })
```

## Available Lazy Functions

```gleam
log.debug_lazy(fn() { "..." })
log.info_lazy(fn() { "..." })
```

## When to Use Lazy Logging

**Use lazy logging when:**
- The message involves expensive string operations
- You're calling functions to build the message
- You're serializing objects for debugging
- The log level might be filtered out

**Use regular logging when:**
- The message is a simple string literal
- The message is already computed
- Performance isn't critical

## Example Comparison

```gleam
// Expensive: always serializes the list
log.debug("Users: " <> string.inspect(large_list))

// Efficient: only serializes if debug is enabled
log.debug_lazy(fn() { "Users: " <> string.inspect(large_list) })
```

## Next Steps

- [11-error-helpers](../11-error-helpers/) - Convenient error logging
- [14-sampling](../14-sampling/) - Reduce log volume
