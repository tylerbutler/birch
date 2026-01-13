# 13 Async Handler

This example demonstrates non-blocking logging with async handlers.

## What You'll Learn

- Wrapping handlers for async operation
- Configuring queue size and overflow behavior
- Flushing before shutdown
- Platform differences (OTP actors vs microtasks)

## Running the Example

```bash
gleam run                         # Erlang/BEAM
gleam run --target javascript     # JavaScript
```

## Basic Usage

Wrap any handler to make it async:

```gleam
import birch/handler/async
import birch/handler/console

let async_console =
  console.handler()
  |> async.make_async(async.default_config())
```

## Configuration Options

```gleam
let config =
  async.config()
  |> async.with_queue_size(5000)        // Max pending messages
  |> async.with_flush_interval(50)       // Batch interval in ms
  |> async.with_overflow(async.Block)    // What to do when full

let handler = async.make_async(console.handler(), config)
```

## Overflow Behaviors

| Behavior | Description |
|----------|-------------|
| `DropOldest` | Drop oldest message to make room (default) |
| `DropNewest` | Drop the new message being added |
| `Block` | Wait until there's room (use with caution) |

## Flushing Before Shutdown

Always flush before application exit to ensure all logs are written:

```gleam
// Before shutdown
async.flush()
```

On Erlang, you can also shutdown all async handlers:

```gleam
@target(erlang)
async.shutdown_all()
```

## Platform Implementation

| Platform | Implementation |
|----------|----------------|
| Erlang/BEAM | OTP actor with Subject message passing |
| JavaScript | setTimeout/setImmediate batching |

## When to Use Async

**Use async handlers when:**
- Log I/O is slow (network, slow disk)
- You need to minimize latency in hot paths
- You're logging at very high volume

**Avoid async when:**
- You need guaranteed log ordering with other output
- You're debugging crashes (logs may be lost)
- Memory usage is critical (queue uses memory)

## Next Steps

- [07-file-handler](../07-file-handler/) - File output (good for async wrapping)
- [14-sampling](../14-sampling/) - Reduce log volume
