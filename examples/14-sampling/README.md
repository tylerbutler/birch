# 14 Sampling

This example demonstrates log sampling and rate limiting for high-volume scenarios.

## What You'll Learn

- Probabilistic sampling with `sampling.config()`
- Rate limiting with token buckets
- When to use each approach

## Running the Example

```bash
gleam run                         # Erlang/BEAM
gleam run --target javascript     # JavaScript
```

## Probabilistic Sampling

Sample a percentage of logs at certain levels:

```gleam
import birch as log
import birch/level
import birch/sampling

// Log only 10% of debug messages
log.configure([
  log.config_sampling(sampling.config(level.Debug, 0.1)),
])
```

### How It Works

- Logs **above** the specified level are always logged
- Logs **at or below** the level are sampled randomly
- Rate is 0.0 (never) to 1.0 (always)

Example with `sampling.config(level.Debug, 0.1)`:
- TRACE: 10% sampled
- DEBUG: 10% sampled
- INFO: Always logged
- WARN: Always logged
- ERROR: Always logged
- FATAL: Always logged

## Rate Limiting (Token Bucket)

For more precise control, use a token bucket:

```gleam
import birch/sampling

// Create a bucket: max 10 tokens, refills at 100 tokens/second
let bucket = sampling.new_token_bucket(10, 100)

// Try to consume a token for each log
let #(allowed, new_bucket) = sampling.try_consume(bucket)

case allowed {
  True -> // Log the message
  False -> // Rate limited, skip this log
}
```

### Token Bucket Parameters

| Parameter | Description |
|-----------|-------------|
| `max_tokens` | Burst capacity (max logs in a burst) |
| `refill_rate` | Tokens added per second (sustained rate) |

## When to Use Each

| Approach | Use When |
|----------|----------|
| Sampling | You want to reduce volume by a fixed percentage |
| Rate Limiting | You want to cap logs per second |

## Production Example

```gleam
// Development: log everything
sampling.config(level.Trace, 1.0)

// Production: sample debug, log info+
sampling.config(level.Debug, 0.01)  // 1% of debug
```

## Next Steps

- [10-lazy-evaluation](../10-lazy-evaluation/) - Avoid work for filtered logs
- [13-async-handler](../13-async-handler/) - Non-blocking writes
