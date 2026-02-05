# ADR-001: Use PRNG for Log Sampling

## Status

Accepted

## Context

The birch logging library includes probabilistic sampling to reduce log volume in high-throughput scenarios. When sampling is configured (e.g., "log 10% of debug messages"), the library must generate random numbers to decide whether each log message should be emitted.

There are two main approaches for random number generation:

1. **PRNG (Pseudo-Random Number Generator)** - Fast, deterministic algorithms like `Math.random()` (JavaScript) or `rand:uniform()` (Erlang)
2. **CSPRNG (Cryptographically Secure PRNG)** - Slower, unpredictable algorithms like `crypto.getRandomValues()` (JavaScript) or `crypto:strong_rand_bytes()` (Erlang)

## Decision

We use the standard library's PRNG via `gleam/float.random()`, which maps to:
- **Erlang**: `rand:uniform/0`
- **JavaScript**: `Math.random()`

## Rationale

### Security is not a concern

Log sampling decides "should I emit this debug message?" - not security-sensitive operations. There is no attack vector from predicting which log messages get sampled.

Security-sensitive operations that require CSPRNG include:
- Session tokens, API keys, passwords
- Cryptographic nonces
- Any value where predictability enables exploitation

Log sampling has none of these properties.

### Performance matters

Sampling logic runs on every log call that passes the level filter. In high-throughput scenarios (the primary use case for sampling), this can be millions of calls per second.

| Operation | Relative Speed |
|-----------|---------------|
| `Math.random()` | ~1x (baseline) |
| `crypto.getRandomValues()` | ~10-50x slower |

The latency difference is meaningful in a logging hot path.

### Statistical uniformity is sufficient

For "10% sampling", we need approximately 1 in 10 messages logged over time. PRNG provides adequate statistical uniformity for this use case. We don't need cryptographic unpredictability.

### Industry precedent

All major logging libraries use PRNG for sampling:
- Java: Log4j2, Logback use `ThreadLocalRandom`
- Node.js: Winston, Bunyan use `Math.random()`
- Go: `math/rand` for sampling decisions
- Python: `random` module for log sampling

## Consequences

### Positive

- Fast sampling decisions with minimal overhead
- No additional dependencies beyond stdlib
- Cross-platform consistency (both targets use their standard PRNG)

### Negative

- Sampling patterns are theoretically predictable (not a practical concern)
- Cannot use birch sampling for security-sensitive decisions (not intended use case)

### Neutral

- Developers familiar with other logging libraries will find this behavior expected

## References

- [MDN: Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Erlang rand module](https://www.erlang.org/doc/man/rand.html)
- [gleam/float.random()](https://hexdocs.pm/gleam_stdlib/gleam/float.html#random)
