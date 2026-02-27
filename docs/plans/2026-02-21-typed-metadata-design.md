# Typed Metadata Values

**Issue:** [#58](https://github.com/tylerbutler/birch/issues/58) — Metadata values are stringly-typed
**Date:** 2026-02-21
**Status:** Approved

## Problem

Metadata is typed as `List(#(String, String))`, forcing manual conversions at every call site:

```gleam
birch.debug_m("Found files", [#("count", int.to_string(list.length(files)))])
```

This is verbose and loses type fidelity in JSON output — numbers and booleans become strings.

## Design

### MetadataValue sum type

Replace the stringly-typed metadata with a four-variant sum type in `record.gleam`:

```gleam
pub type MetadataValue {
  StringVal(String)
  IntVal(Int)
  FloatVal(Float)
  BoolVal(Bool)
}

pub type Metadata = List(#(String, MetadataValue))
```

Four types cover structured logging needs. No `ListVal`/`MapVal` (YAGNI). No `AnyVal(String)` — it would be a branded string with no consumer that distinguishes it from `StringVal`.

### New module: `birch/meta`

Ergonomic constructors following the `gleam/json` pattern:

```gleam
pub fn string(key: String, value: String) -> #(String, MetadataValue)
pub fn int(key: String, value: Int) -> #(String, MetadataValue)
pub fn float(key: String, value: Float) -> #(String, MetadataValue)
pub fn bool(key: String, value: Bool) -> #(String, MetadataValue)
```

### Call-site comparison

Before:

```gleam
logger.info(lgr, "Request processed", [
  #("request_id", request_id),
  #("status", int.to_string(200)),
  #("duration_ms", float.to_string(42.5)),
  #("cached", "true"),
])
```

After:

```gleam
import birch/meta

logger.info(lgr, "Request processed", [
  meta.string("request_id", request_id),
  meta.int("status", 200),
  meta.float("duration_ms", 42.5),
  meta.bool("cached", True),
])
```

### Handler impact

**JSON handler** outputs proper types:

```json
{"status": 200, "cached": true, "duration_ms": 42.5}
```

instead of:

```json
{"status": "200", "cached": "true", "duration_ms": "42.5"}
```

**Console/human-readable formatters** use a `metadata_value_to_string` helper. Visual output unchanged.

### Erlang logger FFI mapping

`birch_logger_formatter.erl:format_metadata` maps OTP `term()` values:

- `is_integer` → `IntVal`
- `is_float` → `FloatVal`
- `true`/`false` atoms → `BoolVal`
- Everything else → `StringVal` (stringified, same as today)

### `get_metadata` return type

Changes from `Result(String, Nil)` to `Result(MetadataValue, Nil)`.

## Blast radius

~15 function signatures across: `birch.gleam`, `logger.gleam`, `record.gleam`, `config.gleam`, `scope.gleam`, `formatter.gleam`, `console.gleam`, `json.gleam`, `erlang_logger.gleam`, both FFI files, and all 17 examples.

## Decisions

- **Breaking change acceptable** — birch is pre-1.0
- **No AnyVal** — branded string with no behavioral difference from StringVal
- **No ListVal/MapVal** — recursive encoding complexity for an edge case
- **Separate meta module** — follows gleam/json patterns, keeps birch.gleam from growing
