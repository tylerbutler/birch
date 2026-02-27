# ADR-002: Typed Metadata Values via Sum Type

## Status

Accepted

## Context

Metadata in birch was originally typed as `List(#(String, String))` — both keys and values were strings. This had two problems:

1. **Verbose call sites.** Every non-string value required manual conversion: `#("count", int.to_string(42))`, `#("rate", float.to_string(0.95))`, `#("cached", "true")`.

2. **Lossy JSON output.** The JSON handler emitted all metadata values as JSON strings (`"count": "42"`) since it had no type information. Log aggregation systems couldn't filter or sort on numeric values.

### Why not a generic helper?

Gleam has no typeclasses, traits, or overloading. A generic `meta(key, value)` function using `string.inspect` would add quotes around string values (`"\"hello\""`) and produce debug representations rather than clean output. There's no way to dispatch on the value type at the call site.

### Why not accept both String and MetadataValue?

Gleam has no union types. A tuple position accepts exactly one type. You cannot write `#(String, String | MetadataValue)`. The only way to support multiple value types is a sum type (custom type with variants).

## Decision

Replace the stringly-typed metadata with a closed sum type:

```gleam
pub type MetadataValue {
  StringVal(String)
  IntVal(Int)
  FloatVal(Float)
  BoolVal(Bool)
}

pub type Metadata = List(#(String, MetadataValue))
```

Provide a `birch/meta` helper module with ergonomic constructors (`meta.string`, `meta.int`, `meta.float`, `meta.bool`) following the `gleam/json` pattern. Recommend `import birch/meta as m` for concise usage.

### Why four types?

String, Int, Float, and Bool cover the structured logging value space. We considered:

- **ListVal / MapVal**: Would require recursive JSON encoding and complicate every formatter. No real-world logging use case justified the complexity. YAGNI.
- **AnyVal(String)**: A "stringified from something else" variant. No handler would distinguish it from `StringVal` — it's a branded string with no consumer. Skipped.
- **NullVal**: For optional/missing values. Rare in logging metadata and expressible as omitting the key entirely. Skipped.

### Why a separate module?

The `birch` module is already large (~650 lines). Adding `string`, `int`, `float`, `bool` functions to it would create naming confusion (`birch.string` vs `gleam/string`). A dedicated `birch/meta` module mirrors how `gleam/json` provides `json.string`, `json.int`, etc.

## Consequences

**Positive:**
- JSON handler outputs proper types: `"count": 42`, `"active": true`
- Call sites are more concise for non-string values (no `int.to_string`)
- Erlang logger FFI preserves OTP value types instead of flattening to strings

**Negative:**
- String-only metadata is slightly more verbose: `m.string("key", "value")` vs the old `#("key", "value")`
- Breaking change to the `Metadata` type alias (acceptable pre-1.0)
- Every file that constructs metadata needs updating

**Neutral:**
- The `import birch/meta as m` convention mitigates the verbosity concern
- Raw constructors (`#("key", StringVal("value"))`) work if users prefer to avoid the helper module
