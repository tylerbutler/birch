# 16 Erlang Logger Integration

This example demonstrates integrating birch with Erlang's built-in `:logger` system.

**Platform: BEAM only** - This example uses Erlang-specific features.

## What You'll Learn

- Forwarding birch logs to Erlang's `:logger`
- Installing birch as an Erlang logger handler
- Level mapping between birch and Erlang

## Running the Example

```bash
gleam run   # Erlang/BEAM only
```

## Forwarding to Erlang Logger

Use `forward_to_logger()` to send birch logs to Erlang's logger:

```gleam
import birch as log
import birch/erlang_logger

// Create a handler that forwards to Erlang :logger
let handler = erlang_logger.forward_to_logger()

log.configure([
  log.config_handlers([handler]),
])

// Logs now go through Erlang's logger system
log.info("This goes to Erlang logger")
```

## Installing Birch as Logger Handler

You can also install birch to receive logs from Erlang's logger:

```gleam
// Install birch as a handler for Erlang :logger
erlang_logger.install_logger_handler()

// Now Erlang :logger messages are handled by birch
// (from other Erlang libraries, OTP, etc.)

// Uninstall when done
erlang_logger.uninstall_logger_handler()
```

## Level Mapping

Birch levels map to Erlang logger levels:

| Birch   | Erlang    |
|---------|-----------|
| Trace   | debug     |
| Debug   | debug     |
| Info    | info      |
| Warn    | warning   |
| Err     | error     |
| Fatal   | emergency |

## Use Cases

### Integration with OTP Applications

When running in an OTP environment, you may want logs to go through
the standard Erlang logger for consistency:

```gleam
pub fn setup_for_otp() {
  // Forward all birch logs to Erlang logger
  log.configure([
    log.config_handlers([erlang_logger.forward_to_logger()]),
  ])
}
```

### Capturing OTP Logs in Birch

To have OTP and library logs appear in birch format:

```gleam
pub fn capture_otp_logs() {
  // Install birch as a logger handler
  erlang_logger.install_logger_handler()

  // OTP logs will now be formatted by birch
}
```

## Next Steps

- [09-global-config](../09-global-config/) - More configuration options
- [08-custom-handlers](../08-custom-handlers/) - Build custom handlers
