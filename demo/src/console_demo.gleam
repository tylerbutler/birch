//// Console Handler Demo
////
//// Demonstrates all the console handler presentation options.

import birch as log
import birch/handler/console
import birch/level
import birch/level_formatter
import birch/logger
import birch/record.{BoolVal, FloatVal, IntVal, StringVal}
import gleam/io

pub fn main() {
  // =========================================================================
  // Simple Style (pipe-delimited, traditional format)
  // =========================================================================
  print_header("SIMPLE STYLE (default)")

  log.configure([
    log.config_handlers([console.handler()]),
    log.config_level(level.Trace),
  ])

  log.trace("Trace level message")
  log.debug("Debug level message")
  log.info("Info level message")
  log.warn("Warning level message")
  log.error("Error level message")
  log.fatal("Fatal level message")

  // =========================================================================
  // Simple Style without timestamps
  // =========================================================================
  print_header("SIMPLE STYLE (no timestamps)")

  let simple_no_ts =
    console.ConsoleConfig(..console.default_config(), timestamps: False)
  log.configure([
    log.config_handlers([console.handler_with_config(simple_no_ts)]),
    log.config_level(level.Trace),
  ])

  log.trace("Trace level message")
  log.debug("Debug level message")
  log.info("Info level message")
  log.warn("Warning level message")
  log.error("Error level message")
  log.fatal("Fatal level message")

  // =========================================================================
  // Fancy Style (compact with icons)
  // =========================================================================
  print_header("FANCY STYLE (label formatter with icons)")

  log.configure([
    log.config_handlers([console.fancy_handler()]),
    log.config_level(level.Trace),
  ])

  log.trace("Trace level message")
  log.debug("Debug level message")
  log.info("Info level message")
  log.warn("Warning level message")
  log.error("Error level message")
  log.fatal("Fatal level message")

  // =========================================================================
  // Fancy Style with timestamps
  // =========================================================================
  print_header("FANCY STYLE (with timestamps)")

  let fancy_with_ts =
    console.ConsoleConfig(..console.default_fancy_config(), timestamps: True)
  log.configure([
    log.config_handlers([console.handler_with_config(fancy_with_ts)]),
    log.config_level(level.Trace),
  ])

  log.trace("Trace level message")
  log.debug("Debug level message")
  log.info("Info level message")
  log.warn("Warning level message")
  log.error("Error level message")
  log.fatal("Fatal level message")

  // =========================================================================
  // Badge Style
  // =========================================================================
  print_header("BADGE STYLE")

  let badge_config =
    console.ConsoleConfig(
      ..console.default_fancy_config(),
      level_formatter: level_formatter.badge_formatter(),
    )
  log.configure([
    log.config_handlers([console.handler_with_config(badge_config)]),
    log.config_level(level.Trace),
  ])

  log.trace("Trace with badge")
  log.debug("Debug with badge")
  log.info("Info with badge")
  log.warn("Warning with badge")
  log.error("Error with badge")
  log.fatal("Fatal with badge")

  // =========================================================================
  // Label Style without icons
  // =========================================================================
  print_header("LABEL STYLE (no icons)")

  let no_icons_config =
    console.ConsoleConfig(
      ..console.default_fancy_config(),
      level_formatter: level_formatter.label_formatter_with_config(
        level_formatter.LabelConfig(icons: False),
      ),
    )
  log.configure([
    log.config_handlers([console.handler_with_config(no_icons_config)]),
    log.config_level(level.Trace),
  ])

  log.trace("Trace level message")
  log.debug("Debug level message")
  log.info("Info level message")
  log.warn("Warning level message")
  log.error("Error level message")
  log.fatal("Fatal level message")

  // =========================================================================
  // With Metadata
  // =========================================================================
  print_header("WITH METADATA")

  log.configure([
    log.config_handlers([console.fancy_handler()]),
    log.config_level(level.Info),
  ])

  log.info_m("Request received", [
    #("method", StringVal("GET")),
    #("path", StringVal("/api/users")),
  ])
  log.warn_m("Slow query detected", [
    #("duration_ms", IntVal(1523)),
    #("table", StringVal("users")),
  ])
  log.error_m("Connection failed", [
    #("host", StringVal("db.example.com")),
    #("port", IntVal(5432)),
  ])

  // =========================================================================
  // Named Loggers
  // =========================================================================
  print_header("NAMED LOGGERS")

  let db_logger =
    logger.new("myapp.database")
    |> logger.with_handlers([console.fancy_handler()])
    |> logger.with_level(level.Debug)

  let http_logger =
    logger.new("myapp.http")
    |> logger.with_handlers([console.fancy_handler()])
    |> logger.with_level(level.Info)

  logger.info(db_logger, "Connected to database", [#("tls", BoolVal(True))])
  logger.debug(db_logger, "Executing query", [
    #("sql", StringVal("SELECT * FROM users")),
  ])
  logger.info(http_logger, "Server started", [#("port", IntVal(8080))])
  logger.warn(http_logger, "Rate limit exceeded", [
    #("client", StringVal("192.168.1.100")),
  ])

  // =========================================================================
  // Semantic Log Types
  // =========================================================================
  print_header("SEMANTIC LOG TYPES")

  console.start("Building project...", [#("target", StringVal("release"))])
  console.success("Build completed!", [#("duration_s", FloatVal(2.5))])
  console.ready("Server listening", [#("port", IntVal(3000))])
  console.fail("Could not connect to cache", [
    #("host", StringVal("redis.local")),
  ])

  // =========================================================================
  // Box Output
  // =========================================================================
  print_header("BOX OUTPUT")

  console.write_box("Hello, World!")

  console.write_box_with_title(
    "This is a message\nwith multiple lines\nof content",
    "Notice",
  )

  // =========================================================================
  // Grouping
  // =========================================================================
  print_header("GROUPING")

  // Each group gets a unique color based on its name hash
  console.with_group("Building components", fn() {
    let indented =
      logger.new("build")
      |> logger.with_handlers([console.indented_handler(1)])
      |> logger.with_level(level.Debug)

    logger.info(indented, "Compiling sources...", [])
    logger.info(indented, "Linking...", [])
    logger.info(indented, "Done!", [])
    Nil
  })

  console.with_group("Running tests", fn() {
    let indented =
      logger.new("test")
      |> logger.with_handlers([console.indented_handler(1)])
      |> logger.with_level(level.Debug)

    logger.info(indented, "Unit tests...", [])
    logger.info(indented, "Integration tests...", [])
    logger.info(indented, "All tests passed!", [])
    Nil
  })

  console.with_group("Deploying application", fn() {
    let indented =
      logger.new("deploy")
      |> logger.with_handlers([console.indented_handler(1)])
      |> logger.with_level(level.Debug)

    logger.info(indented, "Uploading artifacts...", [])
    logger.info(indented, "Updating configuration...", [])
    logger.info(indented, "Deployment complete!", [])
    Nil
  })

  console.with_group("Cleanup", fn() {
    let indented =
      logger.new("cleanup")
      |> logger.with_handlers([console.indented_handler(1)])
      |> logger.with_level(level.Debug)

    logger.info(indented, "Removing temp files...", [])
    logger.info(indented, "Done!", [])
    Nil
  })

  // =========================================================================
  // Custom Level Formatter
  // =========================================================================
  print_header("CUSTOM LEVEL FORMATTER")

  let emoji_formatter =
    level_formatter.custom_level_formatter(
      fn(lvl, _use_color) {
        case lvl {
          level.Trace -> "🔍"
          level.Debug -> "🐛"
          level.Info -> "💡"
          level.Notice -> "📋"
          level.Warn -> "⚠️ "
          level.Err -> "🔥"
          level.Critical -> "🔥"
          level.Alert -> "🚨"
          level.Fatal -> "💀"
        }
      },
      3,
    )
  // Width 3: "⚠️ " is 3 chars (emoji + space + space)

  let custom_config =
    console.ConsoleConfig(
      ..console.default_fancy_config(),
      level_formatter: emoji_formatter,
    )

  log.configure([
    log.config_handlers([console.handler_with_config(custom_config)]),
    log.config_level(level.Trace),
  ])

  log.trace("Trace with emoji")
  log.debug("Debug with emoji")
  log.info("Info with emoji")
  log.warn("Warning with emoji")
  log.error("Error with emoji")
  log.fatal("Fatal with emoji")

  // =========================================================================
  // No Colors (for piping/redirection)
  // =========================================================================
  print_header("NO COLORS")

  let no_color_config =
    console.ConsoleConfig(..console.default_fancy_config(), color: False)

  log.configure([
    log.config_handlers([console.handler_with_config(no_color_config)]),
    log.config_level(level.Trace),
  ])

  log.trace("Trace level message")
  log.debug("Debug level message")
  log.info("Info level message")
  log.warn("Warning level message")
  log.error("Error level message")
  log.fatal("Fatal level message")

  // =========================================================================
  // Auto-Indent from Scopes
  // =========================================================================
  print_header("AUTO-INDENT FROM SCOPES")

  let auto_indent_config =
    console.ConsoleConfig(
      ..console.default_fancy_config(),
      auto_indent_from_scopes: True,
    )

  log.configure([
    log.config_handlers([console.handler_with_config(auto_indent_config)]),
    log.config_level(level.Info),
  ])

  log.info("Outside any scope - no indentation")

  log.with_scope([#("request_id", StringVal("req-123"))], fn() {
    log.info("Level 1 scope - indented once")

    log.with_scope([#("step", StringVal("validation"))], fn() {
      log.info("Level 2 scope - indented twice")
      log.warn("Warnings are also indented at level 2")

      log.with_scope([#("substep", StringVal("schema_check"))], fn() {
        log.info("Level 3 scope - indented three times")
        Nil
      })

      log.info("Back to level 2")
      Nil
    })

    log.info("Back to level 1")
    Nil
  })

  log.info("Back outside scope - no indentation")

  // Reset
  log.reset_config()
  print_header("DEMO COMPLETE")
}

fn print_header(title: String) {
  let line = "═══════════════════════════════════════════════════════════════"
  io.println("")
  io.println(line)
  io.println(" " <> title)
  io.println(line)
}
