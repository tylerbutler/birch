//// Console Handler Demo
////
//// Demonstrates all the console handler presentation options.

import birch
import birch/handler/console
import birch/level
import birch/level_formatter
import birch/log
import birch/record.{BoolVal, FloatVal, IntVal, StringVal}
import gleam/io

pub fn main() {
  // =========================================================================
  // Simple Style (pipe-delimited, traditional format)
  // =========================================================================
  print_header("SIMPLE STYLE (default)")

  birch.configure([
    birch.config_handlers([console.handler()]),
    birch.config_level(level.Trace),
  ])

  birch.trace("Trace level message")
  birch.debug("Debug level message")
  birch.info("Info level message")
  birch.warn("Warning level message")
  birch.error("Error level message")
  birch.fatal("Fatal level message")

  // =========================================================================
  // Simple Style without timestamps
  // =========================================================================
  print_header("SIMPLE STYLE (no timestamps)")

  let simple_no_ts =
    console.ConsoleConfig(..console.default_config(), timestamps: False)
  birch.configure([
    birch.config_handlers([console.handler_with_config(simple_no_ts)]),
    birch.config_level(level.Trace),
  ])

  birch.trace("Trace level message")
  birch.debug("Debug level message")
  birch.info("Info level message")
  birch.warn("Warning level message")
  birch.error("Error level message")
  birch.fatal("Fatal level message")

  // =========================================================================
  // Fancy Style (compact with icons)
  // =========================================================================
  print_header("FANCY STYLE (label formatter with icons)")

  birch.configure([
    birch.config_handlers([console.fancy_handler()]),
    birch.config_level(level.Trace),
  ])

  birch.trace("Trace level message")
  birch.debug("Debug level message")
  birch.info("Info level message")
  birch.warn("Warning level message")
  birch.error("Error level message")
  birch.fatal("Fatal level message")

  // =========================================================================
  // Fancy Style with timestamps
  // =========================================================================
  print_header("FANCY STYLE (with timestamps)")

  let fancy_with_ts =
    console.ConsoleConfig(..console.default_fancy_config(), timestamps: True)
  birch.configure([
    birch.config_handlers([console.handler_with_config(fancy_with_ts)]),
    birch.config_level(level.Trace),
  ])

  birch.trace("Trace level message")
  birch.debug("Debug level message")
  birch.info("Info level message")
  birch.warn("Warning level message")
  birch.error("Error level message")
  birch.fatal("Fatal level message")

  // =========================================================================
  // Badge Style
  // =========================================================================
  print_header("BADGE STYLE")

  let badge_config =
    console.ConsoleConfig(
      ..console.default_fancy_config(),
      level_formatter: level_formatter.badge_formatter(),
    )
  birch.configure([
    birch.config_handlers([console.handler_with_config(badge_config)]),
    birch.config_level(level.Trace),
  ])

  birch.trace("Trace with badge")
  birch.debug("Debug with badge")
  birch.info("Info with badge")
  birch.warn("Warning with badge")
  birch.error("Error with badge")
  birch.fatal("Fatal with badge")

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
  birch.configure([
    birch.config_handlers([console.handler_with_config(no_icons_config)]),
    birch.config_level(level.Trace),
  ])

  birch.trace("Trace level message")
  birch.debug("Debug level message")
  birch.info("Info level message")
  birch.warn("Warning level message")
  birch.error("Error level message")
  birch.fatal("Fatal level message")

  // =========================================================================
  // With Metadata
  // =========================================================================
  print_header("WITH METADATA")

  birch.configure([
    birch.config_handlers([console.fancy_handler()]),
    birch.config_level(level.Info),
  ])

  birch.info_m("Request received", [
    #("method", StringVal("GET")),
    #("path", StringVal("/api/users")),
  ])
  birch.warn_m("Slow query detected", [
    #("duration_ms", IntVal(1523)),
    #("table", StringVal("users")),
  ])
  birch.error_m("Connection failed", [
    #("host", StringVal("db.example.com")),
    #("port", IntVal(5432)),
  ])

  // =========================================================================
  // Named Loggers
  // =========================================================================
  print_header("NAMED LOGGERS")

  let db_logger =
    log.new("myapp.database")
    |> log.with_handlers([console.fancy_handler()])
    |> log.with_level(level.Debug)

  let http_logger =
    log.new("myapp.http")
    |> log.with_handlers([console.fancy_handler()])
    |> log.with_level(level.Info)

  log.info(db_logger, "Connected to database", [#("tls", BoolVal(True))])
  log.debug(db_logger, "Executing query", [
    #("sql", StringVal("SELECT * FROM users")),
  ])
  log.info(http_logger, "Server started", [#("port", IntVal(8080))])
  log.warn(http_logger, "Rate limit exceeded", [
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
      log.new("build")
      |> log.with_handlers([console.indented_handler(1)])
      |> log.with_level(level.Debug)

    log.info(indented, "Compiling sources...", [])
    log.info(indented, "Linking...", [])
    log.info(indented, "Done!", [])
    Nil
  })

  console.with_group("Running tests", fn() {
    let indented =
      log.new("test")
      |> log.with_handlers([console.indented_handler(1)])
      |> log.with_level(level.Debug)

    log.info(indented, "Unit tests...", [])
    log.info(indented, "Integration tests...", [])
    log.info(indented, "All tests passed!", [])
    Nil
  })

  console.with_group("Deploying application", fn() {
    let indented =
      log.new("deploy")
      |> log.with_handlers([console.indented_handler(1)])
      |> log.with_level(level.Debug)

    log.info(indented, "Uploading artifacts...", [])
    log.info(indented, "Updating configuration...", [])
    log.info(indented, "Deployment complete!", [])
    Nil
  })

  console.with_group("Cleanup", fn() {
    let indented =
      log.new("cleanup")
      |> log.with_handlers([console.indented_handler(1)])
      |> log.with_level(level.Debug)

    log.info(indented, "Removing temp files...", [])
    log.info(indented, "Done!", [])
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

  birch.configure([
    birch.config_handlers([console.handler_with_config(custom_config)]),
    birch.config_level(level.Trace),
  ])

  birch.trace("Trace with emoji")
  birch.debug("Debug with emoji")
  birch.info("Info with emoji")
  birch.warn("Warning with emoji")
  birch.error("Error with emoji")
  birch.fatal("Fatal with emoji")

  // =========================================================================
  // No Colors (for piping/redirection)
  // =========================================================================
  print_header("NO COLORS")

  let no_color_config =
    console.ConsoleConfig(..console.default_fancy_config(), color: False)

  birch.configure([
    birch.config_handlers([console.handler_with_config(no_color_config)]),
    birch.config_level(level.Trace),
  ])

  birch.trace("Trace level message")
  birch.debug("Debug level message")
  birch.info("Info level message")
  birch.warn("Warning level message")
  birch.error("Error level message")
  birch.fatal("Fatal level message")

  // =========================================================================
  // Auto-Indent from Scopes
  // =========================================================================
  print_header("AUTO-INDENT FROM SCOPES")

  let auto_indent_config =
    console.ConsoleConfig(
      ..console.default_fancy_config(),
      auto_indent_from_scopes: True,
    )

  birch.configure([
    birch.config_handlers([console.handler_with_config(auto_indent_config)]),
    birch.config_level(level.Info),
  ])

  birch.info("Outside any scope - no indentation")

  birch.with_scope([#("request_id", StringVal("req-123"))], fn() {
    birch.info("Level 1 scope - indented once")

    birch.with_scope([#("step", StringVal("validation"))], fn() {
      birch.info("Level 2 scope - indented twice")
      birch.warn("Warnings are also indented at level 2")

      birch.with_scope([#("substep", StringVal("schema_check"))], fn() {
        birch.info("Level 3 scope - indented three times")
        Nil
      })

      birch.info("Back to level 2")
      Nil
    })

    birch.info("Back to level 1")
    Nil
  })

  birch.info("Back outside scope - no indentation")

  // Reset
  birch.reset_config()
  print_header("DEMO COMPLETE")
}

fn print_header(title: String) {
  let line = "═══════════════════════════════════════════════════════════════"
  io.println("")
  io.println(line)
  io.println(" " <> title)
  io.println(line)
}
