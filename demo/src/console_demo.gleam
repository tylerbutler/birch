//// Console Handler Demo
////
//// Demonstrates all the console handler presentation options.

import birch as log
import birch/handler/console
import birch/level
import birch/level_formatter
import birch/logger
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

  // =========================================================================
  // Simple Style without timestamps
  // =========================================================================
  print_header("SIMPLE STYLE (no timestamps)")

  let simple_no_ts =
    console.default_config()
    |> console.without_timestamps
  log.configure([
    log.config_handlers([console.handler_with_config(simple_no_ts)]),
    log.config_level(level.Trace),
  ])

  log.info("Info without timestamp")
  log.warn("Warning without timestamp")

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

  // =========================================================================
  // Fancy Style with timestamps
  // =========================================================================
  print_header("FANCY STYLE (with timestamps)")

  let fancy_with_ts =
    console.default_fancy_config()
    |> console.with_timestamps
  log.configure([
    log.config_handlers([console.handler_with_config(fancy_with_ts)]),
    log.config_level(level.Trace),
  ])

  log.info("Info with timestamp")
  log.warn("Warning with timestamp")

  // =========================================================================
  // Badge Style
  // =========================================================================
  print_header("BADGE STYLE")

  let badge_config =
    console.default_fancy_config()
    |> console.with_badge_style
  log.configure([
    log.config_handlers([console.handler_with_config(badge_config)]),
    log.config_level(level.Trace),
  ])

  log.trace("Trace with badge")
  log.debug("Debug with badge")
  log.info("Info with badge")
  log.warn("Warning with badge")
  log.error("Error with badge")

  // =========================================================================
  // Label Style without icons
  // =========================================================================
  print_header("LABEL STYLE (no icons)")

  let no_icons_config =
    console.default_fancy_config()
    |> console.with_label_style_no_icons
  log.configure([
    log.config_handlers([console.handler_with_config(no_icons_config)]),
    log.config_level(level.Trace),
  ])

  log.info("Info without icon")
  log.warn("Warning without icon")
  log.error("Error without icon")

  // =========================================================================
  // With Metadata
  // =========================================================================
  print_header("WITH METADATA")

  log.configure([
    log.config_handlers([console.fancy_handler()]),
    log.config_level(level.Info),
  ])

  log.info_m("Request received", [#("method", "GET"), #("path", "/api/users")])
  log.warn_m("Slow query detected", [#("duration_ms", "1523"), #("table", "users")])
  log.error_m("Connection failed", [#("host", "db.example.com"), #("port", "5432")])

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

  logger.info(db_logger, "Connected to database", [])
  logger.debug(db_logger, "Executing query", [#("sql", "SELECT * FROM users")])
  logger.info(http_logger, "Server started", [#("port", "8080")])
  logger.warn(http_logger, "Rate limit exceeded", [#("client", "192.168.1.100")])

  // =========================================================================
  // Semantic Log Types
  // =========================================================================
  print_header("SEMANTIC LOG TYPES")

  console.start("Building project...", [#("target", "release")])
  console.success("Build completed!", [#("duration", "2.5s")])
  console.ready("Server listening", [#("port", "3000")])
  console.fail("Could not connect to cache", [#("host", "redis.local")])

  // =========================================================================
  // Box Output
  // =========================================================================
  print_header("BOX OUTPUT")

  console.write_box("Hello, World!")

  console.write_box_with_title("This is a message\nwith multiple lines\nof content", "Notice")

  // =========================================================================
  // Grouping
  // =========================================================================
  print_header("GROUPING")

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

  // =========================================================================
  // Custom Level Formatter
  // =========================================================================
  print_header("CUSTOM LEVEL FORMATTER")

  let emoji_formatter =
    level_formatter.custom_level_formatter(fn(lvl, _use_color) {
      case lvl {
        level.Trace -> "🔍"
        level.Debug -> "🐛"
        level.Info -> "💡"
        level.Warn -> "⚠️ "
        level.Err -> "🔥"
        level.Fatal -> "💀"
      }
    })

  let custom_config =
    console.default_fancy_config()
    |> console.with_level_formatter(emoji_formatter)

  log.configure([
    log.config_handlers([console.handler_with_config(custom_config)]),
    log.config_level(level.Trace),
  ])

  log.trace("Trace with emoji")
  log.debug("Debug with emoji")
  log.info("Info with emoji")
  log.warn("Warning with emoji")
  log.error("Error with emoji")

  // =========================================================================
  // No Colors (for piping/redirection)
  // =========================================================================
  print_header("NO COLORS")

  let no_color_config =
    console.default_fancy_config()
    |> console.without_color

  log.configure([
    log.config_handlers([console.handler_with_config(no_color_config)]),
    log.config_level(level.Info),
  ])

  log.info("This has no ANSI color codes")
  log.warn("Safe for piping to files")
  log.error("Or processing with other tools")

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
