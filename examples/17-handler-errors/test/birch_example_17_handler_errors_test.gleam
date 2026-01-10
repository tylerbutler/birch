import birch as log
import birch/handler
import birch/handler/console
import gleeunit
import gleeunit/should
import handler_errors as example

pub fn main() {
  gleeunit.main()
}

pub fn safe_handler_test() {
  let handler = example.create_safe_handler()

  // Should be able to use it without error
  log.configure([log.config_handlers([handler])])
  log.info("Test with safe handler")
  log.reset_config()

  should.be_true(True)
}

pub fn monitored_handler_test() {
  // Track if callback was set up correctly
  let handler =
    example.create_monitored_handler(fn(_err: handler.HandlerError) {
      // Error callback is configured
      Nil
    })

  log.configure([log.config_handlers([handler])])
  log.info("Test with monitored handler")
  log.reset_config()

  should.be_true(True)
}

pub fn resilient_handler_test() {
  let handler = example.create_resilient_handler()

  log.configure([log.config_handlers([handler])])
  log.info("Test with resilient handler")
  log.reset_config()

  should.be_true(True)
}

pub fn failing_handler_test() {
  let handler = example.create_failing_handler()

  log.configure([log.config_handlers([handler])])
  log.info("Test with failing handler")
  log.reset_config()

  should.be_true(True)
}

pub fn with_error_callback_test() {
  let handler =
    console.handler()
    |> handler.with_error_callback(fn(_err: handler.HandlerError) { Nil })

  log.configure([log.config_handlers([handler])])
  log.info("Test with error callback")
  log.reset_config()

  should.be_true(True)
}

pub fn config_on_error_test() {
  log.configure([
    log.config_on_error(fn(_err: handler.HandlerError) {
      // Global error handler
      Nil
    }),
  ])

  log.info("Test with global error handler")
  log.reset_config()

  should.be_true(True)
}
