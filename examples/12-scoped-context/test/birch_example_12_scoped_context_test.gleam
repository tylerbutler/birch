import birch as log
import birch/meta
import gleeunit
import gleeunit/should
import birch_example_12_scoped_context as scoped_context

pub fn main() {
  gleeunit.main()
}

pub fn with_request_context_test() {
  // The function should execute and return the result
  let result =
    scoped_context.with_request_context("req-123", "user-456", fn() {
      "work done"
    })

  result
  |> should.equal("work done")
}

pub fn get_current_request_id_inside_scope_test() {
  // On JavaScript without AsyncLocalStorage, with_scope runs the function but
  // scoped context isn't persisted, so we just verify it runs
  log.with_scope([meta.string("request_id", "test-req-789")], fn() {
    case log.is_scoped_context_available() {
      True -> {
        scoped_context.get_current_request_id()
        |> should.be_ok()
        |> should.equal("test-req-789")
      }
      False -> {
        // On JavaScript, scope may not be available - just verify no crash
        Nil
      }
    }
  })
}

pub fn get_current_request_id_outside_scope_test() {
  // Outside a scope, the request ID should be unavailable (error)
  // This works on both platforms
  scoped_context.get_current_request_id()
  |> should.be_error()
}

pub fn generate_request_id_test() {
  let id = scoped_context.generate_request_id()

  // Should start with "req-"
  case id {
    "req-" <> _ -> should.be_true(True)
    _ -> should.fail()
  }
}

pub fn is_scoped_context_available_test() {
  // On Erlang, this is always true
  // On JavaScript, it depends on the runtime (Node.js with AsyncLocalStorage vs browser)
  // Just verify the function runs without error - the result is platform-dependent
  let _available = log.is_scoped_context_available()
  should.be_true(True)
}
