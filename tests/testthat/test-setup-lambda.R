test_that("Lambda can be set up and reset", {
  expect_false(lambda$is_setup)
  expect_error(assert_lambda_is_setup(), "is not configured")

  withr::with_envvar(
    c(
      "AWS_LAMBDA_RUNTIME_API" = "red_panda",
      "LAMBDA_TASK_ROOT" = "giraffe",
      "_HANDLER" = "sqrt"
    ),
    setup_lambda()
  )
  withr::defer(reset_lambda())
  expect_true(lambda$is_setup)

  reset_lambda()
  expect_false(lambda$is_setup)
  expect_error(assert_lambda_is_setup(), "is not configured")
})

test_that("Lambda runtime API retrieval fails before setup", {
  expect_setup_failure(get_lambda_runtime_api)
})

test_that("Lambda task root retrieval fails before setup", {
  expect_setup_failure(get_lambda_task_root)
})

test_that("Lambda handler retrieval fails before setup", {
  expect_setup_failure(get_handler)
  expect_setup_failure(get_handler_character)
})

test_that("we can retrieve the Lambda runtime API variable", {
  use_basic_lambda_setup()
  expect_equal(
    get_lambda_runtime_api(),
    "red_panda"
  )
})

test_that("we can retrieve the Lambda task root variable", {
  use_basic_lambda_setup()
  expect_equal(
    get_lambda_task_root(),
    "giraffe"
  )
})

test_that("we can retrieve the Lambda handler", {
  use_basic_lambda_setup(handler = "sqrt")
  expect_equal(
    get_handler_character(),
    "sqrt"
  )
  expect_equal(
    get_handler(),
    sqrt
  )
})

test_that("initialisation error recorded when task_root undefined", {
  error_received <- trigger_initialisation_error(
    expected_error = paste(
      "LAMBDA_TASK_ROOT environment variable is not set. This environment",
      "variable is set by AWS when Lambda is instantiated. It will not appear",
      "when running local tests."
    ),
    task_root = "",
    handler = "sqrt"
  )
  expect_true(error_received)
})

test_that("initialisation error recorded when handler undefined", {
  error_received <- trigger_initialisation_error(
    expected_error = paste(
      "_HANDLER environment variable is not set. This environment",
      "variable is set by AWS when Lambda is instantiated. It will not appear",
      "when running local tests."
    ),
    task_root = "giraffe",
    handler = ""
  )
  expect_true(error_received)
})

test_that("undefined handlers are caught", {
  error_received <- trigger_initialisation_error(
    expected_error = "undefined_handler not found",
    task_root = "giraffe",
    handler = "undefined_handler"
  )
  expect_true(error_received)
})

test_that("non-function handlers are caught", {
  error_received <- trigger_initialisation_error(
    expected_error = "mtcars is not a function",
    task_root = "giraffe",
    handler = "mtcars"
  )
  expect_true(error_received)
})

test_that("handler can be a custom function", {
  plus_one <- function(x) x + 1

  use_basic_lambda_setup(handler = "plus_one")
  withr::defer(reset_lambda(), envir = parent.frame())

  expect_equal(get_handler()(1), 2)
})

test_that("can recognise context argument in function formals", {
  doesnt_accept_context <- function() {}
  expect_false(function_accepts_context(doesnt_accept_context))

  accepts_context <- function(context) {}
  expect_true(function_accepts_context(accepts_context))
})

test_that("context argument recognised in handler formals", {
  # similar to the test above, but we're checking that the `setup_lambda` uses
  # the function and stores the correct result
  doesnt_accept_context <- function() {}
  use_basic_lambda_setup(handler = "doesnt_accept_context")
  expect_false(lambda$pass_context_argument)

  accepts_context <- function(context) {}
  use_basic_lambda_setup(handler = "accepts_context")
  expect_true(lambda$pass_context_argument)
})
