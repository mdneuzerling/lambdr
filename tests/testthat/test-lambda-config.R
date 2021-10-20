test_that("we can catch invalid lambda configs", {
  expect_true(validate_lambda_config(basic_lambda_config()))

  expected_error <- paste0(
    "The Lambda runtime configuration should be provided by the ",
    "lambda_config function."
  )
  expect_error(
    validate_lambda_config(list()),
    expected_error
  )
})

test_that("Defaults in setup are considered after environment variables ", {
  expect_error(
    get_lambda_environment_variable("doesntexist"),
    "doesntexist environment variable is not set"
  )

  expect_equal(
    get_lambda_environment_variable("doesntexist", default = "red panda"),
    "red panda"
  )

  expect_equal(
    withr::with_envvar(
      c("doesntexist" = "giraffe"),
      get_lambda_environment_variable("doesntexist", default = "red panda")
    ),
    "giraffe"
  )
})

test_that("Lambda config retrieved from environment variable", {
  config <- withr::with_envvar(
    c(
      "AWS_LAMBDA_RUNTIME_API" = "red_panda",
      "LAMBDA_TASK_ROOT" = "giraffe",
      "_HANDLER" = "sqrt"
    ),
    lambda_config()
  )

  expect_s3_class(config, "lambda_config")
  expect_equal(config$runtime_api, "red_panda")
  expect_equal(config$task_root, "giraffe")
  expect_equal(config$handler, sqrt)
})

test_that("lambda config can record if Base64 is to be used for decoding", {
  default_config <- basic_lambda_config()
  no_base64_config <- basic_lambda_config(decode_base64 = FALSE)
  base64_config <- basic_lambda_config(decode_base64 = TRUE)

  expect_true(default_config$decode_base64)
  expect_false(no_base64_config$decode_base64)
  expect_true(base64_config$decode_base64)
})

test_that("outright error when runtime_api not provided", {
  expected_error <- paste(
    "AWS_LAMBDA_RUNTIME_API environment variable is not set. This environment",
    "variable is set by AWS when Lambda is instantiated. It will not appear",
    "when running local tests."
  )
  expect_error(
    basic_lambda_config(runtime_api = ""),
    expected_error
  )
})

# We no longer demand that the task root be defined, since it doesn't actually
# affect anything in the runtime.

# test_that("initialisation error recorded when task_root undefined", {
#   error_received <- trigger_initialisation_error(
#     expected_error = paste(
#       "LAMBDA_TASK_ROOT environment variable is not set. This environment",
#       "variable is set by AWS when Lambda is instantiated. It will not appear",
#       "when running local tests."
#     ),
#     task_root = "",
#     handler = "sqrt"
#   )
#   expect_true(error_received)
# })

test_that("initialisation error recorded when handler undefined", {
  error_received <- trigger_initialisation_error(
    expected_error = paste0(
      "The _HANDLER environment variable is undefined.\n",
      "This environment variable is configured by AWS Lambda based\n",
      "the value configured either as the Dockerfile CMD or in the\n",
      "AWS Lambda console (which will always take priority).\n",
      "Alternatively, pass a function to the `handler` argument\n",
      "of `lambda_config`, although note that `lambda_config` will\n",
      "always defer to the environment variables if available."
    ),
    task_root = "giraffe",
    handler = ""
  )
  expect_true(error_received)
})

test_that("undefined handlers are caught", {
  handler_character <- "undefined_handler"

  expected_error <- paste0(
    handler_character, ", as defined by the _HANDLER environment\n",
    "variable, can't be found. Check that this exists in the",
    "environment passed to `lambda_config` (defaults to the parent\n",
    "frame)"
  )

  error_received <- trigger_initialisation_error(
    expected_error = expected_error,
    task_root = "giraffe",
    handler = handler_character
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
  config <- basic_lambda_config(handler = "plus_one")
  expect_equal(config$handler(1), 2)
})

test_that("can recognise context argument in function formals", {
  doesnt_accept_context <- function() {}
  expect_false(function_accepts_context(doesnt_accept_context))

  accepts_context <- function(context) {}
  expect_true(function_accepts_context(accepts_context))
})

test_that("context argument recognised in handler formals", {
  # similar to the test above, but we're checking that the `lambda_config` uses
  # the function and stores the correct result
  no_context <- function() {}
  no_context_config <- basic_lambda_config(handler = "no_context")
  expect_false(no_context_config$pass_context_argument)

  yes_context <- function(context) {}
  yes_context_config <- basic_lambda_config(handler = "yes_context")
  expect_true(yes_context_config$pass_context_argument)
})
