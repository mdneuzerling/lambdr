test_that("Mock square root function", {
  use_basic_lambda_setup(handler = "sqrt")
  mock_response_success <- mock_response(
    input = jsonlite::toJSON(list(x = 4)),
    expected_response_body = 2
  )
  expect_true(mock_response_success)
})

test_that("Mock custom function", {
  use_basic_lambda_setup(handler = "parity")

  mock_response_success <- mock_response(
    input = jsonlite::toJSON(list(number = 5)),
    expected_response_body = jsonlite::toJSON(list(parity = "odd"), auto_unbox = TRUE)
  )
  expect_true(mock_response_success)
})

test_that("Mock function with no inputs", {
  use_basic_lambda_setup(handler = "no_arguments")

  mock_response_success <- mock_response(
    input = jsonlite::toJSON(list()),
    expected_response_body = jsonlite::toJSON(
      list(animal = "dog", breed = "corgi"),
      auto_unbox = TRUE
    )
  )

  expect_true(mock_response_success)
})

test_that("handlers that accept a context argument receive it", {
  assert_context_exists <- function(context) {
    if (missing(context)) {
      stop("context not received")
    }
    list(animal = "dog", breed = "corgi")
  }

  use_basic_lambda_setup(handler = "assert_context_exists")

  mock_response_success <- mock_response(
    input = jsonlite::toJSON(list()),
    expected_response_body = jsonlite::toJSON(
      list(animal = "dog", breed = "corgi"),
      auto_unbox = TRUE
    )
  )

  expect_true(mock_response_success)
})

test_that("errors are sent to invocation error endpoint", {
  use_basic_lambda_setup(handler = "no_arguments")

  mock_invocation_error_success <- mock_invocation_error(
    input = jsonlite::toJSON(list(x = 3)),
    expected_error_body = jsonlite::toJSON(
      list(
        errorMessage = "unused argument (x = 3)",
        errorType = "simpleError",
        stackTrace = c()
      ),
      auto_unbox = TRUE
    )
  )

  expect_true(mock_invocation_error_success)
})
