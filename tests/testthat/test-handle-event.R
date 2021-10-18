test_that("Mock square root function", {
  mock_response_success <- mock_response(
    input = as_json(list(x = 4)),
    expected_response_body = 2,
    config = basic_lambda_config(handler = "sqrt")
  )
  expect_true(mock_response_success)
})

test_that("Mock custom function", {
  mock_response_success <- mock_response(
    input = as_json(list(number = 5)),
    expected_response_body = as_json(list(parity = "odd")),
    config = basic_lambda_config(handler = "parity")
  )
  expect_true(mock_response_success)
})

test_that("Custom deserialisers are used in event handling", {
  custom_deserialiser <- function(event_content) {
    list(number = 0)
  }
  config <- basic_lambda_config(
    handler = "parity",
    deserialiser = custom_deserialiser
  )

  mock_response_success <- mock_response(
    input = as_json(list(number = 5)),
    expected_response_body = as_json(list(parity = "even")),
    config = config
  )
  expect_true(mock_response_success)
})

test_that("Custom serialisers are used in event handling", {
  custom_serialiser <- function(result) {
    "my heart is a fish"
  }
  config <- basic_lambda_config(
    handler = "parity",
    serialiser = custom_serialiser
  )

  mock_response_success <- mock_response(
    input = as_json(list(number = 5)),
    expected_response_body = "my heart is a fish",
    config = config
  )
  expect_true(mock_response_success)
})

test_that("Mock function with no inputs", {
  mock_response_success <- mock_response(
    input = as_json(list()),
    expected_response_body = as_json(
      list(animal = "dog", breed = "corgi")
    ),
    config = basic_lambda_config(handler = "no_arguments")
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
  mock_response_success <- mock_response(
    input = as_json(list()),
    expected_response_body = as_json(
      list(animal = "dog", breed = "corgi")
    ),
    config = basic_lambda_config(handler = "assert_context_exists")
  )

  expect_true(mock_response_success)
})

test_that("errors are sent to invocation error endpoint", {
  mock_invocation_error_success <- mock_invocation_error(
    input = as_json(list(x = 3)),
    expected_error_body = as_json(
      list(
        errorMessage = "unused argument (x = 3)",
        errorType = "simpleError",
        stackTrace = list()
      )
    ),
    config = basic_lambda_config(handler = "no_arguments")
  )

  expect_true(mock_invocation_error_success)
})
