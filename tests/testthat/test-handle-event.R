test_that("Mock square root function", {
  use_basic_lambda_setup(handler = "sqrt")
  mock_response_success <- mock_response(
    input = as_json(list(x = 4)),
    expected_response_body = 2
  )
  expect_true(mock_response_success)
})

test_that("Mock custom function", {
  use_basic_lambda_setup(handler = "parity")

  mock_response_success <- mock_response(
    input = as_json(list(number = 5)),
    expected_response_body = as_json(list(parity = "odd"))
  )
  expect_true(mock_response_success)
})

test_that("Mock function with no inputs", {
  use_basic_lambda_setup(handler = "no_arguments")

  mock_response_success <- mock_response(
    input = as_json(list()),
    expected_response_body = as_json(
      list(animal = "dog", breed = "corgi")
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
    input = as_json(list()),
    expected_response_body = as_json(
      list(animal = "dog", breed = "corgi")
    )
  )

  expect_true(mock_response_success)
})

test_that("errors are sent to invocation error endpoint", {
  use_basic_lambda_setup(handler = "no_arguments")

  mock_invocation_error_success <- mock_invocation_error(
    input = as_json(list(x = 3)),
    expected_error_body = as_json(
      list(
        errorMessage = "unused argument (x = 3)",
        errorType = "simpleError",
        stackTrace = list()
      )
    )
  )

  expect_true(mock_invocation_error_success)
})

test_that("we can parse input with parameter arguments from an API Gateway", {
  use_basic_lambda_setup(handler = "parity")
  mock_response_success <- mock_api_gateway_event(
    query_parameters = list(number = 9),
    result = list(parity = "odd"),
    expected_response_headers = list(
      "Accept" = "application/json, text/xml, application/xml, */*",
      "Content-Type" = ""
    ),
    request_id = "abc123",
    timeout_seconds = 0.5
  )

  expect_true(mock_response_success)
})

test_that("we can parse input with body arguments from an API Gateway", {
  use_basic_lambda_setup(handler = "parity")
  mock_response_success <- mock_api_gateway_event(
    body_parameters = list(number = 9), #' "{\\"number\\": 9}"',
    result = list(parity = "odd"),
    expected_response_headers = list(
      "Accept" = "application/json, text/xml, application/xml, */*",
      "Content-Type" = ""
    ),
    request_id = "abc123",
    timeout_seconds = 0.5
  )

  expect_true(mock_response_success)
})

test_that("we can parse input with body and parameter arguments from an API Gateway", {
  named_sum <- function(x, y) list(sum = x + y)
  use_basic_lambda_setup(handler = "named_sum")
  mock_response_success <- mock_api_gateway_event(
    query_parameters = list(x = 5),
    body_parameters = list(y = 4),
    result = list(sum = 9),
    expected_response_headers = list(
      "Accept" = "application/json, text/xml, application/xml, */*",
      "Content-Type" = ""
    ),
    request_id = "abc123",
    timeout_seconds = 0.5
  )

  expect_true(mock_response_success)
})
