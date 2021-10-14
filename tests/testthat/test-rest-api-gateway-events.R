test_that("we can parse input with parameter arguments from a REST API Gateway", {
  use_basic_lambda_setup(handler = "parity")
  mock_response_success <- mock_rest_api_gateway_event(
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

test_that("we can parse input with body arguments from a REST API Gateway", {
  use_basic_lambda_setup(handler = "parity")
  mock_response_success <- mock_rest_api_gateway_event(
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

test_that("we can parse input with body and parameter arguments from a REST
          API Gateway", {
  named_sum <- function(x, y) list(sum = x + y)
  use_basic_lambda_setup(handler = "named_sum")
  mock_response_success <- mock_rest_api_gateway_event(
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

test_that("we can parse input with no arguments from a REST API Gateway", {
  use_basic_lambda_setup(handler = "no_arguments")
  mock_response_success <- mock_rest_api_gateway_event(
    query_parameters = NULL,
    body_parameters = NULL,
    result = list(animal = "dog", breed = "corgi"),
    expected_response_headers = list(
      "Accept" = "application/json, text/xml, application/xml, */*",
      "Content-Type" = ""
    ),
    request_id = "abc123",
    timeout_seconds = 0.5
  )

  expect_true(mock_response_success)
})

test_that("REST API Gateway event errors are handled as responses", {
  give_error <- function() stop("my heart is a fish")
  use_basic_lambda_setup(handler = "give_error")

  mock_response_success <- mock_rest_api_gateway_event(
    query_parameters = NULL,
    body_parameters = NULL,
    result = as_json(
      list(
        statusCode = 500L,
        headers = list(
          "Content-Type" = "text/plain",
          "x-amzn-ErrorType" = "simpleError"
        ),
        "isBase64Encoded" = FALSE,
        "body" = "my heart is a fish"
      )
    ),
    expected_response_headers = list(
      "Accept" = "application/json, text/xml, application/xml, */*",
      "Content-Type" = "application/vnd.aws.lambda.error+json"
    ),
    expect_result_as_is = TRUE
  )

  expect_true(mock_response_success)
})

test_that("REST API Gateway event errors can include status codes", {
  give_error <- function() stop_html("my heart is a fish", code = 404L)
  use_basic_lambda_setup(handler = "give_error")

  mock_response_success <- mock_rest_api_gateway_event(
    query_parameters = NULL,
    body_parameters = NULL,
    result = as_json(
      list(
        statusCode = 404L,
        headers = list(
          "Content-Type" = "text/plain",
          "x-amzn-ErrorType" = "html_error"
        ),
        "isBase64Encoded" = FALSE,
        "body" = "my heart is a fish"
      )
    ),
    expected_response_headers = list(
      "Accept" = "application/json, text/xml, application/xml, */*",
      "Content-Type" = "application/vnd.aws.lambda.error+json"
    ),
    expect_result_as_is = TRUE
  )

  expect_true(mock_response_success)
})
