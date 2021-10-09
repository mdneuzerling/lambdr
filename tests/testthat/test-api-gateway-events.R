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
