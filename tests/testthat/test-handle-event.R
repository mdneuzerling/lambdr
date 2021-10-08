test_that("event headers extracted, prettified and distilled", {

  # Make webmockr intercept HTTP requests
  webmockr::enable(quiet = TRUE)
  withr::defer(webmockr::disable(quiet = TRUE))

  lambda_runtime_api <- "red_panda"
  request_id <- "abc123"
  request_arn <- "arn:aws:lambda:us-west-2:123456789012:function:my-function"

  invocation_endpoint <- paste0(
    "http://", lambda_runtime_api, "/2018-06-01/runtime/invocation/next"
  )

  # Mock the invocation to return a Lambda input with mock request ID and input
  webmockr::stub_request("get", invocation_endpoint) %>%
    webmockr::to_return(
      body = list(), # input is irrelevant here --- it's never used
      headers = list(
        "lambda-runtime-aws-request-id" = request_id,
        "LAMBDA-RUNTIME-INVOKED-FUNCTION-ARN" = request_arn
      ),
      status = 200
    )

  event <- httr::GET(invocation_endpoint)
  headers <- extract_event_headers(event)

  expect_equal(
    headers,
    structure(
      list(
        "lambda-runtime-aws-request-id" = request_id,
        # note the conversion to lower-case:
        "lambda-runtime-invoked-function-arn" = request_arn
      ),
      class = c("insensitive", "list")
    )
  )

  expect_equal(
    prettify_list(headers),
    paste0(
      "lambda-runtime-aws-request-id", "=", request_id, ", ",
      "lambda-runtime-invoked-function-arn", "=", request_arn
    )
  )

  expect_equal(
    extract_context(headers),
    list(
      aws_request_id = request_id,
      invoked_function_arn = request_arn
    )
  )

  expect_equal(
    extract_request_id_from_headers(headers),
    request_id
  )
})

test_that("error occurs when request ID not in headers", {

  # Make webmockr intercept HTTP requests
  webmockr::enable(quiet = TRUE)
  withr::defer(webmockr::disable(quiet = TRUE))

  lambda_runtime_api <- "red_panda"
  request_id <- "abc123"
  request_arn <- "arn:aws:lambda:us-west-2:123456789012:function:my-function"

  invocation_endpoint <- paste0(
    "http://", lambda_runtime_api, "/2018-06-01/runtime/invocation/next"
  )

  # Mock the invocation to return a Lambda input with mock request ID and input
  webmockr::stub_request("get", invocation_endpoint) %>%
    webmockr::to_return(
      body = list(), # input is irrelevant here --- it's never used
      headers = list(
        "LAMBDA-RUNTIME-INVOKED-FUNCTION-ARN" = request_arn
      ),
      status = 200
    )

  event <- httr::GET(invocation_endpoint)
  headers <- extract_event_headers(event)

  expect_error(
    extract_request_id_from_headers(headers),
    "Event doesn't contain request ID"
  )
})

test_that("status code check works", {
  # Make webmockr intercept HTTP requests
  webmockr::enable(quiet = TRUE)
  withr::defer(webmockr::disable(quiet = TRUE))

  request_id <- "abc123"

  good_invocation_endpoint <- paste0(
    "http://", "good_invocation", "/2018-06-01/runtime/invocation/next"
  )
  bad_invocation_endpoint <- paste0(
    "http://", "bad_invocation", "/2018-06-01/runtime/invocation/next"
  )

  webmockr::stub_request("get", good_invocation_endpoint) %>%
    webmockr::to_return(
      body = list(), # input is irrelevant here --- it's never used
      headers = list("lambda-runtime-aws-request-id" = request_id),
      status = 200
    )

  webmockr::stub_request("get", bad_invocation_endpoint) %>%
    webmockr::to_return(
      body = list(), # input is irrelevant here --- it's never used
      headers = list("lambda-runtime-aws-request-id" = request_id),
      status = 500
    )

  good_invocation_response <- httr::GET(good_invocation_endpoint)
  expect_true(
    assert_status_code_is_good(httr::status_code(good_invocation_response))
  )

  bad_invocation_response <- httr::GET(bad_invocation_endpoint)
  expect_error(
    assert_status_code_is_good(httr::status_code(bad_invocation_response)),
    "Didn't get status code 200. Status code: 500"
  )
})

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
