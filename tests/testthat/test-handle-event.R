test_that("Mock square root function", {

  mock_request_id <- "abc123"

  # Make webmockr intercept HTTP requests
  webmockr::enable(quiet = TRUE)
  withr::defer(webmockr::disable(quiet = TRUE))

  use_basic_lambda_setup(handler = "sqrt")

  invocation_endpoint <- get_next_invocation_endpoint()
  response_endpoint <- get_response_endpoint(mock_request_id)

  # Mock the invocation to return a Lambda input with mock request ID and
  # input value `x = 4`.
  webmockr::stub_request("get", invocation_endpoint) %>%
    webmockr::to_return(
      body = jsonlite::toJSON(list(x = 4)),
      headers = list("lambda-runtime-aws-request-id" = mock_request_id),
      status = 200
    )

  # Mock the response endpoint. We expect a response of `2`.
  webmockr::stub_request("post", response_endpoint) %>%
    webmockr::wi_th(
      headers = list(
        'Accept' = 'application/json, text/xml, application/xml, */*',
        'Content-Type' = ''
      ),
      body = 2
    ) %>%
    webmockr::to_return(status = 200)

  # 1 second should be plenty of time to calculate some square roots.
  start_listening(timeout_seconds = 1)

  requests <- webmockr::request_registry()
  n_responses <- requests$times_executed(
    webmockr::RequestPattern$new("post", response_endpoint)
  )
  expect_gte(n_responses, 3) # at least 3 response POSTs with the correct body
})
