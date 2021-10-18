test_that("start_lambda starts lambda runtime", {
  lambda_runtime_api <- "red_panda"
  request_id <- "abc123"

  # Make webmockr intercept HTTP requests
  webmockr::enable(quiet = TRUE)
  withr::defer(webmockr::disable(quiet = TRUE))

  invocation_endpoint <- get_next_invocation_endpoint(lambda_runtime_api)
  response_endpoint <- get_response_endpoint(
    lambda_runtime_api,
    request_id
  )

  # Mock the invocation to return a Lambda input with mock request ID and input
  webmockr::stub_request("get", invocation_endpoint) %>%
    webmockr::to_return(
      body = list(number = 3),
      headers = list("lambda-runtime-aws-request-id" = request_id),
      status = 200
    )

  # Mock the response endpoint. We expect a response of `2`.
  webmockr::stub_request("post", response_endpoint) %>%
    webmockr::wi_th(
      headers = default_response_headers,
      body = as_stringified_json(list(parity = "odd"))
    ) %>%
    webmockr::to_return(status = 200)

  withr::with_envvar(
    c(
      "AWS_LAMBDA_RUNTIME_API" = "red_panda",
      "LAMBDA_TASK_ROOT" = "giraffe",
      "_HANDLER" = "parity"
    ),
    start_lambda(timeout_seconds = 0.5)
  )

  requests <- webmockr::request_registry()
  n_responses <- requests$times_executed(
    webmockr::RequestPattern$new("post", response_endpoint)
  )

  expect_gte(n_responses, 1)
})
