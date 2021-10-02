expect_setup_failure <- function(endpoint_function, ...) {
  eval(bquote({
    expect_error(
      endpoint_function(...),
      "The AWS Lambda runtime is not configured"
    )
  }))
}

use_basic_lambda_setup <- function(handler = "sqrt") {
  withr::with_envvar(
    c("AWS_LAMBDA_RUNTIME_API" = "red_panda",
      "LAMBDA_TASK_ROOT" = "giraffe",
      "_HANDLER" = handler),
    withr::with_environment(
      parent.frame(),
      setup_lambda()
    )
  )
  withr::defer(reset_lambda(), envir = parent.frame())
}


mock_response <- function(
  input,
  expected_response_body,
  expected_response_headers = list(
    'Accept' = 'application/json, text/xml, application/xml, */*',
    'Content-Type' = ''
  ),
  request_id = "abc123",
  timeout_seconds = 1
  ) {

  # Make webmockr intercept HTTP requests
  webmockr::enable(quiet = TRUE)
  withr::defer(webmockr::disable(quiet = TRUE))

  invocation_endpoint <- get_next_invocation_endpoint()
  response_endpoint <- get_response_endpoint(request_id)

  # Mock the invocation to return a Lambda input with mock request ID and
  # input value `x = 4`.
  webmockr::stub_request("get", invocation_endpoint) %>%
    webmockr::to_return(
      body = input,
      headers = list("lambda-runtime-aws-request-id" = request_id),
      status = 200
    )

  # Mock the response endpoint. We expect a response of `2`.
  webmockr::stub_request("post", response_endpoint) %>%
    webmockr::wi_th(
      headers = expected_response_headers,
      body = expected_response_body
    ) %>%
    webmockr::to_return(status = 200)

  # 1 second should be plenty of time to calculate some square roots.
  start_listening(timeout_seconds = timeout_seconds)

  requests <- webmockr::request_registry()
  n_responses <- requests$times_executed(
    webmockr::RequestPattern$new("post", response_endpoint)
  )

  n_responses >= 1
}

