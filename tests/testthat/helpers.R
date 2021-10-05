parity <- function(number) {
  list(parity = if (as.integer(number) %% 2 == 0) "even" else "odd")
}

no_arguments <- function() {
  list(animal = "dog", breed = "corgi")
}

expect_setup_failure <- function(endpoint_function, ...) {
  eval(bquote({
    expect_error(
      endpoint_function(...),
      "The AWS Lambda runtime is not configured"
    )
  }))
}

use_basic_lambda_setup <- function(handler = "sqrt") {
  setup_logging(log_threshold = logger::FATAL)
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
  timeout_seconds = 0.5
  ) {

  # Make webmockr intercept HTTP requests
  webmockr::enable(quiet = TRUE)
  withr::defer(webmockr::disable(quiet = TRUE))

  invocation_endpoint <- get_next_invocation_endpoint()
  response_endpoint <- get_response_endpoint(request_id)

  # Mock the invocation to return a Lambda input with mock request ID and input
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

mock_invocation_error <- function(
  input,
  expected_error_body,
  request_id = "abc123",
  timeout_seconds = 0.5
) {

  # Make webmockr intercept HTTP requests
  webmockr::enable(quiet = TRUE)
  withr::defer(webmockr::disable(quiet = TRUE))

  invocation_endpoint <- get_next_invocation_endpoint()
  invocation_error_endpoint <- get_invocation_error_endpoint(request_id)

  # Mock the invocation to return a Lambda input with mock request ID and input
  webmockr::stub_request("get", invocation_endpoint) %>%
    webmockr::to_return(
      body = input,
      headers = list("lambda-runtime-aws-request-id" = request_id),
      status = 200
    )

  # Mock the response endpoint. We expect a response of `2`.
  webmockr::stub_request("post", invocation_error_endpoint) %>%
    webmockr::wi_th(
      headers = list(
        'Accept' = 'application/json, text/xml, application/xml, */*',
        'Content-Type' = 'application/vnd.aws.lambda.error+json'
      ),
      body = expected_error_body
    ) %>%
    webmockr::to_return(status = 202) # accepted

  start_listening(timeout_seconds = timeout_seconds)

  requests <- webmockr::request_registry()
  n_responses <- requests$times_executed(
    webmockr::RequestPattern$new("post", invocation_error_endpoint)
  )

  n_responses >= 1
}
