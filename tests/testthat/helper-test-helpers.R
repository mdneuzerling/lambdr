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

basic_lambda_config <- function(handler = "sqrt",
                                runtime_api = "red_panda",
                                task_root = "giraffe",
                                direct_handler = NULL,
                                log_threshold = test_debug_level,
                                ...) {
  setup_logging(log_threshold = log_threshold)

  # The handler is configured via an environment variable, UNLESS direct_handler
  # is provided, in which it's passed through to `lambda_config`
  env_vars <- if (is.null(direct_handler)) {
    c(
      "AWS_LAMBDA_RUNTIME_API" = runtime_api,
      "LAMBDA_TASK_ROOT" = task_root,
      "_HANDLER" = handler
    )
  } else {
    c(
      "AWS_LAMBDA_RUNTIME_API" = runtime_api,
      "LAMBDA_TASK_ROOT" = task_root
    )
  }

  withr::with_envvar(env_vars,
    lambda_config(handler = direct_handler, environ = parent.frame(), ...)
  )
}

mock_response <- function(input,
                          expected_response_body,
                          expected_response_headers = default_response_headers,
                          request_id = "abc123",
                          timeout_seconds = 0.5,
                          config = basic_lambda_config()) {

  # Make webmockr intercept HTTP requests
  webmockr::enable(quiet = TRUE)
  withr::defer(webmockr::disable(quiet = TRUE))

  invocation_endpoint <- get_next_invocation_endpoint(config)
  response_endpoint <- get_response_endpoint(config, request_id)

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

  start_listening(
    config = config,
    timeout_seconds = timeout_seconds
  )

  requests <- webmockr::request_registry()
  n_responses <- requests$times_executed(
    webmockr::RequestPattern$new("post", response_endpoint)
  )

  n_responses >= 1
}

mock_invocation_error <- function(input,
                                  expected_error_body,
                                  request_id = "abc123",
                                  timeout_seconds = 0.5,
                                  config = basic_lambda_config()) {

  # Make webmockr intercept HTTP requests
  webmockr::enable(quiet = TRUE)
  withr::defer(webmockr::disable(quiet = TRUE))

  invocation_endpoint <- get_next_invocation_endpoint(config)
  invocation_error_endpoint <- get_invocation_error_endpoint(config, request_id)

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
        "Accept" = "application/json, text/xml, application/xml, */*",
        "Content-Type" = "application/vnd.aws.lambda.error+json"
      ),
      body = expected_error_body
    ) %>%
    webmockr::to_return(status = 202) # accepted

  start_listening(config = config, timeout_seconds = timeout_seconds)

  requests <- webmockr::request_registry()
  n_responses <- requests$times_executed(
    webmockr::RequestPattern$new("post", invocation_error_endpoint)
  )

  n_responses >= 1
}

trigger_initialisation_error <- function(expected_error,
                                         task_root = "giraffe",
                                         handler = "sqrt",
                                         direct_handler = NULL,
                                         deserialiser = NULL,
                                         serialiser = NULL) {

  # Make webmockr intercept HTTP requests
  webmockr::enable(quiet = TRUE)
  withr::defer(webmockr::disable(quiet = TRUE))

  lambda_runtime_api <- "red_panda"
  request_id <- "abc123"
  invocation_endpoint <- paste0(
    "http://", lambda_runtime_api, "/2018-06-01/runtime/invocation/next"
  )
  initialisation_error_endpoint <- paste0(
    "http://", lambda_runtime_api, "/2018-06-01/runtime/init/error"
  )

  # Mock the invocation to return a Lambda input with mock request ID and input
  webmockr::stub_request("get", invocation_endpoint) %>%
    webmockr::to_return(
      body = list(), # input is irrelevant here --- it's never used
      headers = list("lambda-runtime-aws-request-id" = request_id),
      status = 200
    )

  # Mock the initialisation error endpoint.
  webmockr::stub_request("post", initialisation_error_endpoint) %>%
    webmockr::wi_th(
      headers = list(
        "Accept" = "application/json, text/xml, application/xml, */*",
        "Content-Type" = "application/vnd.aws.lambda.error+json"
      ),
      body = as_json(
        list(
          errorMessage = expected_error,
          errorType = "simpleError",
          stackTrace = list()
        )
      )
    ) %>%
    webmockr::to_return(status = 202) # accepted

  # Failure when task_root not set-up
  error_message <- tryCatch(
    basic_lambda_config(
      runtime_api = "red_panda",
      task_root = task_root,
      handler = handler,
      direct_handler = direct_handler,
      deserialiser = deserialiser,
      serialiser = serialiser
    ),
    error = function(e) e$message
  )

  if (error_message != expected_error) {
    stop(error_message)
  }

  requests <- webmockr::request_registry()
  request_pattern <- webmockr::RequestPattern$new(
    "post",
    initialisation_error_endpoint
  )
  return(requests$times_executed(request_pattern) == 1)
}
