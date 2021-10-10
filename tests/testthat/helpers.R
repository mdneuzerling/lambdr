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

use_basic_lambda_setup <- function(handler = "sqrt",
                                   runtime_api = "red_panda",
                                   task_root = "giraffe",
                                   log_threshold = logger::FATAL) {
  setup_logging(log_threshold = log_threshold)
  withr::with_envvar(
    c(
      "AWS_LAMBDA_RUNTIME_API" = runtime_api,
      "LAMBDA_TASK_ROOT" = task_root,
      "_HANDLER" = handler
    ),
    withr::with_environment(
      parent.frame(),
      setup_lambda()
    )
  )
  withr::defer(reset_lambda(), envir = parent.frame())
}

mock_response <- function(input,
                          expected_response_body,
                          expected_response_headers = list(
                            "Accept" = "application/json, text/xml, application/xml, */*",
                            "Content-Type" = ""
                          ),
                          request_id = "abc123",
                          timeout_seconds = 0.5) {

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

mock_invocation_error <- function(input,
                                  expected_error_body,
                                  request_id = "abc123",
                                  timeout_seconds = 0.5) {

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
        "Accept" = "application/json, text/xml, application/xml, */*",
        "Content-Type" = "application/vnd.aws.lambda.error+json"
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

mock_initialisation_error <- function(expected_error_body,
                                      request_id = "abc123",
                                      timeout_seconds = 0.5) {

  # Make webmockr intercept HTTP requests
  webmockr::enable(quiet = TRUE)
  withr::defer(webmockr::disable(quiet = TRUE))

  invocation_endpoint <- get_next_invocation_endpoint()
  initialisation_error_endpoint <- get_initialisation_error_endpoint()

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
      body = expected_error_body
    ) %>%
    webmockr::to_return(status = 202) # accepted

  # start_listening(timeout_seconds = timeout_seconds)
  #
  # requests <- webmockr::request_registry()
  # n_responses <- requests$times_executed(
  #   webmockr::RequestPattern$new("post", initialisation_error_endpoint)
  # )
  #
  # n_responses >= 1
}

trigger_initialisation_error <- function(expected_error,
                                         task_root = "giraffe",
                                         handler = "sqrt") {

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
    use_basic_lambda_setup(
      runtime_api = "red_panda",
      task_root = task_root,
      handler = handler
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


mock_api_gateway_event <- function(query_parameters = NULL,
                                   body_parameters = NULL,
                                   result,
                                   expected_response_headers = list(
                                     "Accept" = "application/json, text/xml, application/xml, */*",
                                     "Content-Type" = ""
                                   ),
                                   expect_result_as_is = FALSE,
                                   request_id = "abc123",
                                   timeout_seconds = 0.5) {
  api_gateway_event_body <- mock_api_gateway_event_body(
    query_parameters,
    body_parameters
  )

  # Make webmockr intercept HTTP requests
  webmockr::enable(quiet = TRUE)
  withr::defer(webmockr::disable(quiet = TRUE))

  invocation_endpoint <- get_next_invocation_endpoint()
  response_endpoint <- get_response_endpoint(request_id)

  # Mock the invocation to return a Lambda input with mock request ID and input
  webmockr::stub_request("get", invocation_endpoint) %>%
    webmockr::to_return(
      body = api_gateway_event_body,
      headers = list("lambda-runtime-aws-request-id" = request_id),
      status = 200
    )

  expected_body <- if (expect_result_as_is) {
    result
  } else {
    as_json(
      list(
        isBase64Encoded = FALSE,
        statusCode = 200L,
        body = as_stringified_json(result)
      )
    )
  }

  # Mock the response endpoint. We expect a response of `2`.
  webmockr::stub_request("post", response_endpoint) %>%
    webmockr::wi_th(
      headers = expected_response_headers,
      body = expected_body
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

mock_api_gateway_event_body <- function(query_parameters = NULL,
                                        body_parameters = NULL) {
  # Bizarrely, the body needs to be a stringified JSON but the query parameters
  # need to be just a normal JSON
  as_json(
    list(
      resource = "/parity",
      path = "/parity",
      httpMethod = "POST",
      headers = list(
        accept = "*/*",
        Host = "abcdefghijk.execute-api.ap-southeast-2.amazonaws.com",
        "User-Agent" = "curl/7.64.1",
        "X-Amzn-Trace-Id" = "Root=1-615e4711-5f239aad2b046b5609e43b1c",
        "X-Forwarded-For" = "192.168.1.1",
        "X-Forwarded-Port" = "443",
        "X-Forwarded-Proto" = "https"
      ),
      multiValueHeaders = list(
        accept = list("*/*"),
        Host = list("abcdefghijk.execute-api.ap-southeast-2.amazonaws.com"),
        "User-Agent" = list("curl/7.64.1"),
        "X-Amzn-Trace-Id" = list("Root=1-615e4711-5f239aad2b046b5609e43b1c"),
        "X-Forwarded-For" = list("192.168.1.1"),
        "X-Forwarded-Port" = list("443"),
        "X-Forwarded-Proto" = list("https")
      ),
      "queryStringParameters" = query_parameters,
      "multiValueQueryStringParameters" = query_parameters, # should be boxed
      pathParameters = NULL,
      stageVariables = NULL,
      requestContext = list(
        resourceId = "abcdef",
        resourcePath = "/parity",
        httpMethod = "POST",
        extendedRequestId = "G0AKsFXISwMFsGA=",
        requestTime = "07/Oct/2021:01:02:09 +0000",
        path = "/test/parity",
        accountId = "1234567890",
        protocol = "HTTP/1.1",
        stage = "test",
        domainPrefix = "abcdefghijk",
        requestTimeEpoch = 1.633569e+12,
        requestId = "59bbb4c9-9d24-4cbb-941b-60dd4969e9c5",
        identity = list(
          cognitoIdentityPoolId = NULL,
          accountId = NULL,
          cognitoIdentityId = NULL,
          caller = NULL,
          sourceIp = "192.168.1.1",
          principalOrgId = NULL,
          accessKey = NULL,
          cognitoAuthenticationType = NULL,
          cognitoAuthenticationProvider = NULL,
          userArn = NULL,
          userAgent = "curl/7.64.1",
          user = NULL
        ),
        domainName = "abcdefghijk.execute-api.ap-southeast-2.amazonaws.com",
        apiId = "abcdefghijk"
      ),
      body = as_stringified_json(body_parameters),
      isBase64Encoded = FALSE
    ),
    pretty = TRUE
  )
}
