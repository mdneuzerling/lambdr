mock_rest_api_gateway_event <- function(query_parameters = NULL,
                                        body_parameters = NULL,
                                        result,
                                        expected_response_headers = default_response_headers,
                                        expect_result_as_is = FALSE,
                                        request_id = "abc123",
                                        config = basic_lambda_config(),
                                        timeout_seconds = 0.5) {
  api_gateway_event_body <- mock_rest_api_gateway_event_body(
    query_parameters,
    body_parameters
  )

  # Make webmockr intercept HTTP requests
  webmockr::enable(quiet = TRUE)
  withr::defer(webmockr::disable(quiet = TRUE))

  invocation_endpoint <- get_next_invocation_endpoint(config)
  response_endpoint <- get_response_endpoint(config, request_id)

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

  start_listening(config = config, timeout_seconds = timeout_seconds)

  request_received("post", response_endpoint)
}

mock_rest_api_gateway_event_body <- function(query_parameters = NULL,
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
