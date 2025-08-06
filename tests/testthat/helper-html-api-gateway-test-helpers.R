mock_html_api_gateway_event <- function(query_parameters = NULL,
                                        body_parameters = NULL,
                                        result,
                                        expected_response_headers = default_response_headers,
                                        expect_result_as_is = FALSE,
                                        request_id = "abc123",
                                        config = basic_lambda_config(),
                                        timeout_seconds = 0.5) {
  api_gateway_event_body <- mock_html_api_gateway_event_body(
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

  requests <- webmockr::request_registry()
  request_received("post", response_endpoint)
}

mock_html_api_gateway_event_body <- function(query_parameters = NULL,
                                             body_parameters = NULL) {
  # path_arameters ?
  rawQueryString <- ""
  for (i in seq_along(query_parameters)) {
    rawQueryString <- add_parameter(
      rawQueryString,
      names(query_parameters[i]),
      as.character(query_parameters[[i]])
    )
  }
  rawQueryString <- gsub("^\\?", "", rawQueryString)
  body <- jsonlite::base64_enc(as_stringified_json(body_parameters))

  queryStringParameters <- lapply(
    query_parameters,
    function(x) paste(as.character(x), collapse = ",")
  )

  as_json(
    list(
      version = "2.0",
      routeKey = "ANY /parity",
      rawPath = "/default/parity",
      rawQueryString = rawQueryString,
      queryStringParameters = queryStringParameters,
      headers = list(
        "accept" = "*/*",
        "content-length" = "12",
        "content-type" = "application/x-www-form-urlencoded",
        "host" = "abcdefghi.execute-api.ap-southeast-2.amazonaws.com",
        "user-agent" = "curl/7.64.1",
        "x-amzn-trace-id" = "Root=1-6167f9fb-1ada874811eaf2bc1464c679",
        "x-forwarded-for" = "192.168.1.1",
        "x-forwarded-port" = "443",
        "x-forwarded-proto" = "https"
      ),
      requestContext = list(
        "accountId" = "123456789",
        "apiId" = "abcdefghi",
        "domainName" = "abcdefghi.execute-api.ap-southeast-2.amazonaws.com",
        "domainPrefix" = "abcdefghi",
        "http" = list(
          "method" = "POST",
          "path" = "/default/parity",
          "protocol" = "HTTP/1.1",
          "sourceIp" = "192.168.1.1",
          "userAgent" = "curl/7.64.1"
        ),
        "requestId" = "HMP_QiusywMEPEg=",
        "routeKey" = "ANY /parity",
        "stage" = "default",
        "time" = "14/Oct/2021:09:35:55 +0000",
        "timeEpoch" = 1634204155055
      ),
      body = body,
      isBase64Encoded = TRUE
    ),
    pretty = TRUE,
    na = "null"
  )
}
