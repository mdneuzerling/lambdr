extract_event_headers <- function(event) {
  event_headers <- httr::headers(event)
  names(event_headers) <- tolower(names(event_headers))
  logger::log_debug("Event headers:", prettify_list(event_headers))
  event_headers
}

extract_request_id_from_headers <- function(headers) {
  if (!("lambda-runtime-aws-request-id" %in% names(headers))) {
    error_message <- paste(
      "Event doesn't contain request ID",
      "Can't clear this request from the queue."
    )
    stop_missing_request_id(error_message)
  }
  headers[["lambda-runtime-aws-request-id"]]
}

assert_status_code_is_good <- function(status_code) {
  logger::log_debug("Status code:", status_code)
  if (status_code != 200) {
    error_message <- paste("Didn't get status code 200. Status code:",
                           status_code)
    stop_lambda(error_message, code = status_code)
  }
  TRUE
}

parse_event_content <- function(event) {
  # we need to parse the event in four contexts before sending to the lambda fn:
  # 1a) direct invocation with no function args (empty event)
  # 1b) direct invocation with function args (parse and send entire event)
  # 2a) api endpoint with no args (parse HTTP request, confirm null request
  #   element; send empty list)
  # 2b) api endpoint with args (parse HTTP request, confirm non-null request
  #   element; extract and send it)

  unparsed_content <- httr::content(event, "text", encoding = "UTF-8")

  # Thank you to Menno Schellekens for this fix for Cloudwatch events
  # I'm getting conflicting information about whether or not scheduled events
  # can have input. This needs further research, or really a reprex.
  is_scheduled_event <- grepl("Scheduled Event", unparsed_content)
  logger::log_debug("Unparsed content:", unparsed_content)

  if (unparsed_content == "" || is_scheduled_event) {
    event_body <- list()
  } else {
    event_body <- jsonlite::fromJSON(unparsed_content)
  }

  http_request_element <- "queryStringParameters"
  request_type <- if (http_request_element %in% names(event_body)) {
    "HTML"
  } else if (is_scheduled_event) {
    "scheduled"
  } else {
    "direct"
  }
  logger::log_debug("Request type:", request_type)

  if (request_type == "HTML") {
    event_body <- event_body[[http_request_element]]
    if (is.null(event_body)) {
      event_body <- list()
    }
  }

  list(arguments = event_body, request_type = request_type)
}

wait_for_event <- function(endpoint) {
  logger::log_debug("Waiting for event")
  event <- httr::GET(url = get_next_invocation_endpoint())
  logger::log_debug("Event received")

  tryCatch(
    handle_event(event),
    missing_request_id = handle_missing_request_id,
    api_error = handle_api_error(event),
    error = handle_invocation_error(event)
  )
}

handle_event <- function(event) {

  event_headers <- extract_event_headers(event)
  request_id <- extract_request_id_from_headers(event_headers)
  status_code <- httr::status_code(event)
  assert_status_code_is_good(status_code)

  # According to the AWS guide, we need to set the trace ID as an env var
  # https://docs.aws.amazon.com/lambda/latest/dg/runtimes-custom.html
  runtime_trace_id <- event_headers[["lambda-runtime-trace-id"]]
  if (!is.null(runtime_trace_id)) {
    Sys.setenv("_X_AMZN_TRACE_ID" = runtime_trace_id)
  }

  event_content <- parse_event_content(event)
  event_arguments <- event_content$arguments
  request_type <- event_content$request_type

  result <- do.call(lambda$handler, event_arguments)
  logger::log_debug("Result:", as.character(result))

  # AWS API Gateway is a bit particular about the response format
  body <- if (request_type == "HTML") {
    list(
      isBase64Encoded = FALSE,
      statusCode = 200L,
      body = as.character(jsonlite::toJSON(result, auto_unbox = TRUE))
    )
  } else {
    as.character(jsonlite::toJSON(result, auto_unbox = TRUE))
  }

  httr::POST(
    url = get_response_endpoint(request_id),
    body = body,
    encode = "raw"
  )
}

start_listening <- function(timeout_seconds = NULL) {
  if (!is.null(timeout_seconds)) {
    expire_after <- Sys.time() + timeout_seconds
    while (Sys.time() < expire_after) {
      wait_for_event()
    }
  } else {
    while (TRUE) wait_for_event()
  }
}
