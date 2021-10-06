# From https://docs.aws.amazon.com/lambda/latest/dg/lambda-dg.pdf:
#
# Processing tasks
# * Get an event – Call the next invocation (p. 187) API to get the next event.
#   The response body contains the event data. Response headers contain the
#   request ID and other information.
# * Propagate the tracing header – Get the X-Ray tracing header from the
#   Lambda-Runtime-Trace-Id header in the API response. Set the _X_AMZN_TRACE_ID
#   environment variable locally with the same value. The X-Ray SDK uses this
#   value to connect trace data between services.
# * Create a context object – Create an object with context information from
#   environment variables and headers in the API response.
# * Invoke the function handler – Pass the event and context object to the
#   handler.
# * Handle the response – Call the invocation response (p. 188) API to post the
#   response from the handler.
# * Handle errors – If an error occurs, call the invocation error (p. 189) API.
# * Cleanup – Release unused resources, send data to other services, or perform
#   additional tasks before getting the next event.


#' Extract the headers from a Lambda event
#'
#' This function is largely equivalent to \code{\link[httr]{headers}}, which it
#' wraps. The only difference is that the names of the headers returned are
#' converted to lower-case (these are meant to be case-insensitive) and the
#' headers are logged at the DEBUG level.
#'
#' @inheritParams parse_event_content
#'
#' @keywords internal
extract_event_headers <- function(event) {
  event_headers <- httr::headers(event)
  names(event_headers) <- tolower(names(event_headers))
  logger::log_debug("Event headers:", prettify_list(event_headers))
  event_headers
}

#' Extract the context of a Lambda invocation from the headers of an event
#'
#' @section Event context:
#' The _context_ of an event is a list of metadata about the invocation. It is
#' derived from the headers of a next event invocation response. It consists of:
#'
#' * `aws_request_id` - The identifier of the invocation request
#' * `invoked_function_arn` – The Amazon Resource Name (ARN) that's used to
#'   invoke the function. Indicates if the invoker specified a version number or
#'   alias.
#'
#' If the handler function accepts a `context` argument then it will
#' automatically receive at runtime a named list consisting of these values
#' along with the arguments in the body (if any). For example, a function such
#' as `my_func(x, context)` will receive the context argument automatically.
#' The `context` argument must be named (`...` will not work).
#'
#' @inheritParams parse_event_content
#'
#' @return list
#' @keywords internal
extract_context <- function(event_headers) {
  list(
    aws_request_id = event_headers[["lambda-runtime-aws-request-id"]],
    invoked_function_arn = event_headers[["lambda-runtime-invoked-function-arn"]]
  )
}

#' Extract the request ID from the headers of an event, or error otherwise
#'
#' The Request ID is unique for each input of a Lambda. It is carried by the
#' "lambda-runtime-aws-request-id" header of the response from the next
#' invocation endpoint (see \code{\link{endpoints}}).
#'
#' @param headers headers of a HTML response, as extracted by
#'   \code{\link[httr]{headers}} or \code{\link{extract_event_headers}}
#'
#' @return character
#' @keywords internal
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

#' Check that the status code shows a success, and error otherwise
#'
#' @param status_code integer, usually returned by
#'   \code{\link[httr]{status_code}}
#'
#' @return TRUE
#'
#' @keywords internal
assert_status_code_is_good <- function(status_code) {
  logger::log_debug("Status code:", status_code)
  if (status_code != 200) {
    error_message <- paste(
      "Didn't get status code 200. Status code:",
      status_code
    )
    stop_lambda(error_message, code = status_code)
  }
  TRUE
}

#' Parse the body of the Lambda event
#'
#' @param event the response received from querying the next invocation
#'   endpoint.
#' @param deserialiser function for deserialising the body of the event.
#'   By default, will attempt to deserialise the body as JSON, based on whether
#'   the input is coming from an API Gateway, scheduled Cloudwatch event, or
#'   direct. To use the body as is, pass the `identity` function.
#'
#' @return A list containing the "arguments" and "content_type", the latter of
#'   which is either "HTML", "scheduled", or "direct". The content type may be
#'   used in serialising the response to be sent back to Lambda.
#'
#' @keywords internal
parse_event_content <- function(event, deserialiser = NULL) {
  # we need to parse the event in four contexts before sending to the lambda fn:
  # 1a) direct invocation with no function args (empty event)
  # 1b) direct invocation with function args (parse and send entire event)
  # 2a) api endpoint with no args (parse HTTP request, confirm null request
  #   element; send empty list)
  # 2b) api endpoint with args (parse HTTP request, confirm non-null request
  #   element; extract and send it)

  unparsed_content <- httr::content(event, "text", encoding = "UTF-8")
  logger::log_debug("Raw body:", unparsed_content)

  if (!is.null(deserialiser)) {
    return(
      list(
        arguments = deserialiser(unparsed_content),
        request_type = "custom"
      )
    )
  }

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
  logger::log_debug("Parsed event body:", event_body)

  list(arguments = event_body, request_type = request_type)
}

#' Serialise a result and send it to Lambda
#'
#' @param result result to be sent back to Lambda
#' @param request_id character request ID, as extracted by
#'   \code{\link{extract_request_id_from_headers}} from the headers of an event.
#' @param request_type one of "HTML", "scheduled", or "direct". The content type
#'   may be used in serialising the response to be sent back to Lambda.
#' @param serialiser function for serialising the result before sending.
#'   By default, will attempt to serialise the body as JSON, based on the
#'   request type. To send the result as is, pass the `identity` function.
#'
#' @keywords internal
post_result <- function(result, request_id, request_type, serialiser = NULL) {
  logger::log_debug("Raw result:", result)
  body <- if (!is.null(serialiser)) {
    serialiser(result)
    # AWS API Gateway is a bit particular about the response format
  } else if (request_type == "HTML") {
    list(
      isBase64Encoded = FALSE,
      statusCode = 200L,
      body = as.character(jsonlite::toJSON(result, auto_unbox = TRUE))
    )
  } else {
    as.character(jsonlite::toJSON(result, auto_unbox = TRUE))
  }

  logger::log_debug("Result to be posted:", body)
  httr::POST(
    url = get_response_endpoint(request_id),
    body = body,
    encode = "raw"
  )
}

#' Process the input of an event, and submit the result to Lambda
#'
#' If the handler function accepts a named `context` argument then the Lambda
#' invocation context will be included as an argument. See the section below for
#' more details.
#'
#' @inheritSection extract_context Event context
#'
#' @inheritParams parse_event_content
#' @inheritParams post_result
#'
#' @keywords internal
handle_event <- function(event, deserialiser = NULL, serialiser = NULL) {
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

  event_content <- parse_event_content(event, deserialiser)
  event_arguments <- event_content$arguments

  # if the handler function accepts either a `context` argument then calculate
  # the event context and append it to the function arguments.
  if (lambda$pass_context_argument) {
    event_arguments <- c(
      event_arguments,
      list(context = extract_context(event_headers))
    )
  }
  request_type <- event_content$request_type

  result <- do.call(lambda$handler, args = event_arguments)
  logger::log_debug("Result:", as.character(result))

  post_result(result, request_id, request_type, serialiser = serialiser)
}
