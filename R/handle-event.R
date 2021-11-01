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

#' Parse the content of an event and pass it through the handler function
#'
#' @inheritParams handle_event
#' @inheritParams validate_lambda_config
#' @inheritParams parse_event_content
#'
#' @return An object of class the same as `event`. The object contains a
#'   `result` value, and the `result_calculated` attribute is set to `TRUE`.
#'
#' @keywords internal
generate_result <- function(event,
                            config = lambda_config(),
                            deserialiser = NULL) {
  parsed_event_content <- parse_event_content(event, config)
  logger::log_debug("Parsed event body:", prettify_list(parsed_event_content))

  # if the handler function accepts either a `context` argument then calculate
  # the event context and append it to the function arguments.
  if (config$pass_context_argument) {
    event_arguments <- c(
      parsed_event_content,
      list(context = extract_and_augment_context(event, config))
    )
  } else {
    event_arguments <- parsed_event_content
  }

  result <- do.call(config$handler, args = event_arguments)
  logger::log_debug("Result:", as.character(result))

  # NULL is a valid result, so we track whether this event has had its result
  # calculated with the `result_calculated` flag
  structure(
    c(event, list(result = result)),
    class = class(event),
    result_calculated = TRUE
  )
}

#' Process the input of an event, and submit the result to Lambda
#'
#' @description
#' If the handler function accepts a named `context` argument then the Lambda
#' invocation context will be included as an argument. See the section below for
#' more details.
#'
#' @inheritSection is_from_rest_api_gateway Invocations via an API Gateway
#'
#' @param event the response received from querying the next invocation
#'   endpoint.
#' @inheritParams validate_lambda_config
#'
#' @inheritSection extract_context Event context
#'
#' @keywords internal
handle_event <- function(event,
                         config = lambda_config()) {

  # According to the AWS guide, we need to set the trace ID as an env var
  # https://docs.aws.amazon.com/lambda/latest/dg/runtimes-custom.html
  runtime_trace_id <- event$event_headers[["lambda-runtime-trace-id"]]
  if (!is.null(runtime_trace_id)) {
    Sys.setenv("_X_AMZN_TRACE_ID" = runtime_trace_id)
  }

  event_with_result <- generate_result(event, config)

  post_result(event_with_result, config)
}
