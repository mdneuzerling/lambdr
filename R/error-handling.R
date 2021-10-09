#' Post an error to an endpoint with the format expected by AWS Lambda
#'
#' @description
#' According to the
#' \href{https://docs.aws.amazon.com/lambda/latest/dg/lambda-dg.pdf}{AWS Lambda
#' Developer Guide} an error posted to the initialisation or invocation error
#' endpoints must be of content type `application/vnd.aws.lambda.error+json`
#' and of the format:
#'
#' ```json
#' {
#'   "errorMessage": "...",
#'   "errorType": "...",
#'   "stackTrace": [],
#' }
#' ```
#'
#' Here the `errorMessage` and `errorType` are strings, with the `stackTrace`
#' a list of strings.
#'
#' This function accepts an error as caught by  \code{\link[base]{tryCatch}} and
#' posts it to the given endpoint, ensuring that the correct formatting is
#' adhered to. This function does not stop the runtime as in the case of
#' invocation errors it's desirable to continue.
#'
#' The stacktrace is not currently reported. This functionality is yet to be
#' implemented. See the
#' \href{https://github.com/mdneuzerling/lambdr/issues/5}{GitHub issue} for more
#' details.
#'
#' @param e an error as caught by \code{\link[base]{tryCatch}}
#' @param endpoint where to \code{\link[httr]{POST}} the error
#'
#' @keywords internal
post_lambda_error <- function(e, endpoint) {
  logger::log_error(e$message)
  error_list <- list(
    errorMessage = e$message,
    errorType = class(e)[[1]],
    stackTrace = list()
  )
  error_json <- as_json(error_list)
  httr::POST(
    url = endpoint,
    body = error_json,
    encode = "raw",
    httr::content_type("application/vnd.aws.lambda.error+json")
  )
}

#' Define a condition (like an error) with support for HTTP status codes
#'
#' For more information on conditions and errors see
#' \url{http://adv-r.had.co.nz/Exceptions-Debugging.html}. See also
#' \code{\link[base]{conditions}}.
#'
#' @param subclass conditions returned by this function will be of the class
#' `c(subclass, "simpleError", "error", "condition")`.
#' @param message character message, such as an error description, to include
#'   with the condition.
#' @param code HTTP status code to return (if applicable). Defaults to `500`,
#'   which is a generic "Internal Server Error". Not currently used anywhere.
#' @param call call expression
#' @param ...attributes, specified in `tag = value` form, which will be attached
#'   to the condition
#'
#' @keywords internal
condition <- function(subclass, message, code = 500L, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "simpleError", "error", "condition"),
    list(message = message, code = code, call = call),
    ...
  )
}

#' Raise an error when there's a missing request ID
#'
#' @description
#' A missing Request ID is an unusual error. It occurs after the runtime has
#' been initialised, and is _technically_ an invocation error. However it's
#' impossible to post the error to the invocation error endpoint as that
#' requires a Request ID.
#'
#' We define a separate condition for it here, such that instead of `stop` we
#' can use `stop_missing_request_id`. This allows us to catch and treat the
#' error in a unique way. The `handle_missing_request_id` function accepts an
#' error caught by \code{\link[base]{tryCatch}}, logs it, and then returns
#' a standard error with a sensible message.
#'
#' @inheritParams condition
#'
#' @keywords internal
stop_missing_request_id <- function(message) {
  stop(
    condition(
      c("missing_request_id", "error"),
      message,
    )
  )
}

#' @param e error caught by \code{\link[base]{tryCatch}}
#' @keywords internal
#' @rdname stop_missing_request_id
handle_missing_request_id <- function(e) {
  error_message <- paste(
    "lambda-runtime-aws-request-id header not found.",
    "Can't process this request."
  )
  logger::log_error(error_message)
  stop(error_message)
}

#' Generate a handling function for an invocation error
#'
#' @description
#' An error caught during event handling must be handled in a special way. An
#' error message must be logged and posted to the invocation error endpoint,
#' and the the runtime must continue --- an invocation error is a problem for
#' the invocation, not the runtime.
#'
#' The `handle_invocation_error` function accepts an event and generates a
#' function. The generated function accepts error caught by
#' \code{\link[base]{tryCatch}}, logs it, and then submits it to the invocation
#' error endpoint. Importantly it does not stop the kernel --- the intention is
#' that the runtime moves onto the next event.
#'
#' ```r
#' tryCatch(
#'     handle_event(...),
#'     error = handle_invocation_error(event) # returns a function(e)
#'  )
#' ```
#'
#' @param e error caught by \code{\link[base]{tryCatch}}
#'
#' @keywords internal
handle_invocation_error <- function(event) {
  event_headers <- extract_event_headers(event)
  request_id <- extract_request_id_from_headers(event_headers)

  error_handling_function <- function(e) {
    error_message <- as.character(e)
    logger::log_error(error_message)
    logger::log_debug("POSTing invocation error for request ID:", request_id)
    post_lambda_error(e, endpoint = get_invocation_error_endpoint(request_id))
  }

  error_handling_function
}
