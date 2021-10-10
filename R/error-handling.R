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
  error_response <- list(
    errorMessage = e$message,
    errorType = class(e)[[1]],
    stackTrace = list()
  )

  httr::POST(
    url = endpoint,
    body = as_stringified_json(error_response),
    encode = "raw",
    httr::content_type("application/vnd.aws.lambda.error+json")
  )
}

#' Generate a handling function for an invocation error
#'
#' @description
#' An error caught during event handling must be handled in a special way. An
#' error message must be logged and posted to the invocation error endpoint,
#' and the the runtime must continue --- an invocation error is a problem for
#' the invocation, not the runtime.
#'
#' The `handle_event_error` function accepts an event and generates a
#' function. The generated function accepts error caught by
#' \code{\link[base]{tryCatch}}, logs it, and then submits it to the invocation
#' error endpoint. Importantly it does not stop the kernel --- the intention is
#' that the runtime moves onto the next event.
#'
#' This function may need to be implemented differently depending on the source
#' of an event. As such, `handle_event_error` is an S3 generic that can dispatch
#' on the event class as returned by \code{\link{classify_event}}.
#'
#' ```r
#' tryCatch(
#'     handle_event(...),
#'     error = handle_invocation_error(event) # returns a function(e)
#'  )
#' ```
#'
#' @return A function that accepts an error `e` as caught by
#'   \code{\link[base]{tryCatch}}
#'
#' @keywords internal
#' @export
handle_event_error <- function(event, ...) {
  logger::log_debug("Preparing invocation error for request ID:", event$request_id)
  UseMethod("handle_event_error")
}

#' @export
handle_event_error.default <- function(event, ...) {
  error_handling_function <- function(e) {
    post_lambda_error(e, endpoint = get_invocation_error_endpoint(event$request_id))
  }

  error_handling_function
}


#' Handle an error that occurs when decomposing an invocation into an event
#'
#' This function is largely similar to \code{\link{handle_event_error}}. Its
#' purpose is to catch errors that occur between identifying the request ID
#' of an invocation and handling the event it defines. As such, it takes a
#' `request_id` argument rather than an `event argument`.
#'
#' @inheritParams endpoints
#'
#' @inherit handle_event_error return
#'
#' @keywords internal
handle_decomposition_error <- function(request_id) {
  error_handling_function <- function(e) {
    post_lambda_error(e, endpoint = get_invocation_error_endpoint(request_id))
  }
  error_handling_function
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
#'   which is a generic "Internal Server Error". This is used when errors are to
#'   be returned to an API Gateway.
#' @param call call expression
#' @param ...attributes, specified in `tag = value` form, which will be attached
#'   to the condition
#'
#' @keywords internal
condition <- function(subclass, message, code = 500L, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "error", "condition"),
    list(message = message, code = code, call = call),
    ...
  )
}

################################################################################
############################## API Gateway errors ##############################
################################################################################

#' Raise an error with an optional HTML status code for API Gateways
#'
#' @description
#' This variation of `stop` can be used to raise an error with a specific error
#' code. This is provided to the API Gateway to return an appropriate response.
#' It had no use outside of invocations via an API Gateway.
#'
#' If a status code is not provided, a generic "500" internal server error will
#' be used.
#'
#' @inheritParams condition
#'
#' @export
#'
#' @examples \dontrun{
#' stop_html("Resource doesn't exist", code = 404L)
#' }
stop_html <- function(message, code = 500L) {
  stop(
    condition(
      c("html_error", "error"),
      message,
      code = code
    )
  )
}
