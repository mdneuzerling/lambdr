# error handling with http codes
# from http://adv-r.had.co.nz/Exceptions-Debugging.html
condition <- function(subclass, message, code, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, code = code, call = call),
    ...
  )
}

stop_missing_request_id <- function(message) {
  stop(
    condition(
      c("missing_request_id", "error"),
      message,
    )
  )
}

handle_missing_request_id <- function(e) {
  error_message <- paste("lambda-runtime-aws-request-id not found.",
                         "Can't process this request.")
  logger::log_error(error_message)
  stop(error_message)
}

stop_lambda <- function(message, code = 500, call = sys.call(-1), ...) {
  stop(
    condition(
      c("api_error", "error"),
      message,
      code = code,
      call = call,
      ...
    )
  )
}

handle_api_error <- function(event) {
  headers <- extract_event_headers(headers)
  request_id <- extract_request_id_from_headers(headers)
  logger::log_debug("POSTing invocation error for ID:", request_id)

  error_handling_function <- function(e) {
    logger::log_error(as.character(e))
    httr::POST(
      url = get_invocation_error_endpoint(request_id),
      body = list(
        statusCode = e$code,
        errorMessage = as.character(e$message)
      ),
      encode = "json"
    )
  }

  error_handling_function
}

handle_invocation_error <- function(event) {
  headers <- extract_event_headers(headers)
  request_id <- extract_request_id_from_headers(headers)

  error_handling_function <- function(e) {
    error_message <- as.character(e)
    logger::log_error(error_message)
    logger::log_debug("POSTing invocation error for request ID:", request_id)
    httr::POST(
      url = get_invocation_error_endpoint(request_id),
      body = list(error_message = error_message),
      encode = "json"
    )
  }

  error_handling_function
}
