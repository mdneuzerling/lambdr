# error handling with http codes
# from http://adv-r.had.co.nz/Exceptions-Debugging.html
condition <- function(subclass, message, code, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, code = code, call = call),
    ...
  )
}
stop_api <- function(message, code = 500, call = sys.call(-1), ...) {
  stop(condition(c("api_error", "error"), message, code = code, call = call,
                 ...))
}


handle_invocation_error = function(request_id) {
  invocation_error_endpoint <- get_invocation_error_endpoint(
    request_id = request_id
  )

  error_handling_function <- function(e) {
    error_message <- as.character(e)
    logger::log_error(error_message)
    logger::log_debug("POSTing invocation error for request ID:", request_id)
    httr::POST(
      url = invocation_error_endpoint,
      body = list(error_message = error_message),
      encode = "json"
    )
  }

  error_handling_function
}
