# Content-Type: application/vnd.aws.lambda.error+json:
# {
#     "errorMessage": "...",
#     "errorType": "...",
#     "stackTrace": [],
# }
post_lambda_error <- function(e, endpoint) {
  error_list <- list(
    errorMessage = e$message,
    errorType = class(e)[[1]],
    stackTrace = c()
  )
  error_json <- jsonlite::toJSON(error_list, auto_unbox = TRUE)
  httr::POST(
    url = endpoint,
    body = error_json,
    encode = "raw",
    httr::content_type("application/vnd.aws.lambda.error+json")
  )
}

# error handling with http codes
# from http://adv-r.had.co.nz/Exceptions-Debugging.html
condition <- function(subclass, message, code = NULL, call = sys.call(-1), ...) {
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
  event_headers <- extract_event_headers(event)
  request_id <- extract_request_id_from_headers(event_headers)
  logger::log_debug("POSTing invocation error for ID:", request_id)

  error_handling_function <- function(e) {
    logger::log_error(as.character(e))
    post_lambda_error(e, endpoint = get_invocation_error_endpoint(request_id))
  }

  error_handling_function
}

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
