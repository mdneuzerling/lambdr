#' Query the next invocation endpoint to get the next input
#'
#' The query will receive a response when an input is queued up. If there is no
#' input waiting, the Lambda instance will be shut down after a period of
#' inactivity.
#'
#' @inheritParams parse_event_content
#' @inheritParams post_result
#'
#' @keywords internal
wait_for_event <- function(deserialiser = NULL,
                           serialiser = NULL) {
  logger::log_debug("Waiting for event")
  event <- httr::GET(url = get_next_invocation_endpoint())
  logger::log_debug("Event received")

  tryCatch(
    handle_event(event, deserialiser = deserialiser, serialiser = serialiser),
    missing_request_id = handle_missing_request_id,
    api_error = handle_api_error(event),
    error = handle_invocation_error(event)
  )
}

#' Start listening for events, and process them as they come
#'
#' @inheritParams parse_event_content
#' @inheritParams post_result
#' @param timeout_seconds If set, the function will stop listening for events
#' after this timeout. The timeout is checked between events, so this won't
#' interrupt the function while it is waiting for a new event. This argument
#' is provided for testing purposes, and shouldn't otherwise need to be set:
#' AWS should handle the shutdown of idle Lambda instances.
#'
#' @export
start_listening <- function(deserialiser = NULL,
                            serialiser = NULL,
                            timeout_seconds = NULL) {
  assert_lambda_is_setup()

  if (!is.null(timeout_seconds)) {
    expire_after <- Sys.time() + timeout_seconds
    while (Sys.time() < expire_after) {
      wait_for_event(
        deserialiser = deserialiser,
        serialiser = serialiser
      )
    }
  } else {
    while (TRUE) wait_for_event()
  }
}
