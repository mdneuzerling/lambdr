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
    stop("Didn't get status code 200. Status code: ", status_code)
  }
  TRUE
}

#' Extract the headers from a Lambda event
#'
#' This function is largely equivalent to \code{\link[httr]{headers}}, which it
#' wraps. The only difference is that the names of the headers returned are
#' converted to lower-case (these are meant to be case-insensitive) and the
#' headers are logged at the DEBUG level.
#'
#' @inheritParams handle_event
#'
#' @keywords internal
extract_event_headers <- function(event) {
  event_headers <- httr::headers(event)
  names(event_headers) <- tolower(names(event_headers))
  logger::log_debug("Event headers:", prettify_list(event_headers))
  event_headers
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
    stop(error_message)
  }
  headers[["lambda-runtime-aws-request-id"]]
}

#' Classify an event based on how it is invoked
#'
#' @description
#' Events need to be handled differently depending on how the Lambda is invoked.
#' For example, an event via an API Gateway needs to be parsed and handled
#' differently to that of an event received via direct invocation. This function
#' attempts to detect the method of invocation and returns a character vector
#' which can be used to assign an S3 class to the event. The last element of
#' the vector is always "event".
#'
#' @param event_content the content of the response received from querying the
#'   text invocation endpoint, as a character
#'
#' @return character vector, the last element of which is always "event"
#'
#' @keywords internal
classify_event <- function(event_content) {
  invocation_type <- if (is_from_api_gateway(event_content)) {
    "api_gateway_event"
  } else if (is_scheduled_event_content(event_content)) {
    "scheduled_event"
  } else {
    character()
  }
  c(invocation_type, "event")
}

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
wait_for_and_handle_event <- function(deserialiser = NULL, serialiser = NULL) {
  logger::log_debug("Waiting for event")

  # If an error occurs during the following tryCatch block it is impossible to
  # post it to the invocation error endpoint as that requires a Request ID.
  # We log the error and move on.
  tryCatch(
    {
      invocation <- httr::GET(url = get_next_invocation_endpoint())
      logger::log_debug("Event received")
      event_headers <- extract_event_headers(invocation)
      request_id <- extract_request_id_from_headers(event_headers)
      logger::log_debug("Request ID: ", request_id)
    },
    error = function(e) {
      logger::log_error(e$message)
      return(NULL)
    }
  )

  tryCatch(
    {
      status_code <- httr::status_code(invocation)
      assert_status_code_is_good(status_code)

      event_content <- httr::content(invocation, "text", encoding = "UTF-8")
      logger::log_debug("Raw event content:", event_content)

      event_classification <- classify_event(event_content)
      logger::log_debug("Event class:", event_classification[1])
    },
    error = function(e) {
      handle_decomposition_error(request_id)(e)
      return(NULL)
    }
  )

  event <- structure(
    list(
      request_id = request_id,
      status_code = status_code,
      event_headers = event_headers,
      event_content = event_content
    ),
    class = event_classification,
    # NULL is a valid result, so we track whether this event has had its result
    # calculated with the `result_calculated` flag
    result_calculated = FALSE
  )

  tryCatch(
    handle_event(event, deserialiser = deserialiser, serialiser = serialiser),
    error = function(e) {
      handle_event_error(event)(e)
    }
  )

  return(NULL)
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
      wait_for_and_handle_event(
        deserialiser = deserialiser,
        serialiser = serialiser
      )
    }
  } else {
    while (TRUE) {
      wait_for_and_handle_event(
        deserialiser = deserialiser,
        serialiser = serialiser
      )
    }
  }
}
