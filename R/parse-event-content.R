#' Parse the body of the Lambda event
#'
#' @inheritSection is_from_api_gateway Invocations via an API Gateway
#'
#' @param event_content the content of the response received from querying the
#'   text invocation endpoint, as a character
#' @param deserialiser function for deserialising the body of the event.
#'   By default, will attempt to deserialise the body as JSON, based on whether
#'   the input is coming from an API Gateway, scheduled Cloudwatch event, or
#'   direct. To use the body as is, pass the `identity` function. If input is
#'   coming via an API Gateway this will require some complicated parsing (see
#'   below).
#'
#' @return A list containing the arguments to be passed to the handler function
#'
#' @keywords internal
#' @export
parse_event_content <- function(event, deserialiser = NULL) {
  if (!is.null(deserialiser)) {
    return(deserialiser(event$event_content))
  }
  UseMethod("parse_event_content")
}

#' @export
parse_event_content.default <- function(event, ...) {
  parse_json_or_empty(event$event_content)
}
