#' Determine if a Lambda event is coming via an API Gateway
#'
#' @inheritSection is_from_rest_api_gateway Invocations via an API Gateway
#'
#' @inheritParams classify_event
#'
#' @return logical
#' @keywords internal
is_from_html_api_gateway <- function(event_content) {
  grepl("routeKey", event_content)
}

#' @export
parse_event_content.html_api_gateway_event <- function(event, ...) {
}

#' @export
serialise_result.html_api_gateway_event <- function(event, ...) {
  serialise_result.rest_api_gateway_event(event, ...)
}

#' @export
handle_event_error.html_api_gateway_event <- function(event, ...) {
  handle_event_error.rest_api_gateway_event(event, ...)
}
