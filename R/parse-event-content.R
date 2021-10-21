#' Parse the body of the Lambda event
#'
#' @inheritSection is_from_rest_api_gateway Invocations via an API Gateway
#'
#' @inheritParams handle_event
#' @inheritParams validate_lambda_config
#'
#' @return A list containing the arguments to be passed to the handler function
#'
#' @keywords internal
#' @export
parse_event_content <- function(event, config) {
  if (!is.null(config$deserialiser)) {
    return(config$deserialiser(event$event_content))
  }
  UseMethod("parse_event_content")
}

#' @export
parse_event_content.default <- function(event, ...) {
  parse_json_or_empty(event$event_content)
}
