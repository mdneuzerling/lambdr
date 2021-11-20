#' Serialise a result
#'
#' This result is posted **as is**, and so all JSON serialisation, etc. must be
#' performed here.
#'
#' @inheritSection is_from_rest_api_gateway Invocations via an API Gateway
#'
#' @inheritParams handle_event
#' @inheritParams validate_lambda_config
#'
#' @return character.
#'
#' @keywords internal
#' @export
serialise_result <- function(event, config) {
  if (!attr(event, "result_calculated")) {
    stop("The result for event ", event$request_id, " has not been calculated")
  }
  if (!is.null(config$serialiser)) {
    return(config$serialiser(event$result))
  }

  # See `html_result` for an example of this usage, where a response has already
  # been prepared and serialised and doesn't need any further treatment.
  is_already_serialised <- attr(event$result, "already_serialised")
  if (!is.null(is_already_serialised) && is_already_serialised) {
    return(event$result)
  }

  UseMethod("serialise_result")
}

#' @export
serialise_result.default <- function(event, ...) {
  as_stringified_json(event$result)
}
