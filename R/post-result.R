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
  UseMethod("serialise_result")
}

#' @export
serialise_result.default <- function(event, ...) {
  as_stringified_json(event$result)
}

#' Post an event with a result to the response endpoint
#'
#' This function will first serialise the event result according to its class
#' by dispatching through \code{\link{serialise_result}}.The result of that
#' serialisation is posted **as is** to the response endpoint; this function
#' will not perform any JSON serialisation, for example.
#'
#' @inheritSection is_from_rest_api_gateway Invocations via an API Gateway
#'
#' @inheritParams handle_event
#' @inheritParams validate_lambda_config
#' @inheritParams serialise_result
#'
#' @keywords internal
post_result <- function(event, config) {
  logger::log_debug("Raw result:", event$result)
  serialised_result <- serialise_result(event, config)
  logger::log_debug("Result to be posted:", serialised_result)

  httr::POST(
    url = get_response_endpoint(config = config, request_id = event$request_id),
    body = serialised_result,
    encode = "raw"
  )
}
