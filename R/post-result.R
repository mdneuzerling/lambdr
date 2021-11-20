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
    body = as.character(serialised_result),
    encode = "raw"
  )
}
