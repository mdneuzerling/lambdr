#' AWS Lambda endpoints
#'
#' @param config A list of configuration values as created by the
#'   \code{\link{lambda_config}} function. Alternatively, a runtime API host can
#'   be provided directly. This would be the case if there's not enough
#'   information available to build a full configuration object due to an error.
#' @param request_id For `get_response_endpoint` and
#'   `get_invocation_error_endpoint`, the ID of the particular event/request.
#'   This is provided in the "lambda-runtime-aws-request-id" header of the
#'   event.
#'
#' @description
#' These endpoints are configured based on the "AWS_LAMBDA_RUNTIME_API"
#' environment variable set by AWS Lambda during initialisation. They generally
#' won't be available locally. The "AWS_LAMBDA_RUNTIME_API" environment variable
#' (accessed through \code{\link{lambda_config}}) is used in the following
#' functions:
#'
#' * `get_next_invocation_endpoint` returns the endpoint which R must query for
#' the next input. R must send a `GET` request to this endpoint and will wait
#' until either a response is received or the Lambda instance is shut down for
#' inactivity. When Lambda receives an input from, say, an API Gateway, it will
#' respond to the pending request with details of the input.
#' * `get_initialisation_error_endpoint` returns the endpoint to which an error
#' should be sent if the error occurs when setting up the runtime. This is
#' distinct from errors that occur during handling of an event.
#' * `get_response_endpoint` returns the endpoint to which an event response
#' should be sent. It is unique for each event.
#' * `get_invocation_error_endpoint` returns the endpoint to which errors that
#' occur during event handling should be sent. It is unique for each event.
#'
#' The values returned by `get_next_invocation_endpoint` and
#' `get_initialisation_error_endpoint` are unique in each Lambda instance. That
#' is, the runtime only needs to retrieve their values once. The values returned
#' by `get_response_endpoint` and `get_invocation_error_endpoint` are determined
#' by the `request_id` argument that these functions require, and so need to be
#' recalculated for each event. The request ID is given in the
#' "lambda-runtime-aws-request-id" header in the event.
#'
#' @return character
#'
#' @name endpoints
#' @keywords internal
NULL

#' @rdname endpoints
#' @keywords internal
get_next_invocation_endpoint <- function(config, runtime_api) {
  lambda_runtime_api <- config_or_runtime_api(config)
  next_invocation_endpoint <- paste0(
    "http://", lambda_runtime_api, "/2018-06-01/runtime/invocation/next"
  )
  next_invocation_endpoint
}

#' @rdname endpoints
#' @keywords internal
get_initialisation_error_endpoint <- function(config, runtime_api) {
  lambda_runtime_api <- config_or_runtime_api(config)
  initialisation_error_endpoint <- paste0(
    "http://", lambda_runtime_api, "/2018-06-01/runtime/init/error"
  )
  initialisation_error_endpoint
}

#' @rdname endpoints
#' @keywords internal
get_response_endpoint <- function(config, request_id) {
  lambda_runtime_api <- config_or_runtime_api(config)
  paste0(
    "http://", lambda_runtime_api, "/2018-06-01/runtime/invocation/",
    request_id, "/response"
  )
}

#' @rdname endpoints
#' @keywords internal
get_invocation_error_endpoint <- function(config, request_id) {
  lambda_runtime_api <- config_or_runtime_api(config)
  paste0(
    "http://", lambda_runtime_api, "/2018-06-01/runtime/invocation/",
    request_id, "/error"
  )
}

#' Convert a config to a runtime API if necessary
#'
#' Endpoint functions need to accept either a config (as created by
#' \code{\link{lambda_config}}) or a runtime API. This function will accept
#' either and ensure that the runtime API is returned.
#'
#' @inheritParams validate_lambda_config
#'
#' @keywords internal
config_or_runtime_api <- function(config) {
  if ("lambda_config" %in% class(config)) config$runtime_api else config
}
