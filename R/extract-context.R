#' Extract context from environment variables
#'
#' This function is intended to provide ambient configuration that makes up part
#' of the context returned by \code{\link{extract_context}}. These are the
#' components of the context formed by environment variables. To speed things up
#' a little we call the environment variables only once on runtime
#' initialisation and store them in the config.
#'
#' @return list
#' @keywords internal
extract_context_from_environment <- function() {
  list(
    function_name = Sys.getenv("AWS_LAMBDA_FUNCTION_NAME"),
    function_version = Sys.getenv("AWS_LAMBDA_FUNCTION_VERSION"),
    memory_limit_in_mb = Sys.getenv("AWS_LAMBDA_FUNCTION_MEMORY_SIZE"),
    log_group_name = Sys.getenv("AWS_LAMBDA_LOG_GROUP_NAME"),
    log_stream_name = Sys.getenv("AWS_LAMBDA_LOG_STREAM_NAME")
  )
}

#' Combine class-specific context with general context for an event
#'
#' The \code{\link{extract_context}} function dispatches on the class of an
#' event to extract context specific to that class. By default, it returns an
#' empty list. This function takes that class-specific context and combined it
#' with the context that is applicable for all classes, contained in both
#' event headers and environment variables.
#'
#' @inheritParams handle_event
#' @inheritParams validate_lambda_config
#' @param ...additional arguments passed to \code{\link{extract_context}}
#'
#' @inheritSection extract_context Event context
#'
#' @return list
#' @keywords internal
extract_and_augment_context <- function(event, config, ...) {
  standard_context <- c(
    list(
      aws_request_id = event$event_headers[["lambda-runtime-aws-request-id"]],
      invoked_function_arn = event$event_headers[["lambda-runtime-invoked-function-arn"]]
    ),
    config$environment_context
  )

  c(standard_context, extract_context(event, config, ...))
}

#' Extract the context of a Lambda invocation from the headers of an event
#'
#' @section Event context:
#' Context is metadata associated with each invocation. If the handler function
#' accepts a `context` argument then it will automatically receive at runtime a
#' named list consisting of these values along with the arguments in the body
#' (if any). For example, a function such as `my_func(x, context)` will receive
#' the context argument automatically. The `context` argument must be named
#' (`...` will not work).
#'
#' Refer to \code{vignette("lambda-runtime-in-container", package = "lambdr")}
#' for details.
#'
#' @inheritParams handle_event
#' @inheritParams validate_lambda_config
#'
#' @return list
#' @keywords internal
#' @export
extract_context <- function(event, config, ...) {
  UseMethod("extract_context")
}

#' @export
extract_context.default <- function(event, config, ...) {
  list()
}
