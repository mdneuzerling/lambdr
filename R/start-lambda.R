#' Set-up logging and a Lambda runtime, and start listening for events
#'
#' @description
#' Runs the following three functions in order:
#' * \code{\link{setup_logging}}
#' * \code{\link{setup_lambda}}
#' * \code{\link{start_listening}}
#'
#' @inheritParams setup_logging
#' @inheritParams setup_lambda
#' @inheritParams start_listening
#'
#' @inheritSection lambda_variables AWS Lambda variables
#' @inheritSection extract_context Event context
#'
#' @export
start_lambda <- function(
  log_threshold = logger::INFO,
  handler = NULL,
  runtime_api = NULL,
  task_root = NULL,
  environ = parent.frame(),
  deserialiser = NULL,
  serialiser = NULL,
  timeout_seconds = NULL
) {

  setup_logging(log_threshold = log_threshold)

  setup_lambda(
    handler = handler,
    runtime_api = runtime_api,
    task_root = task_root,
    environ = environ
  )

  start_listening(
    deserialiser = deserialiser,
    serialiser = serialiser,
    timeout_seconds = timeout_seconds
  )
}
