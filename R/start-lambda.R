#' Set-up logging and a Lambda runtime, and start listening for events
#'
#' @description
#' Runs the following three functions in order:
#' * \code{\link{setup_logging}}
#' * \code{\link{setup_lambda}}
#' * \code{\link{start_listening}}
#'
#' See \code{vignette("lambda-runtime-in-container", package = "lambdr")} for an
#' example of how to use this function to place an R Lambda Runtime in a
#' container.
#'
#' @inheritParams setup_logging
#' @inheritParams setup_lambda
#' @inheritParams start_listening
#'
#' @inheritSection lambda_variables AWS Lambda variables
#' @inheritSection extract_context Event context
#'
#' @export
#'
#' @examples \dontrun{
#' # A general usage pattern involves sourcing necessary functions and running
#' # this `start_lambda` in a `runtime.R` file which is then executed to start
#' # the runtime. In the following example, the function handler can be set to
#' # "lambda" either as the container `CMD`, or configured through AWS Lambda.
#'
#' parity <- function(number) {
#'   list(parity = if (as.integer(number) %% 2 == 0) "even" else "odd")
#' }
#'
#' start_lambda()
#'
#' }
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
