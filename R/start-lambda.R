#' Start the Lambda runtime
#'
#' This is the main function of the package, responsible for starting the
#' infinite loop of listening for new invocations. It relies on configuration
#' provided to the `config` argument and produced by the
#' \code{\link{lambda_config}} function.
#'
#' See \code{vignette("lambda-runtime-in-container", package = "lambdr")} for an
#' example of how to use this function to place an R Lambda Runtime in a
#' container.
#'
#' This package uses the \code{\link[logger]{logger}} package for logging.
#' Debug log entries can be enabled with `logger::log_threshold(logger::DEBUG)`.
#' This will log additional information such as raw event bodies.
#'
#' @inheritParams validate_lambda_config
#' @inheritParams start_listening
#'
#' @inheritSection extract_context Event context
#' @inheritSection lambda_config AWS Lambda variables
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
#' # Alternatively, it can be passed as an argument `handler = parity` to
#' # the lambda configuration. If the handler is configured through other means
#' # then this will be ignored:
#'
#' start_lambda(config = lambda_config(handler = parity))
#' }
start_lambda <- function(config = lambda_config(environ = parent.frame()),
                         timeout_seconds = NULL) {
  start_listening(
    config = config,
    timeout_seconds = timeout_seconds
  )
}
