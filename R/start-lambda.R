#' Set-up logging and a Lambda runtime, and start listening for events
#'
#' @description
#' Runs the following three functions in order:
#' * \code{\link{setup_logging}}
#' * \code{\link{lambda_config}}
#' * \code{\link{start_listening}}
#'
#' See \code{vignette("lambda-runtime-in-container", package = "lambdr")} for an
#' example of how to use this function to place an R Lambda Runtime in a
#' container.
#'
#' @inheritParams validate_lambda_config
#' @inheritParams start_listening
#'
#' @inheritSection lambda_config AWS Lambda variables
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
#' }
start_lambda <- function(config = lambda_config(environ = parent.frame()),
                         timeout_seconds = NULL) {
  start_listening(
    config = config,
    timeout_seconds = timeout_seconds
  )
}
