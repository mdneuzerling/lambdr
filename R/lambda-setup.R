setup_logging <- function(log_threshold = logger::INFO) {
  logger::log_formatter(logger::formatter_paste)
  logger::log_threshold(log_threshold)
}

#' Retrieve the Lambda Runtime API value
#'
#' This value is set once per Lambda instance. It is used to construct the
#' various endpoints required to connect with the Lambda service.
#'
#' @return character
#' @keywords internal
get_lambda_runtime_api <- function() {
  assert_lambda_is_setup()
  lambda$runtime_api
}

get_lambda_task_root <- function() {
  assert_lambda_is_setup()
  lambda$task_root
}

get_handler <- function() {
  assert_lambda_is_setup()
  lambda$handler
}

get_handler_character <- function() {
  assert_lambda_is_setup()
  lambda$handler_character
}

assert_lambda_is_setup <- function() {
  if (is.null(lambda$is_setup) || !lambda$is_setup) {
    stop("The AWS Lambda runtime is not configured. Run `setup_lambda()` once ",
         "at the beginning of your runtime script.")
  }
  TRUE
}

reset_lambda <- function() {
  # delete all objects in Lambda environment
  rm(list = ls(envir = lambda), envir = lambda)
  lambda$is_setup <- FALSE
}

get_lambda_environment_variable <- function(env_var, default = NULL) {
  setup_logging()

  value <- Sys.getenv(env_var)
  if (value == "" && is.null(default)) {
    stop(env_var, " environment variable is not set. This ",
         "environment variable is set by AWS when Lambda is instantiated. It ",
         "will not appear when running local tests.")
  }

  if (value != "") {
    value
  } else {
    default
  }
}

#' Set up endpoints, variables and logging for AWS Lambda
#'
#' @param log_threshold Threshold for recording and displaying log entries.
#'   Passed to logger::log_threshold. Defaults to `logger::INFO`. To debug a
#'   failing Lambda container, set this to `logger::DEBUG` to log verbose
#'   information about how each request is processed.
#' @param handler character. Name of function to use for processing inputs from
#'   events. This argument is provided for debugging and testing only. The
#'   "_HANDLER" environment variable, as configured in AWS, as configured by
#'   AWS, will always override this value if present.
#' @param runtime_api character. Used as the host in the various endpoints used
#'   by AWS Lambda. This argument is provided for debugging and testing only.
#'   The "AWS_LAMBDA_RUNTIME_API" environment variable, as configured by AWS,
#'   will always override this value if present.
#' @param task_root character. Defines the path to the Lambda function code.
#'   This argument is provided for debugging and testing only. The
#'   "LAMBDA_TASK_ROOT" environment variable, as configured by AWS, will always
#'   override this value if present.
#'
#' @details
#' As a rule of thumb, it takes longer to retrieve a value from an environment
#' variable than it does to retrieve a value from R. This is because retrieving
#' an environment variable requires a system call. Since the environment
#' variables do not change in a Lambda instance, we fetch them once and set them
#' to a package environment.
#'
#' @export
setup_lambda <- function(
  log_threshold = logger::INFO,
  handler = NULL,
  runtime_api = NULL,
  task_root = NULL
  ) {

  setup_logging(log_threshold = log_threshold)

  handler_character <- get_lambda_environment_variable(
    "_HANDLER",
    handler
  )

  if (!exists(handler_character, envir = parent.frame())) {
    stop(handler_character, " not found")
  }

  handler <- get(handler_character, envir = parent.frame())
  if (!is.function(handler)) {
    stop(handler_character, " is not a function")
  }
  lambda$handler_character <- handler_character
  lambda$handler <- handler

  lambda$runtime_api <- get_lambda_environment_variable(
    "AWS_LAMBDA_RUNTIME_API",
    runtime_api
  )

  lambda$task_root <- get_lambda_environment_variable(
    "LAMBDA_TASK_ROOT",
    task_root
  )

  lambda$is_setup <- TRUE
}
