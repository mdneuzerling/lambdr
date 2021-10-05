#' Assert that the Lambda runtime has been set up
#'
#' This package maintains a package-internal environment that, amongst other
#' uses, records if Lambda runtime has been set up via the
#' \verb{\link{setup_lambda}} or \verb{\link{start_lambda}} functions. This
#' function asserts that this setup has been completed and recorded, and
#' errors otherwise.
#'
#' @return TRUE
#' @keywords internal
assert_lambda_is_setup <- function() {
  if (is.null(lambda$is_setup) || !lambda$is_setup) {
    stop("The AWS Lambda runtime is not configured. Run `setup_lambda()` once ",
         "at the beginning of your runtime script.")
  }
  TRUE
}

#' Retrieve AWS Lambda configuration values
#'
#' @description
#' These functions can only be called after the `setup_lambda` function has
#' been run. They return the various configuration values provided by AWS
#' Lambda. These values are described in the section below.
#'
#' The handler, as a character, is returned by `get_handler_character`. The
#' function defined by the handler is returned by `get_handler`.
#'
#' @section AWS Lambda variables:
#'
#' The following variables are made available through environment variables
#' configured by AWS:
#'
#' * Lambda Runtime API, available as the "AWS_LAMBDA_RUNTIME_API" environment
#'   variable, is the host of the various HTTP endpoints through which the
#'   runtime interacts with Lambda. See also: \verb{\link{endpoints}}.
#' * Lambda Task Root, available as the "LAMBDA_TASK_ROOT" environment variable,
#'   defines the path to the Lambda function code.
#' * The handler, available as the "_HANDLER" environment variable, is
#'   interpreted by R as the function that is executed when the Lambda is
#'   called. This value could be anything, as the interpretation is solely up
#'   to the runtime, so requiring it to be a function is a standard imposed by
#'   this package.
#'
#' @return character
#' @keywords internal
#' @name lambda_variables
NULL

#' @rdname lambda_variables
#' @keywords internal
get_lambda_runtime_api <- function() {
  assert_lambda_is_setup()
  lambda$runtime_api
}

#' @rdname lambda_variables
#' @keywords internal
get_lambda_task_root <- function() {
  assert_lambda_is_setup()
  lambda$task_root
}

#' @rdname lambda_variables
#' @keywords internal
get_handler <- function() {
  assert_lambda_is_setup()
  lambda$handler
}

#' @rdname lambda_variables
#' @keywords internal
get_handler_character <- function() {
  assert_lambda_is_setup()
  lambda$handler_character
}

#' Retrieve a Lambda environment variable if available, and error otherwise
#'
#' @description
#' This function is provided to return a specific error if an environment
#' variable is not defined. This is used by `setup_lambda` to ensure that the
#' environment variables that are expected to be defined by AWS are present.
#'
#' If the environment variable is undefined but a `default` value is provided,
#' then that default value will be returned. However, the environment variable
#' will always take precedence.
#'
#' @param env_var character environment variable to retrieve
#' @param default character default value to return if the environment variable
#'   is undefined. The environment variable always takes precedence.
#'
#' @return character
#'
#' @keywords internal
get_lambda_environment_variable <- function(env_var, default = NULL) {
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
#' @param environ environment in which to search for the function given by the
#'   handler.
#'
#' @details
#' As a rule of thumb, it takes longer to retrieve a value from an environment
#' variable than it does to retrieve a value from R. This is because retrieving
#' an environment variable requires a system call. Since the environment
#' variables do not change in a Lambda instance, we fetch them once and set them
#' to a package environment.
#'
#' @inheritSection lambda_variables AWS Lambda variables
#'
#' @export
setup_lambda <- function(
  handler = NULL,
  runtime_api = NULL,
  task_root = NULL,
  environ = parent.frame()
  ) {

  handler_character <- get_lambda_environment_variable(
    "_HANDLER",
    handler
  )

  if (!exists(handler_character, envir = environ)) {
    stop(handler_character, " not found")
  }

  handler <- get(handler_character, envir = environ)
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

#' Undo the Lambda runtime setup
#'
#' This function removes all detected variables and setup initiated by either
#' \verb{\link{setup_lambda}} or \verb{\link{start_lambda}}. It does not affect
#' the logging handlers setup by these functions.
#'
#' @keywords internal
reset_lambda <- function() {
  # delete all objects in Lambda environment
  rm(list = ls(envir = lambda), envir = lambda)
  lambda$is_setup <- FALSE
}
