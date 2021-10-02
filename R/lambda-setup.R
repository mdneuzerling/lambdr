setup_logging <- function(log_threshold = logger::INFO) {
  logger::log_formatter(logger::formatter_paste)
  logger::log_threshold(log_threshold)
}

get_lambda_runtime_api <- function() {
  assert_lambda_is_setup()
  lambda$runtime_api
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

#' Set up endpoints and logging for AWS Lambda
#'
#' @details
#' As a rule of thumb, it takes longer to retrieve a value from an environment
#' variable than it does to retrieve a value from R. This is because retrieving
#' an environment variable requires a system call. Since the environment
#' variables do not change in a Lambda instance, we fetch them once and set them
#' to a package environment.
#'
#' @export
setup_lambda <- function() {
  env_var <- "AWS_LAMBDA_RUNTIME_API"
  lambda_runtime_api <- Sys.getenv(env_var)
  if (lambda_runtime_api == "") {
    stop(env_var, " environment variable is not set. This environment ",
         "variable is set by AWS when Lambda is instantiated. It will not ",
         "appear when running local tests.")
  }

  lambda$runtime_api <- lambda_runtime_api
  lambda$is_setup <- TRUE
}
