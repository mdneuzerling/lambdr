expect_setup_failure <- function(endpoint_function, ...) {
  eval(bquote({
    expect_error(
      endpoint_function(...),
      "The AWS Lambda runtime is not configured"
    )
  }))
}

use_basic_lambda_setup <- function(handler = "sqrt") {
  withr::with_envvar(
    c("AWS_LAMBDA_RUNTIME_API" = "red_panda",
      "LAMBDA_TASK_ROOT" = "giraffe",
      "_HANDLER" = handler),
    setup_lambda()
  )
  withr::defer(reset_lambda(), envir = parent.frame())
}
