expect_setup_failure <- function(endpoint_function, ...) {
  eval(bquote({
    expect_error(
      endpoint_function(...),
      "The AWS Lambda runtime is not configured"
    )
  }))
}
