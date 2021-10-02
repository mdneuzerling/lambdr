test_that("endpoint retrievals fail before setup", {
  expect_setup_failure(get_next_invocation_endpoint)
  expect_setup_failure(get_initialisation_error_endpoint)
  expect_setup_failure(get_response_endpoint, "request_id")
  expect_setup_failure(get_invocation_error_endpoint, "request_id")
})

test_that("we can retrieve the Lambda runtime API variable", {
  withr::with_envvar(c("AWS_LAMBDA_RUNTIME_API" = "red_panda"), setup_lambda())
  withr::defer(reset_lambda())
  expect_equal(
    get_lambda_runtime_api(),
    "red_panda"
  )
})

test_that("we can retrieve the next invocation endpoint", {
  withr::with_envvar(c("AWS_LAMBDA_RUNTIME_API" = "red_panda"), setup_lambda())
  withr::defer(reset_lambda())
  expect_equal(
    get_next_invocation_endpoint(),
    "http://red_panda/2018-06-01/runtime/invocation/next"
  )
})

test_that("we can retrieve the initialisation error endpoint", {
  withr::with_envvar(c("AWS_LAMBDA_RUNTIME_API" = "red_panda"), setup_lambda())
  withr::defer(reset_lambda())
  expect_equal(
    get_initialisation_error_endpoint(),
    "http://red_panda/2018-06-01/runtime/init/error"
  )
})

test_that("we can retrieve the initialisation error endpoint", {
  withr::with_envvar(c("AWS_LAMBDA_RUNTIME_API" = "red_panda"), setup_lambda())
  withr::defer(reset_lambda())
  expect_equal(
    get_initialisation_error_endpoint(),
    "http://red_panda/2018-06-01/runtime/init/error"
  )
})

test_that("we can retrieve the response endpoint", {
  withr::with_envvar(c("AWS_LAMBDA_RUNTIME_API" = "red_panda"), setup_lambda())
  withr::defer(reset_lambda())
  expect_equal(
    get_response_endpoint(request_id = "corgi"),
    "http://red_panda/2018-06-01/runtime/invocation/corgi/response"
  )
})

test_that("we can retrieve the invocation endpoint", {
  withr::with_envvar(c("AWS_LAMBDA_RUNTIME_API" = "red_panda"), setup_lambda())
  withr::defer(reset_lambda())
  expect_equal(
    get_invocation_error_endpoint(request_id = "corgi"),
    "http://red_panda/2018-06-01/runtime/invocation/corgi/error"
  )
})
