test_that("Lambda can be set up and reset", {
  expect_false(lambda$is_setup)
  expect_error(assert_lambda_is_setup(), "is not configured")

  withr::with_envvar(c("AWS_LAMBDA_RUNTIME_API" = "red_panda"), setup_lambda())
  withr::defer(reset_lambda())
  expect_true(lambda$is_setup)

  reset_lambda()
  expect_false(lambda$is_setup)
  expect_error(assert_lambda_is_setup(), "is not configured")
})

test_that("Lambda runtime API retrieval fails before setup", {
  expect_setup_failure(get_lambda_runtime_api)
})

test_that("we can retrieve the Lambda runtime API variable", {
  withr::with_envvar(c("AWS_LAMBDA_RUNTIME_API" = "red_panda"), setup_lambda())
  withr::defer(reset_lambda())
  expect_equal(
    get_lambda_runtime_api(),
    "red_panda"
  )
})
