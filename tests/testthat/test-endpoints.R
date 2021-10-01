test_that("we can retrieve the AWS Lambda runtime environment variables", {
  expect_error(
    get_lambda_runtime_api(),
    "environment variable is not set"
  )

  withr::with_envvar(
    c("AWS_LAMBDA_RUNTIME_API" = "red_panda"),
    expect_equal(get_lambda_runtime_api(), "red_panda")
  )
})

test_that("we can retrieve the next invocation endpoint", {
  expect_error(
    get_next_invocation_endpoint(),
    "environment variable is not set"
  )

  withr::with_envvar(
    c("AWS_LAMBDA_RUNTIME_API" = "red_panda"),
    expect_equal(
      get_next_invocation_endpoint(),
      "http://red_panda/2018-06-01/runtime/invocation/next"
    )
  )
})

test_that("we can retrieve the initialisation error endpoint", {
  expect_error(
    get_next_invocation_endpoint(),
    "environment variable is not set"
  )

  withr::with_envvar(
    c("AWS_LAMBDA_RUNTIME_API" = "red_panda"),
    expect_equal(
      get_initialisation_error_endpoint(),
      "http://red_panda/2018-06-01/runtime/init/error"
    )
  )
})

test_that("we can retrieve the initialisation error endpoint", {
  expect_error(
    get_next_invocation_endpoint(),
    "environment variable is not set"
  )

  withr::with_envvar(
    c("AWS_LAMBDA_RUNTIME_API" = "red_panda"),
    expect_equal(
      get_initialisation_error_endpoint(),
      "http://red_panda/2018-06-01/runtime/init/error"
    )
  )
})

test_that("we can retrieve the response endpoint", {
  expect_error(
    get_response_endpoint(request_id = "corgi"),
    "environment variable is not set"
  )

  withr::with_envvar(
    c("AWS_LAMBDA_RUNTIME_API" = "red_panda"),
    expect_equal(
      get_response_endpoint(request_id = "corgi"),
      "http://red_panda/2018-06-01/runtime/invocation/corgi/response"
    )
  )
})

test_that("we can retrieve the invocation endpoint", {
  expect_error(
    get_response_endpoint(request_id = "corgi"),
    "environment variable is not set"
  )

  withr::with_envvar(
    c("AWS_LAMBDA_RUNTIME_API" = "red_panda"),
    expect_equal(
      get_invocation_error_endpoint(request_id = "corgi"),
      "http://red_panda/2018-06-01/runtime/invocation/corgi/error"
    )
  )
})
