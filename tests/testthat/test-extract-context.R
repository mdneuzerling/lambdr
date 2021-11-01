test_that("by default, extract_context extracts nothing", {
  event <- event_with_headers
  expect_equal(extract_context(event, config = list()), list())
  # should be the same as
  expect_equal(extract_context.default(event, config = list()), list())
})

test_that("context can be extracted from headers", {
  request_id <- "abc123"
  request_arn <-  "arn:aws:lambda:us-west-2:123456789012:function:my-function"

  event <- event_with_headers(
    request_id = request_id,
    request_arn = request_arn
  )

  from_headers <- extract_and_augment_context(event, config = list())

  expect_equal(from_headers$aws_request_id, request_id)
  expect_equal(from_headers$invoked_function_arn, request_arn)
})

test_that("context can be extracted from headers", {
  request_id <- "abc123"
  request_arn <-  "arn:aws:lambda:us-west-2:123456789012:function:my-function"

  event <- event_with_headers(
    request_id = request_id,
    request_arn = request_arn
  )

  from_headers <- extract_and_augment_context(event, config = list())

  expect_equal(from_headers$aws_request_id, request_id)
  expect_equal(from_headers$invoked_function_arn, request_arn)
})

test_that("context can be extracted from the environment", {
  from_env <- withr::with_envvar(
    new = c(
      AWS_LAMBDA_FUNCTION_NAME = "function_name",
      AWS_LAMBDA_FUNCTION_VERSION = "function_version",
      AWS_LAMBDA_FUNCTION_MEMORY_SIZE = "memory_limit_in_mb",
      AWS_LAMBDA_LOG_GROUP_NAME = "log_group_name",
      AWS_LAMBDA_LOG_STREAM_NAME = "log_stream_name"
    ),
    extract_context_from_environment()
  )

  expect_equal(from_env$function_name, "function_name")
  expect_equal(from_env$function_version, "function_version")
  expect_equal(from_env$memory_limit_in_mb, "memory_limit_in_mb")
  expect_equal(from_env$log_group_name, "log_group_name")
  expect_equal(from_env$log_stream_name, "log_stream_name")

  request_id <- "abc123"
  request_arn <-  "arn:aws:lambda:us-west-2:123456789012:function:my-function"

  event <- event_with_headers(
    request_id = request_id,
    request_arn = request_arn
  )
  config <- list(environment_context = from_env)
  full_context <- extract_and_augment_context(event, config)

  expect_equal(full_context$aws_request_id, request_id)
  expect_equal(full_context$invoked_function_arn, request_arn)
  expect_equal(full_context$function_name, "function_name")
  expect_equal(full_context$function_version, "function_version")
  expect_equal(full_context$memory_limit_in_mb, "memory_limit_in_mb")
  expect_equal(full_context$log_group_name, "log_group_name")
  expect_equal(full_context$log_stream_name, "log_stream_name")
})
