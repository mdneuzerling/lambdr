test_that("we can use either a runtime API or a config", {
  config <- basic_lambda_config(runtime_api = "red_panda")

  expect_equal(config_or_runtime_api(config), "red_panda")
  expect_equal(config_or_runtime_api(config$runtime_api), "red_panda")
})

test_that("we can retrieve the next invocation endpoint", {
  config <- basic_lambda_config(runtime_api = "red_panda")

  expect_equal(
    get_next_invocation_endpoint(config),
    "http://red_panda/2018-06-01/runtime/invocation/next"
  )
  expect_equal(
    get_next_invocation_endpoint(config$runtime_api),
    "http://red_panda/2018-06-01/runtime/invocation/next"
  )
})

test_that("we can retrieve the initialisation error endpoint", {
  config <- basic_lambda_config(runtime_api = "red_panda")
  expect_equal(
    get_initialisation_error_endpoint(config),
    "http://red_panda/2018-06-01/runtime/init/error"
  )
  expect_equal(
    get_initialisation_error_endpoint(config$runtime_api),
    "http://red_panda/2018-06-01/runtime/init/error"
  )
})

test_that("we can retrieve the response endpoint", {
  config <- basic_lambda_config(runtime_api = "red_panda")
  expect_equal(
    get_response_endpoint(config, request_id = "corgi"),
    "http://red_panda/2018-06-01/runtime/invocation/corgi/response"
  )
  expect_equal(
    get_response_endpoint(config$runtime_api, request_id = "corgi"),
    "http://red_panda/2018-06-01/runtime/invocation/corgi/response"
  )
})

test_that("we can retrieve the invocation error endpoint", {
  config <- basic_lambda_config(runtime_api = "red_panda")
  expect_equal(
    get_invocation_error_endpoint(config, request_id = "corgi"),
    "http://red_panda/2018-06-01/runtime/invocation/corgi/error"
  )
  expect_equal(
    get_invocation_error_endpoint(config$runtime_api, request_id = "corgi"),
    "http://red_panda/2018-06-01/runtime/invocation/corgi/error"
  )
})
