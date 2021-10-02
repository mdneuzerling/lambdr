test_that("Mock square root function", {
  use_basic_lambda_setup(handler = "sqrt")
  mock_response_success <- mock_response(
    input = jsonlite::toJSON(list(x = 4)),
    expected_response_body = 2
  )
  expect_true(mock_response_success)
})

test_that("Mock custom function", {
  named_sqrt <- function(x) {
    list(sqrt = sqrt(x))
  }
  use_basic_lambda_setup(handler = "named_sqrt")

  mock_response_success <- mock_response(
    input = jsonlite::toJSON(list(x = 4)),
    expected_response_body = jsonlite::toJSON(list(sqrt = 2), auto_unbox = TRUE)
  )
  expect_true(mock_response_success)
})
