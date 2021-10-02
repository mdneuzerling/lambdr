test_that("Mock square root function", {
  use_basic_lambda_setup(handler = "sqrt")
  mock_response_success <- mock_response(
    input = jsonlite::toJSON(list(x = 4)),
    expected_response_body = 2
  )
  expect_true(mock_response_success)
})
