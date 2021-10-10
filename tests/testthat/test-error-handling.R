test_that("errors can have HTML codes", {
  err500 <- tryCatch(
    stop_html("my heart is a fish"),
    error = function(e) e
  )
  expect_identical(err500$code, 500L)

  err404 <- tryCatch(
    stop_html("my heart is a fish", code = 404L),
    error = function(e) e
  )
  expect_identical(err404$code, 404L)
})

test_that("handle_decomposition_error produces error function", {
  use_basic_lambda_setup(runtime_api = "red_panda")
  request_id <- "abc123"

  error_handling_function <- handle_decomposition_error(request_id)

  expect_equal(
    names(formals(error_handling_function)),
    "e"
  )
})

test_that("handle_decomposition_error produces error function", {
  use_basic_lambda_setup(runtime_api = "red_panda")
  request_id <- "abc123"
  event <- structure(list(request_id = request_id), class = "event")

  error_handling_function <- handle_event_error(event)

  expect_equal(
    names(formals(error_handling_function)),
    "e"
  )
})
