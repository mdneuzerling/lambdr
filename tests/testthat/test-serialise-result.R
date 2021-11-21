test_that("events without results return errors when serialising", {
  no_result <- structure(
    list(request_id = "abc123"),
    class = "event",
    result_calculated = FALSE
  )
  expect_error(
    serialise_result(no_result),
    "The result for event abc123 has not been calculated"
  )
})

test_that("custom serialisers are used", {
  event <- structure(
    list(
      result = list(x = 1),
      request_id = "abc123"
    ),
    class = "event",
    result_calculated = TRUE
  )

  custom_serialiser <- function(result) "my heart is a fish"
  config <- structure(
    list(serialiser = custom_serialiser),
    class = "lambda_config"
  )

  expect_equal(
    serialise_result(event, config = config),
    "my heart is a fish"
  )
})

test_that("already_serialised results are not serialised twice", {
  already_serialised <- structure(
    list(result = html_response("abc")),
    "result_calculated" = TRUE,
    class = "event"
  )
  test_config <- structure(list(serialiser = NULL), class = "lambda_config")

  # serialise_result should return the event$result unaltered
  expect_equal(
    already_serialised$result,
    serialise_result(already_serialised, test_config)
  )
})
