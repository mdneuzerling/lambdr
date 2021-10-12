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

  expect_equal(
    serialise_result(event, serialiser = custom_serialiser),
    "my heart is a fish"
  )
})
