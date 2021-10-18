test_that("custom deserialisers are used", {
  custom_deserialiser <- function(event_content) {
    "my heart is a fish"
  }
  config <- structure(
    list(deserialiser = custom_deserialiser),
    class = "lambda_config"
  )

  event <- structure(
    as_stringified_json(list(x = 1)),
    class = "event"
  )

  expect_equal(
    parse_event_content(event, config),
    "my heart is a fish"
  )
})
