test_that("SNS events are detected", {
  expect_false(
    is_sns_event_content(
      as_stringified_json(
        list(x = 3)
      )
    )
  )

  expect_true(
    is_sns_event_content(
      as_stringified_json(
        list(EventSource = "aws:sns"),
        pretty = TRUE
      )
    )
  )
})
