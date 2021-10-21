test_that("EventBridge events are detected", {
  expect_false(
    is_eventbridge_event_content(
      as_stringified_json(
        list(x = 3)
      )
    )
  )

  expect_true(
    is_eventbridge_event_content(
      as_stringified_json(
        "source"
      )
    )
  )
})

test_that("EventBridge event content is ignored", {
  event <- structure(
    as_stringified_json(list(x = 5)),
    class = c("scheduled_event", "event")
  )
  expect_equal(
    parse_event_content.eventbridge_event(event),
    list()
  )
})
