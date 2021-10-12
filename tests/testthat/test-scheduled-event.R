test_that("scheduled events are detected", {
  expect_false(
    is_scheduled_event_content(
      as_stringified_json(
        list(x = 3)
      )
    )
  )

  expect_true(
    is_scheduled_event_content(
      as_stringified_json(
        "Scheduled Event"
      )
    )
  )
})

test_that("scheduled events content is ignored", {
  event <- structure(
    as_stringified_json(list(x = 5)),
    class = c("scheduled_event", "event")
  )
  expect_equal(
    parse_event_content.scheduled_event(event),
    list()
  )
})
