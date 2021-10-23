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

test_that("EventBridge without detail (scheduled events) can be parsed", {
  raw_event_content <- '
    {
      "version": "0",
      "account": "1234567890",
      "region": "ap-southeast-2",
      "detail": {},
      "detail-type": "Scheduled Event",
      "source": "aws.events",
      "time": "2021-10-24T08:47:23Z",
      "id": "2007b97d-449b-41b7-9257-7fbbd5974ff6",
      "resources": [
        "arn:aws:events:ap-southeast-2:1234567890:rule/scheduled"
      ]
    }'

  event <- structure(
    list(event_content = raw_event_content),
    class = c("eventbridge_event", "event")
  )

  expect_true(is_eventbridge_event_content(event))

  parsed_content <- parse_event_content.eventbridge_event(event)
  expect_true(is.list(parsed_content))

  named_empty_list <- setNames(list(), character())
  expect_equal(parsed_content, named_empty_list)
})

test_that("EventBridge with detail can be parsed", {
  raw_event_content <- '
    {
      "version": "0",
      "account": "1234567890",
      "region": "ap-southeast-2",
      "detail": {
        "EventCategories": [
            "backup"
        ],
        "SourceType": "DB_INSTANCE",
        "SourceArn": "arn:aws:rds:ap-southeast-2:1234567890:db:abcdefghijklmn",
        "Date": "2021-10-24T08:56:37.156Z",
        "Message": "Finished DB Instance backup",
        "SourceIdentifier": "abcdefghijklmn"
      },
      "detail-type": "Scheduled Event",
      "source": "aws.events",
      "time": "2021-10-24T08:47:23Z",
      "id": "2007b97d-449b-41b7-9257-7fbbd5974ff6",
      "resources": [
        "arn:aws:events:ap-southeast-2:1234567890:rule/scheduled"
      ]
    }'

  event <- structure(
    list(event_content = raw_event_content),
    class = c("eventbridge_event", "event")
  )

  expect_true(is_eventbridge_event_content(event))

  parsed_content <- parse_event_content.eventbridge_event(event)
  expect_true(is.list(parsed_content))

  expect_equal(
    names(parsed_content),
    c("EventCategories", "SourceType", "SourceArn", "Date", "Message",
      "SourceIdentifier")
  )
  # test a couple of fields
  expect_equal(parsed_content$EventCategories, "backup")
  expect_equal(parsed_content$SourceType, "DB_INSTANCE")
  expect_equal(parsed_content$Message, "Finished DB Instance backup")
  expect_equal(parsed_content$SourceIdentifier, "abcdefghijklmn")
})

