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

raw_sns_event_content <- '{
    "Records": [
        {
            "EventSource": "aws:sns",
            "EventVersion": "1.0",
            "EventSubscriptionArn": "arn:aws:sns:ap-southeast-2:1234567890:lambda-parity-test:123456abcdef",
            "Sns": {
                "Type": "Notification",
                "MessageId": "123456abcdef",
                "TopicArn": "arn:aws:sns:ap-southeast-2:1234567890:lambda-parity-test",
                "Subject": "INSUFFICIENT_DATA: \\"test-lambda-parity\\" in Asia Pacific (Sydney)",
                "Message": "{\\"AlarmName\\":\\"test-lambda-parity\\",\\"AlarmDescription\\":null,\\"AWSAccountId\\":\\"1234567890\\",\\"NewStateValue\\":\\"INSUFFICIENT_DATA\\",\\"NewStateReason\\":\\"Insufficient Data: 1 datapoint was unknown.\\",\\"StateChangeTime\\":\\"2021-10-21T10:20:50.932+0000\\",\\"Region\\":\\"Asia Pacific (Sydney)\\",\\"AlarmArn\\":\\"arn:aws:cloudwatch:ap-southeast-2:1234567890:alarm:test-lambda-parity\\",\\"OldStateValue\\":\\"ALARM\\",\\"Trigger\\":{\\"MetricName\\":\\"NumberOfObjects\\",\\"Namespace\\":\\"AWS/S3\\",\\"StatisticType\\":\\"Statistic\\",\\"Statistic\\":\\"MAXIMUM\\",\\"Unit\\":null,\\"Dimensions\\":[{\\"value\\":\\"random-data\\",\\"name\\":\\"BucketName\\"},{\\"value\\":\\"AllStorageTypes\\",\\"name\\":\\"StorageType\\"}],\\"Period\\":60,\\"EvaluationPeriods\\":1,\\"ComparisonOperator\\":\\"GreaterThanOrEqualToThreshold\\",\\"Threshold\\":0.0,\\"TreatMissingData\\":\\"- TreatMissingData:                    missing\\",\\"EvaluateLowSampleCountPercentile\\":\\"\\"}}",
                "Timestamp": "2021-10-21T10:20:50.973Z",
                "SignatureVersion": "1",
                "Signature": "abcdefghijklmnopqrstubwxyz",
                "SigningCertUrl": "https://sns.ap-southeast-2.amazonaws.com/SimpleNotificationService-123456abcdef.pem",
                "UnsubscribeUrl": "https://sns.ap-southeast-2.amazonaws.com/?Action=Unsubscribe&SubscriptionArn=arn:aws:sns:ap-southeast-2:1234567890:lambda-parity-test:123456abcdef",
                "MessageAttributes": {}
            }
        }
    ]
}'

sns_event <- structure(
  list(event_content = raw_sns_event_content),
  class = c("sns_event", "event")
)

test_that("JSON SNS messages can be parsed", {
  parsed_content <- parse_event_content.sns_event(sns_event)
  expect_equal(
    names(parsed_content),
    c(
      "AlarmName", "AlarmDescription", "AWSAccountId", "NewStateValue",
      "NewStateReason", "StateChangeTime", "Region", "AlarmArn",
      "OldStateValue", "Trigger"
    )
  )
  # check a couple of data points
  expect_equal(parsed_content$AlarmName, "test-lambda-parity")
  expect_equal(parsed_content$AWSAccountId, "1234567890")
  expect_equal(parsed_content$NewStateValue, "INSUFFICIENT_DATA")
  expect_equal(parsed_content$Region, "Asia Pacific (Sydney)")

})



