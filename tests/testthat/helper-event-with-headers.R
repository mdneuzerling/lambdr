# This is a basic event with headers, but is otherwise empty
# This is used, for example, when testing context extraction


event_with_headers <- function(
  request_id = "abc123",
  request_arn = "arn:aws:lambda:us-west-2:123456789012:function:my-function"
) {
  structure(
    list(event_headers = list(
      "lambda-runtime-aws-request-id" = request_id,
      "lambda-runtime-invoked-function-arn" = request_arn
    )),
    class = "event"
  )
}
