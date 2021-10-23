#' Determine if a Lambda event is coming from a EventBridge event (CloudWatch events)
#'
#' See \url{https://docs.aws.amazon.com/lambda/latest/dg/services-cloudwatchevents.html}
#' for more information.
#'
#' @inheritParams classify_event
#'
#' @return logical
#' @keywords internal
is_eventbridge_event_content <- function(event_content) {
  grepl('"source"', event_content)
}

#' @export
parse_event_content.eventbridge_event <- function(event, ...) {
  detail <- jsonlite::fromJSON(event$event_content)[["detail"]]
  if (is.null(detail)) {
    list()
  } else {
    detail
  }
}
