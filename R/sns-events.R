#' Determine if a Lambda event is coming from SNS
#'
#' See \url{https://docs.aws.amazon.com/lambda/latest/dg/with-sns.html}
#' for more information.
#'
#' @param event_content
#'
#' @return logical
#' @keywords internal
is_sns_event_content <- function(event_content) {
  grepl('"EventSource": "aws:sns"', event_content)
}

#' @export
parse_event_content.sns_event <- function(event, ...) {
  sns <- jsonlite::fromJSON(event$event_content)[["Records"]][[1]][["Sns"]]
  list(message = jsonlite::fromJSON(sns[["Message"]]))
}
