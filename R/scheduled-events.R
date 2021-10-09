#' Determine if a Lambda event is coming from a scheduled Cloudwatch event
#'
#' @param event_content
#'
#' @return logical
#' @keywords internal
is_scheduled_event_content <- function(event_content) {
  grepl("Scheduled Event", event_content)
}
