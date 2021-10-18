#' Set up logging with the `logger` package
#'
#' @param log_threshold Threshold for recording and displaying log entries.
#'   Defaults to `logger::INFO`. To debug a failing Lambda container, set this
#'   to `logger::DEBUG` to log verbose information about how each request is
#'   processed.
#'
#' @export
setup_logging <- function(log_threshold = logger::INFO) {
  logger::log_formatter(logger::formatter_paste)
  logger::log_threshold(log_threshold)
}
