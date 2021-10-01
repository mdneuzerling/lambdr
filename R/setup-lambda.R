setup_lambda <- function(log_threshold = logger::INFO) {
  logger::log_formatter(logger::formatter_paste)
  logger::log_threshold(log_threshold)
}
