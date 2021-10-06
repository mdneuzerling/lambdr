#' Convert a list to a single character, preserving names
#'
#' @param x Named list.
#'
#' @return character
#' @export
#'
#' @examples
#' prettify_list(list("a" = 1, "b" = 2, "c" = 3))
#' # "a=5, b=5, c=5"
#' @keywords internal
prettify_list <- function(x) {
  paste(
    paste(names(x), x, sep = "="),
    collapse = ", "
  )
}

#' Parse a JSON, but force a NULL or empty string to be interpreted as an empty list
#'
#' Since `jsonlite::fromJSON(NULL)` and `jsonlite::fromJSON("")` return errors,
#' this function will force a NULL or empty string to be interpreted as
#' `list()`. Otherwise, the output of this function is identical to
#' \code{\link[jsonlite]{fromJSON}}.
#'
#' @param json character to be interpreted as a JSON
#' @param ... additional arguments passed to \code{\link[jsonlite]{fromJSON}}
#'
#' @return list
#' @keywords internal
parse_json_or_empty <- function(json, ...) {
  if (is.null(json) || json == "") {
    list()
  } else {
    jsonlite::fromJSON(json, ...)
  }
}
