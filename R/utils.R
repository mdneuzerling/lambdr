#' Convert a list to a single character, preserving names
#'
#' @param x Named list.
#'
#' @return character
#' @export
#'
#' @examples \dontrun{
#' prettify_list(list(a = 1, b = 2, c = 3))
#' # "a=1, b=2, c=3"
#' }
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

#' Convert an object to JSON
#'
#' This function effectively wraps \code{\link[jsonlite]{toJSON}} with two
#' hardcoded arguments:
#'
#' * `auto_unbox` is set to `TRUE`, such that singleton values are not
#'   represented as lists.
#' * `NULL` values are represented as JSON `null`s.
#'
#' @param x R object to be converted to JSON.
#' @param ... additional arguments (except `auto_unbox` and `null`) passed to
#'   \code{\link[jsonlite]{toJSON}}
#'
#' @return character of class "json"
#' @keywords internal
as_json <- function(x, ...) {
  jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", ...)
}

#' Convert an R object to stringified JSON matching AWS Lambda conventions
#'
#' @description
#' Stringified JSON is a string which can be parsed as a JSON. While a standard
#' JSON interpretation of `list(number = 9)` would be `{"number":9}`,
#' a stringified JSON representation would be `"{\"number\":9}"`.
#'
#' This function will convert `NULL` values to JSON "nulls", to match the
#' convention used by Lambda event inputs, and values are automatically
#' unboxed.
#'
#' @param x R object to be converted to stringified JSON.
#'
#' @return character
#' @export
#'
#' @examples
#' as_stringified_json(list(number = 9))
#' "{\"number\":9}"
as_stringified_json <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  as.character(as_json(x))
}

default_response_headers <- list(
  "Accept" = "application/json, text/xml, application/xml, */*",
  "Content-Type" = ""
)

#' Decode a Base64 encoded value to a string
#'
#' Events coming via an API Gateway can have content with bodies encoded as
#' Base64. This is especially true for HTML API Gateways (as opposed to REST
#' API Gateways).
#'
#' @param x a Base64 string
#'
#' @return character
#' @export
#'
#' @examples
#' from_base64("eyJudW1iZXIiOjd9")
#' # "{\"number\":7}"
from_base64 <- function(x) {
  rawToChar(jsonlite::base64_dec(x))
}
