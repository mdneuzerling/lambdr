#' Convert an R object into a string that can be included in a URL
#'
#' Before a character can be used as part of a html, spaces must be converted to
#' "%20", and R logicals must be converted to "true"/"false".
#'
#' @param input Logical or character.
#'
#' @export
#'
#' @return Character.
#'
make_url_friendly <- function(input) {
  if (is.logical(input)) {
    input <- ifelse(input, "true", "false")
  } else if (is.character(input)) {
    input <- gsub(" ", "%20", input)
  }

  as.character(input)
}

#' Suffix a parameter to a HTML request
#'
#' Parameters are suffixed to a URL, like so:
#' "request?para1=value1&para2=value2". The first parameter is suffixed with "?"
#' and all others after that "&". This function will determine the correct
#' suffix based on the presence of "&" in the request. If the given parameter
#' value is `NULL` or of length 0 then the URL is returned unchanged.
#'
#' @details There is no standardised way to combine multiple values for a
#' parameter. You should see how your API expects multiple values to be provided
#' to the same parameter. This function allows for the following strategies. If
#' any other value is provided, then the values will be concatenated and
#' separated with the provided value.
#' \itemize{
#'   \item "repeat_name" (default). The values of the parameter are repeated
#'     with the parameter name. For example, `("request", "para", c(1, 2))` will
#'     return "request?para=1&para=2".
#'   \item "with_commas". The values of the parameter are concatenated and
#'     separated with commas. For example, `("request", "para", c(1, 2))` will
#'     return "request?para=1,2".
#'   \item "with_commas". The values of the parameter are concatenated and
#'     separated with the ASCII keycode in hexadecimal for a comma ("%2C"). For
#'     example, `("request", "para", c(1, 2))` will return "request?para=1%2C2".
#' }
#'
#' @param request Character. The base URL or request which will be suffixed with
#' the parameter.
#' @param parameter_name Character. Name of parameter to suffix.
#' @param parameter_value Character, or a value that can be coerced to a
#' character. The value of the parameter to suffix.
#' @param .combine How to combine parameters with multiple values. One
#'   of "repeat_name", "with_commas", "with_hex_commas". Alternatively, a
#'   custom character can be provided here which will be used to separate the
#'   values. See Details.
#'
#' @return Character. The request with suffixed parameters.
#'
#' @export
#'
#' @examples
#' add_parameter("www.example.com", "animal", "crocodile")
#' add_parameter(
#'   "www.example.com",
#'   "numbers",
#'   c(1, 2, 3),
#'   .combine = "repeat_names"
#' )
add_parameter <- function(request,
                          parameter_name,
                          parameter_value,
                          .combine = "repeat_name") {
  if (is.null(parameter_value) || length(parameter_value) == 0) {
    return(request)
  }

  if (length(parameter_value) == 1) {
    conjunction <- ifelse(grepl("\\?", request), "&", "?")
    paste0(
      request,
      conjunction,
      parameter_name,
      "=",
      make_url_friendly(parameter_value)
    )
  } else if (.combine == "repeat_name") {
    for (value in parameter_value) {
      request <- add_parameter(
        request,
        parameter_name,
        value,
        .combine = .combine
      )
    }
    request
  } else if (.combine == "with_commas") {
    add_parameter(request, parameter_name, parameter_value, .combine = ",")
  } else if (.combine == "with_hex_commas") {
    add_parameter(request, parameter_name, parameter_value, .combine = "%2C")
  } else {
    combined_value <- paste(parameter_value, collapse = .combine)
    add_parameter(request, parameter_name, combined_value)
  }
}


#' Suffix one or many parameters to a HTML request
#'
#' This function wraps \code{\link{add_parameter}}, allowing for multiple
#' parameters to be suffixes to a URL at once, and for parameters and their
#' values to be provided as named arguments. Parameters are suffixed to a URL,
#' like so: "request?para1=value1&para2=value2". The first parameter is suffixed
#' with "?" and all others after that "&". This function will determine the
#' correct suffix based on the presence of "&" in the request. If a given
#' parameter value is `NULL` or of length 0 then it will not be appended to the
#' URL.
#'
#' @inherit add_parameter details
#'
#' @inheritParams add_parameter
#' @param ... The parameters to be suffixed, with name/value pairs provided as
#'   arguments.
#'
#' @inherit add_parameter return
#'
#' @export
#'
#' @examples
#' add_parameters("www.example.com", animal = "crocodile")
#' add_parameters(
#'   "www.example.com",
#'   animal = "crocodile",
#'   food = "cherries"
#' )
#' add_parameters(
#'   "www.example.com",
#'   animal = "crocodile",
#'   numbers = c(1, 2, 3),
#'   .combine = "repeat_names"
#' )
add_parameters <- function(request, ..., .combine = "repeat_name") {
  dots <- list(...)
  dots_names <- names(dots)
  for (i in seq_along(dots)) {
    dot_name <- dots_names[i]
    dot_value <- dots[[i]]
    if (is.null(dot_name) || dot_name == "") stop("Parameters must be named")
    # add_parameter will error if dot_value if not a singletons, and will
    # return the request unaltered if dot_value is NULL
    request <- add_parameter(request, dot_name, dot_value, .combine = .combine)
  }
  request
}
