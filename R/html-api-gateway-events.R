#' Determine if a Lambda event is coming via an API Gateway
#'
#' @inheritSection is_from_rest_api_gateway Invocations via an API Gateway
#'
#' @inheritParams classify_event
#'
#' @return logical
#' @keywords internal
is_from_html_api_gateway <- function(event_content) {
  grepl("routeKey", event_content)
}

#' @export
parse_event_content.html_api_gateway_event <- function(event, config, ...) {
  parsed_json <- parse_json_or_empty(event$event_content)

  query_string_parameters <- parsed_json[["queryStringParameters"]]
  query_parameters <- if (is.null(query_string_parameters)) {
    list()
  } else {
    # convert "value1,value2" to c("value1", "value2")
    Map(
      function(x) strsplit(x, ",")[[1]],
      query_string_parameters
    )
  }

  # Parse the JSON within the JSON
  body <- parsed_json[["body"]]
  base64_encoded <- parsed_json[["isBase64Encoded"]]
  body_parameters <- decode_html_body(body, config, base64_encoded)

  # query parameters always named, should go last
  c(body_parameters, query_parameters)
}

#' @export
serialise_result.html_api_gateway_event <- function(event, ...) {
  serialise_result.rest_api_gateway_event(event, ...)
}

#' @export
handle_event_error.html_api_gateway_event <- function(event, ...) {
  handle_event_error.rest_api_gateway_event(event, ...)
}
