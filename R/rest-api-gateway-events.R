#' Determine if a Lambda event is coming via a REST API Gateway
#'
#' @section Invocations via an API Gateway:
#' Events coming from an API Gateway need to be treated a little differently,
#' both in parsing the event content and in posting the results. Refer to
#' \code{vignette("api-gateway-invocations", package = "lambdr")} for details.
#'
#' @inheritParams classify_event
#'
#' @return logical
#' @keywords internal
is_from_rest_api_gateway <- function(event_content) {
  grepl("httpMethod", event_content)
}

#' @export
parse_event_content.rest_api_gateway_event <- function(event, ...) {
  logger::log_debug("Input coming via REST API Gateway")
  parsed_json <- parse_json_or_empty(event$event_content)

  query_parameters <- parsed_json[["queryStringParameters"]]
  if (is.null(query_parameters)) query_parameters <- list()

  # Parse the JSON within the JSON
  body <- parsed_json[["body"]]
  base64_encoded <- parsed_json[["isBase64Encoded"]]
  body_parameters <- if (!base64_encoded) {
    parse_json_or_empty(body)
  } else if (lambda$decode_base64) {
    parse_json_or_empty(from_base64(body))
  } else {
    body
  }

  # query parameters always named, should go last
  c(body_parameters, query_parameters)
}

#' @export
serialise_result.rest_api_gateway_event <- function(event, ...) {
  as_stringified_json(
    list(
      isBase64Encoded = FALSE,
      statusCode = 200L,
      body = as_stringified_json(event$result)
    )
  )
}

#' @export
handle_event_error.rest_api_gateway_event <- function(event, ...) {
  error_handling_function <- function(e) {
    html_code <- if (is.null(e$code)) 500L else e$code # internal server error
    logger::log_error(e$message)
    error_response <- list(
      statusCode = html_code,
      headers = list(
        "Content-Type" = "text/plain",
        "x-amzn-ErrorType" = class(e)[1]
      ),
      isBase64Encoded = FALSE,
      body = e$message
    )

    httr::POST(
      url = get_response_endpoint(event$request_id),
      body = as_stringified_json(error_response),
      encode = "raw",
      httr::content_type("application/vnd.aws.lambda.error+json")
    )
  }

  error_handling_function
}
