# handle_event <- function(event) {
#   status_code <- status_code(event)
#   log_debug("Status code:", status_code)
#   if (status_code != 200) {
#     stop_api("Didn't get status code 200. Status code: ", status_code,
#              code = 400)
#   }
#   event_headers <- headers(event)
#
#   # HTTP headers are case-insensitive
#   names(event_headers) <- tolower(names(event_headers))
#   log_debug("Event headers:", prettify_list(event_headers))
#
#   aws_request_id <- event_headers[["lambda-runtime-aws-request-id"]]
#   if (is.null(aws_request_id)) {
#     stop_api("Could not find lambda-runtime-aws-request-id header in event",
#              code = 400)
#   }
#
#   # According to the AWS guide, the below is used by "X-Ray SDK"
#   runtime_trace_id <- event_headers[["lambda-runtime-trace-id"]]
#   if (!is.null(runtime_trace_id)) {
#     Sys.setenv("_X_AMZN_TRACE_ID" = runtime_trace_id)
#   }
#
#   # we need to parse the event in four contexts before sending to the lambda fn:
#   # 1a) direct invocation with no function args (empty event)
#   # 1b) direct invocation with function args (parse and send entire event)
#   # 2a) api endpoint with no args (parse HTTP request, confirm null request
#   #   element; send empty list)
#   # 2b) api endpoint with args (parse HTTP request, confirm non-null request
#   #   element; extract and send it)
#
#   unparsed_content <- httr::content(event, "text", encoding = "UTF-8")
#   # Thank you to Menno Schellekens for this fix for Cloudwatch events
#   is_scheduled_event <- grepl("Scheduled Event", unparsed_content)
#   if(is_scheduled_event) log_info("Event type is scheduled")
#   log_debug("Unparsed content:", unparsed_content)
#   if (unparsed_content == "" || is_scheduled_event) {
#     # (1a) direct invocation with no args (or scheduled request)
#     event_content <- list()
#   } else {
#     # (1b, 2a or 2b)
#     event_content <- jsonlite::fromJSON(unparsed_content)
#   }
#
#   # if you want to do any additional inspection of the event body (including
#   # other http request elements if it's an endpoint), you can do that here!
#
#   # change `http_req_element` if you'd prefer to send the http request `body` to
#   # the lambda fn, rather than the query parameters
#   # (note that query string params are always strings! your lambda fn may need to
#   # convert them back to numeric/logical/Date/etc.)
#   is_http_req <- FALSE
#   http_req_element <- "queryStringParameters"
#
#   if (http_req_element %in% names(event_content)) {
#     is_http_req <- TRUE
#     if (is.null(event_content[[http_req_element]])) {
#       # (2a) api request with no args
#       event_content <- list()
#     } else {
#       # (2b) api request with args
#       event_content <- event_content[[http_req_element]]
#     }
#   }
#
#   result <- do.call(function_name, event_content)
#   log_debug("Result:", as.character(result))
#   response_endpoint <- paste0(
#     "http://", lambda_runtime_api, "/2018-06-01/runtime/invocation/",
#     aws_request_id, "/response"
#   )
#   # aws api gateway is a bit particular about the response format
#   body <- if (is_http_req) {
#     list(
#       isBase64Encoded = FALSE,
#       statusCode = 200L,
#       body =  as.character(jsonlite::toJSON(result, auto_unbox = TRUE))
#     )
#   } else {
#     result
#   }
#   POST(
#     url = response_endpoint,
#     body = body,
#     encode = "json"
#   )
#   rm("aws_request_id") # so we don't report errors to an outdated endpoint
# }

# log_info("Querying for events")
# while (TRUE) {
#   tryCatch(
#     {
#       event <- GET(url = next_invocation_endpoint)
#       log_debug("Event received")
#       handle_event(event)
#     },
#     api_error = function(e) {
#       log_error(as.character(e))
#       aws_request_id <-
#         headers(event)[["lambda-runtime-aws-request-id"]]
#       if (exists("aws_request_id")) {
#         log_debug("POSTing invocation error for ID:", aws_request_id)
#         invocation_error_endpoint <- paste0(
#           "http://", lambda_runtime_api, "/2018-06-01/runtime/invocation/",
#           aws_request_id, "/error"
#         )
#         POST(
#           url = invocation_error_endpoint,
#           body = list(
#             statusCode = e$code,
#             error_message = as.character(e$message)),
#           encode = "json"
#         )
#       } else {
#         log_debug("No invocation ID!",
#                   "Can't clear this request from the queue.")
#       }
#     },
#     error = function(e) {
#       log_error(as.character(e))
#       aws_request_id <-
#         headers(event)[["lambda-runtime-aws-request-id"]]
#       if (exists("aws_request_id")) {
#         log_debug("POSTing invocation error for ID:", aws_request_id)
#         invocation_error_endpoint <- paste0(
#           "http://", lambda_runtime_api, "/2018-06-01/runtime/invocation/",
#           aws_request_id, "/error"
#         )
#         POST(
#           url = invocation_error_endpoint,
#           body = list(error_message = as.character(e)),
#           encode = "json"
#         )
#       } else {
#         log_debug("No invocation ID!",
#                   "Can't clear this request from the queue.")
#       }
#     }
#   )
# }
