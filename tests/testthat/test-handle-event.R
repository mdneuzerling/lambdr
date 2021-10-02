test_that("Mock square root function", {

  webmockr::enable(quiet = TRUE)

  withr::with_envvar(
    c("AWS_LAMBDA_RUNTIME_API" = "red_panda",
      "LAMBDA_TASK_ROOT" = "giraffe",
      "_HANDLER" = "sqrt"),
    setup_lambda()
  )
  withr::defer({
    webmockr::disable(quiet = TRUE)
    reset_lambda()
  })

  invocation_endpoint <- "http://red_panda/2018-06-01/runtime/invocation/next"
  response_endpoint <- "http://red_panda/2018-06-01/runtime/invocation/abc123/response"
  webmockr::stub_request("get", invocation_endpoint) %>%
    webmockr::to_return(
      body = jsonlite::toJSON(list(x = 4)),
      headers = list("lambda-runtime-aws-request-id" = "abc123"),
      status = 200
    )
  webmockr::stub_request("post", response_endpoint) %>%
    webmockr::to_return(status = 200)

  named_sqrt <- function(x) {
    list(result = sqrt(x))
  }

  start_listening(timeout_seconds = 1)

  requests <- webmockr::request_registry()
  n_responses <- requests$times_executed(
    webmockr::RequestPattern$new("post", response_endpoint)
  )
  expect_gte(n_responses, 3) # at least 3 response POSTs
})
