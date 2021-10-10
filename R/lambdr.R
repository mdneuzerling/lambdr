#' lambdr: Create a Runtime for Serving Containerised R Functions on AWS Lambda
#'
#' @description
#' Runtime for serving containers that can execute R code on the
#' \href{https://aws.amazon.com/lambda/}{_AWS Lambda_ serverless compute
#' service}. Provides the necessary functionality for handling the various
#' endpoints required for accepting new input and sending responses.
#'
#' To see an example of how to use this package to create a runtime, refer to
#' \code{vignette("lambda-runtime-in-container", package = "lambdr")}.
#'
#' @section Direct invocations:
#' `r lifecycle::badge('maturing')`
#'
#' @section API Gateway invocations:
#' `r lifecycle::badge('experimental')`
#'
#' @section Cloudwatch scheduled event invocations:
#' `r lifecycle::badge('experimental')`
#'
#' @docType package
#' @name lambdr
NULL
