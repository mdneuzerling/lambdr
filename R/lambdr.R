#' lambdr: Create a Runtime for Serving Containerised R Functions on AWS Lambda
#'
#' @description
#' This package provides an R runtime for the
#' \href{https://aws.amazon.com/lambda/}{_AWS Lambda_ serverless compute
#' service}. It is intended to be used to create containers that can run on _AWS
#' Lambda_. `lambdr` provides the necessary functionality for handling the
#' various endpoints required for accepting new input and sending responses.
#'
#' This package is **unofficial**. Its creators are not affiliated with _Amazon
#' Web Services_, nor is its content endorsed by _Amazon Web Services_.
#' _Lambda_, _API Gateway_, _EventBridge_, _CloudWatch_, and _SNS_ are services
#' of _Amazon Web Services_.
#'
#' To see an example of how to use this package to create a runtime, refer to
#' \code{vignette("lambda-runtime-in-container", package = "lambdr")}.
#'
#' The following invocation types have been implemented to some degree. Refer to
#' the vignettes or the package website for more information.
#'
#' Alternatively, custom functions can be provided for parsing event content and
#' serialising results. Refer to \code{\link{lambda_config}} for more
#' information.
#'
#' @section Direct invocations:
#' `r lifecycle::badge('stable')`
#'
#' @section REST API Gateway invocations:
#' `r lifecycle::badge('experimental')`
#' \code{vignette("api-gateway-invocations", package = "lambdr")}
#'
#' @section HTML API Gateway invocations:
#' `r lifecycle::badge('experimental')`
#' \code{vignette("api-gateway-invocations", package = "lambdr")}
#'
#' @section EventBridge invocations:
#' `r lifecycle::badge('experimental')`
#' \code{vignette("eventbridge-and-sns-invocations", package = "lambdr")}
#'
#' @section SNS invocations:
#' `r lifecycle::badge('experimental')`
#' \code{vignette("eventbridge-and-sns-invocations", package = "lambdr")}
#'
#' @docType package
#' @name lambdr
NULL
