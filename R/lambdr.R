#' lambdr: Create a Runtime for Serving Containerised R Functions on AWS Lambda
#'
#' @description
#' \if{html}{\figure{lambdr.png}{options: align='right' alt='logo' width='150'}}
#'
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
#' The default behaviour is to convert the body of the received event from JSON
#' into an R list using the `jsonlite` package. This works for direct invocations,
#' as well as situations where the user wishes to implement their own behaviour.
#'
#' Some invocation types have their own logic for converting the event body into
#' an R object. This is useful for say, using an R function in a Lambda behind
#' an API Gateway, so that the R function does not need to deal with the HTML
#' elements of the invocation. The below invocation types have custom logic
#' implemented. Refer to the vignettes or the package website for more
#' information.
#'
#' Alternatively, user-defined functions can be provided for parsing event
#' content and serialising results. The user can also use the `identity`
#' function as a deserialiser to pass the raw event content --- as a string ---
#' to the handler function. Refer to \code{\link{lambda_config}} for more
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
