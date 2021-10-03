# lambdr

<!-- badges: start -->
[![R-CMD-check](https://github.com/mdneuzerling/lambdr/workflows/R-CMD-check/badge.svg)](https://github.com/mdneuzerling/lambdr/actions)
<!-- badges: end -->

Running R containers on AWS Lambda.

This package is in active development, and is not yet ready for use.

## Installation

``` r
remotes::install_github("mdneuzerling/lambdr")
```

## Running

In a `runtime.R` file, source all functions needed and then run:

```{r}
lambdr::start_lambda()
```

This `runtime.R` file should be executed by the Docker image containing your
Lambda code.

The `lambdr::start_lambda()` function relies on environment variables
configured by AWS. It will fail if run locally. In particular, the _handler_ as
configured by the user through AWS will determine which function handles the
Lambda events. For debugging and testing, values can be provided to the function in the absence of environment variables. See `?lambdr::start_lambda` for
details.
