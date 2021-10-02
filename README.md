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

In a `runtime.R` file:

```{r}
library(lambdr)

# source functions here

setup_lambda() # relies on environment variables configured by AWS

wait_for_events()
```
