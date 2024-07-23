# lambdr <img src="man/figures/lambdr.png" style="height:200px;" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/mdneuzerling/lambdr/workflows/R-CMD-check/badge.svg)](https://github.com/mdneuzerling/lambdr/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/lambdr)](https://cran.r-project.org/package=lambdr)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/lambdr)](https://cran.r-project.org/package=lambdr)
[![Last commit](https://img.shields.io/github/last-commit/mdneuzerling/lambdr/main.svg)](https://github.com/mdneuzerling/lambdr/tree/main)
[![Codecov test coverage](https://codecov.io/gh/mdneuzerling/lambdr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/mdneuzerling/lambdr?branch=main)
[![license](https://img.shields.io/badge/license-MIT-lightgrey.svg)](https://choosealicense.com/licenses/mit/)
<!-- badges: end -->

This package provides an R runtime interface client for the [_AWS Lambda_
serverless compute service](https://aws.amazon.com/lambda/). It makes it
possible to deploy code written in R as _Lambda functions_ when using container
images.

This package is **unofficial**. Its creators are not affiliated with
_Amazon Web Services_, nor is its content endorsed by _Amazon Web Services_.
_Lambda_, _API Gateway_, _EventBridge_, _CloudWatch_, and _SNS_ are services of
_Amazon Web Services_.

## The runtime

Any programming language can be used to create a container-based Lambda
function. All that's required is a language-specific runtime. AWS provides such
runtimes for a handful of languages, e.g. Python, Go, and Ruby - but not for R.

As a runtime for R, `lambdr`'s job is to cordinate the translation and transfer
of inputs/responses between the outside world and an invoked Lambda's `handler`
function.

The default behaviour is to convert the body of the received event from JSON
into arguments for the Lambda's `handler` function using the `jsonlite` package.

For example, a raw event body of `{"number": 9}` will be converted to
`list(number = 9)`. The handler function will then receive the arguments
directly after unlisting, eg. `number = 9`. This works for direct invocations,
as well as situations where the user wishes to implement behaviour specific to
a trigger.

Some invocation types have their own logic for converting the event body into
an R object. This is useful for say, using an R function in a Lambda behind
an API Gateway, so that the R function does not need to deal with the HTML
elements of the invocation. The below invocation types have custom logic
implemented. Refer to the vignettes or the package website for more
information.

Alternatively, user-defined functions can be provided for parsing event
content and serialising results. The user can also use the `identity`
function as a deserialiser to pass the raw event content --- as a string ---
to the handler function. Refer to `?lambda_config` for more information.

 invocation type | implementation stage
|:---------------|:---------------------|
 direct | <img src="man/figures/lifecycle-stable.svg"/>
 API Gateway (REST) | <img src="man/figures/lifecycle-experimental.svg"/>
 API Gateway (HTML) | <img src="man/figures/lifecycle-experimental.svg"/>
 EventBridge | <img src="man/figures/lifecycle-experimental.svg"/>
 SNS | <img src="man/figures/lifecycle-experimental.svg"/>

## Installation

The package can be installed from CRAN with:

```r
install.packages("lambdr")
```

The development version is available with:

```r
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
Lambda events. For debugging and testing, values can be provided to the function in the absence of environment variables. See `?lambdr::lambda_config` for
details.

## Example

Consider the following `runtime.R` file:

```r
parity <- function(number) {
  list(parity = if (as.integer(number) %% 2 == 0) "even" else "odd")
}

lambdr::start_lambda()
```

The `parity` function accepts a `number` argument and returns its parity as a named list, for example:

```r
parity(5)
# $parity
# [1] "odd"


parity(8)
# $parity
# [1] "even"
```

This function can then be placed into a Docker image. An **example** is provided below, but the key components are:

* Start from the `public.ecr.aws/lambda/provided:al2` parent image, which provides the basic components necessary to serve a Lambda
* Install R and dependencies, both system dependencies and R packages, including the `lambdr` package
* Copy across `runtime.R` and any other necessary files
* Generate a simple bootstrap which runs `runtime.R` with R
* Set the handler as the `CMD`. The `lambdr` package interprets the handler as the name of the function to use, in this case, "parity". The `CMD` can also be set (or overriden) when setting up the Lambda in AWS.

```dockerfile
FROM public.ecr.aws/lambda/provided:al2-x86_64

ENV R_VERSION=4.0.3

RUN yum -y install wget git tar

RUN yum -y install https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm \
  && wget https://cdn.rstudio.com/r/centos-7/pkgs/R-${R_VERSION}-1-1.x86_64.rpm \
  && yum -y install R-${R_VERSION}-1-1.x86_64.rpm \
  && rm R-${R_VERSION}-1-1.x86_64.rpm

ENV PATH="${PATH}:/opt/R/${R_VERSION}/bin/"

# System requirements for R packages
RUN yum -y install openssl-devel

RUN Rscript -e "install.packages(c('httr', 'jsonlite', 'logger', 'remotes'), repos = 'https://packagemanager.rstudio.com/all/__linux__/centos7/latest')"
RUN Rscript -e "remotes::install_github('mdneuzerling/lambdr')"

RUN mkdir /lambda
COPY runtime.R /lambda
RUN chmod 755 -R /lambda

RUN printf '#!/bin/sh\ncd /lambda\nRscript runtime.R' > /var/runtime/bootstrap \
  && chmod +x /var/runtime/bootstrap

CMD ["parity"]
```

For production uses it's recommended to replace the "al2" tag above with a specific tag for the image.

The image is built and uploaded to AWS Elastic Container Registry (ECR). First, a repository is created:

```bash
aws ecr create-repository --repository-name parity-lambda --image-scanning-configuration scanOnPush=true
```

This provides a URI, the resource identifier of the created repository. The image can now be pushed:

```bash
docker build -f dockerfile -t r-on-lambda .   
docker tag mdneuzerling/r-on-lambda:latest {URI}/parity-lambda:latest
aws ecr get-login-password | docker login --username AWS --password-stdin {URI}
docker push {URI}/parity-lambda:latest
```

In either the AWS console or the command line, a Lambda can be created from this image. Call the Lambda "parity" to match the function name. Tests can be executed within the console. Alternatively the Lambda can be invoked from the CLI:

```bash
aws lambda invoke --function-name parity \
  --invocation-type RequestResponse --payload '{"number": 8}' \
  /tmp/response.json --cli-binary-format raw-in-base64-out
```

The output is now available in the generated file:

```bash
cat /tmp/response.json            
```

```bash
{"parity":"even"}
```

---

Hex logo by Phizz Leeder
