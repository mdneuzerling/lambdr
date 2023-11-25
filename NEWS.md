# lambdr 1.2.5

* Update docs to include information on Docker tags.

# lambdr 1.2.4

* Fixed incorrect argument name in internal documentation.

# lambdr 1.2.3

* Removed all verbatim chunks (marked by three backticks) in help docs, to meet
  new CRAN requirements.
* Documented package with "_PACKAGE" sentinel to meet new CRAN requirements.

# lambdr 1.2.2

* Removed all `center` attributes in `<img>` tags, in line with new CRAN policy.

# lambdr 1.2.1

* Fixed a bug that was preventing `lambdr` from identifying events coming from
  AWS SNS.
* Default to the `formatter_paste` log formatter instead of `formatter_glue`, 
  which can cause issues when logging JSON objects.

# lambdr 1.2.0

* Added `html_response` for sending bespoke responses to API Gateways.

# lambdr 1.1.0

* Removed `setup_logging`. Logging thresholds can be changed directly with
  `logger::log_threshold`. An example has been added to `?start_lambda`.
* Improved documentation for how deserialisation works in README and `?lambdr`.
* Minor changes for CRAN submission.

# lambdr 1.0.0

* Initial release
