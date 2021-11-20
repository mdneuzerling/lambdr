test_that("lists are prettified", {
  expect_equal(
    prettify_list(list()),
    ""
  )
  expect_equal(
    prettify_list(list(a = 1)),
    "a=1"
  )
  expect_equal(
    prettify_list(list(a = 1, b = 2, c = 3)),
    "a=1, b=2, c=3"
  )
})

test_that("JSON converted with proper NULLs and singleton values", {
  json_string <- function(x) structure(x, class = "json")

  # NULL as null
  expect_equal(
    as_json(NULL),
    json_string("null")
  )

  # c() as null, list() as []
  expect_equal(
    as_json(list(x = c())),
    json_string('{"x":null}')
  )
  expect_equal(
    as_json(list(x = list())),
    json_string('{"x":[]}')
  )

  # auto-unboxing
  expect_equal(
    as_json(list(x = 1)),
    json_string('{"x":1}')
  )
  expect_equal(
    as_json(list(x = 1, y = c(1, 2))),
    json_string('{"x":1,"y":[1,2]}')
  )
})

test_that("we can decode Base64 strings", {
  expect_equal(
    from_base64("eyJudW1iZXIiOjd9"),
    "{\"number\":7}"
  )
})

test_that("from_base64 propagates NULLs", {
  expect_null(from_base64(NULL))
})

test_that("HTML body decoding accommodates for base64", {
  # should only decode base_64 when config$decode_base64 and base64_encoded are
  # both TRUE
  expect_equal(
    decode_html_body("eyJudW1iZXIiOjd9",
                     list(decode_base64 = TRUE),
                     base64_encoded = TRUE),
    list(number = 7)
  )

  expect_equal(
    decode_html_body("eyJudW1iZXIiOjd9",
                     list(decode_base64 = FALSE),
                     base64_encoded = TRUE),
    "eyJudW1iZXIiOjd9"
  )

  # everything else gets treated as JSON
  expect_equal(
    decode_html_body('{"number": 7}',
                     list(decode_base64 = TRUE),
                     base64_encoded = FALSE),
    list(number = 7)
  )

  expect_equal(
    decode_html_body('{"number": 7}',
                     list(decode_base64 = FALSE),
                     base64_encoded = FALSE),
    list(number = 7)
  )
})

test_that("we can mark results as having already been serialised", {
  expect_true(
    attr(mark_as_already_serialised(3), "already_serialised")
  )
})

test_that("we can produce custom HTML results", {
  expect_equal(
    html_response("abc"),
    mark_as_already_serialised(
      "{\"body\":\"abc\",\"isBase64Encoded\":false,\"statusCode\":200}"
    )
  )

  expect_equal(
    html_response("abc", is_base64 = TRUE),
    mark_as_already_serialised(
      "{\"body\":\"abc\",\"isBase64Encoded\":true,\"statusCode\":200}"
    )
  )

  expect_equal(
    html_response("abc", headers = list(x = "a")),
    mark_as_already_serialised(
      paste0(
        "{\"body\":\"abc\",\"isBase64Encoded\":false,\"statusCode\":200,",
        "\"headers\":{\"x\":\"a\"}}"
      )
    )
  )
})

test_that("we can add content types to custom HTML responses", {
  expect_equal(
    html_response("abc", content_type = "text/html"),
    mark_as_already_serialised(
      paste0(
        "{\"body\":\"abc\",\"isBase64Encoded\":false,\"statusCode\":200,",
        "\"headers\":{\"Content-Type\":\"text/html\"}}"
      )
    )
  )

  expect_equal(
    html_response(
      "abc",
      headers = list(`Content-Type` = "application/pdf"),
      content_type = "text/html" # Expect this to be overridden by the above
    ),
    mark_as_already_serialised(
      paste0(
        "{\"body\":\"abc\",\"isBase64Encoded\":false,\"statusCode\":200,",
        "\"headers\":{\"Content-Type\":\"application/pdf\"}}"
      )
    )
  )
})
