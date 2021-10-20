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
