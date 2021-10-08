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
