test_that("errors can have HTML codes", {
  err500 <- tryCatch(
    stop_html("my heart is a fish"),
    error = function(e) e
  )
  expect_identical(err500$code, 500L)

  err404 <- tryCatch(
    stop_html("my heart is a fish", code = 404L),
    error = function(e) e
  )
  expect_identical(err404$code, 404L)
})
