context("basic test")

test_that("check mode of x", {
  x <- rnorm(4L)
  expect_match(mode(x), "numeric")
})

