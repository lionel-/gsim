
library("gsim")
testthat::context("Internal and modeling utils")


test_that("Dynamic lookup", {})

test_that("`intercept()` recovers correct `n`", {
  x1 <- cbind(intercept(), 1:5)
  x2 <- data.frame(intercept(), 1:5)
  x3 <- data_frame(intercept(), 1:5)
  expect_equal(dim(x1)[1], 5)
  expect_equal(dim(x2)[1], 5)
  expect_equal(dim(x3)[1], 5)

  y <- 1:5
  x4 <- cbind(y, intercept())
  expect_equal(dim(x4)[1], 5)

  sims <- clone(new_sims)
  x5 <- sims(I(cbind(intercept(), 1:5)))
  expect_equal(dim(x5)[1], 5)
})
