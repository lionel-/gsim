
library("gsim")
testthat::context("Manipulations of posterior objects")

test_that("Initialization produces correct dimensions", {
  x1 <- 1:4
  out1 <- init_posterior(x1, 100)
  expect_equal(dim(out1), c(100, 4))

  x2 <- 4
  out2 <- init_posterior(x2, 100)
  expect_equal(dim(out2), c(100, 1))

  x3 <- matrix(NA, 4, 5)
  out3 <- init_posterior(x3, 100)
  expect_equal(dim(out3), c(100, 4, 5))

  x4 <- matrix(NA, 4, 1)
  out4 <- init_posterior(x4, 100)
  expect_equal(dim(out4), c(100, 4))

  x5 <- 1:4
  dim(x5) <- 4
  out5 <- init_posterior(x5, 100)
  expect_equal(dim(out5), c(100, 4))
})
