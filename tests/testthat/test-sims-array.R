
context("sims_array objects")

test_that("can subset by dimension", {
  # No dropping of dimensions
  expect_equal(dim(radon_sims$Beta[1, 2]), c(100, 1, 1))
  expect_equal(dim(radon_sims$Beta[, 2]), c(100, 85, 1))

  # NSE works
  ind <- 2
  expect_equal(dim(radon_sims$Beta[, ind]), c(100, 85, 1))

  expect_error(dim(radon_sims$Beta[1, 2, 3]))
})

test_that("can subset by address", {
  out <- radon_sims$Beta[6:20][[10]]
  expected <- radon_sims$Beta[[10]][6:20]
  expect_identical(out, expected)
})

test_that("can subset out the sims_array", {
  out <- radon_sims$Beta[[1]]

  expect_equal(dim(out), c(85, 2))
  expect_is(out, "matrix")

  expected <- unclass(radon_sims$Beta)[1, , ]
  expect_identical(out, expected)

  rand_out <- radon_sims$Beta[[NULL]]
  expect_equal(dim(rand_out), c(85, 2))
  expect_is(rand_out, "matrix")
})
