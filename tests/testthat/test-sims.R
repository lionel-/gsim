
context("sims objects")

test_that("subsetting", {
  sims <- radon_sims["mu_y"]
  expect_equal(class(sims), "sims")
})

test_that("n_sims() recover number of simulations", {
  expect_equal(n_sims(radon_sims), 100)
})

test_that("concatenated sims are in sim-major order", {
  skip("todo")
})
