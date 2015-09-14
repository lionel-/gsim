
context("sims objects")

test_that("subsetting", {
  sims <- radon_sims["mu_y"]
  expect_equal(class(sims), "sims")
})

test_that("degenerate sims objects behave", {
  degenerate_sims <- radon_sims
  degenerate_sims[[1]] <- degenerate_sims[[1]][1:10, ]

  # Looses the sims_array class when we modify the number of
  # simulations but not when we modify the data
  expect_false(is_sims_array(degenerate_sims[[1]]))
})

test_that("n_sims() recover number of simulations", {
  expect_equal(n_sims(radon_sims), 100)

  degenerate_sims <- radon_sims
  degenerate_sims[[1]] <- degenerate_sims[[1]][1:10, ]
  expect_equal(n_sims(degenerate_sims), 100)
})

test_that("concatenated sims are in sim-major order", {
  skip("todo")
})
