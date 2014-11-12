
library("gsim")
context("Equality of outputs and manually looped results")

test_that("Simple function", {
  simple_loop <- array(dim = c(n_sims, 4))
  for (i in 1:n_sims)
    simple_loop[i, ] <- arm_sims@coef[i, ] + arm_sims@coef[i, ]

  simple_vectorized <- get_clean_sims()(I(beta + beta))

  add <- `+`
  simple_unvectorized <- get_clean_sims()(I(add(beta, beta)))

  expect_identical_output(simple_loop, simple_vectorized)
  expect_identical_output(simple_loop, simple_unvectorized)
})
