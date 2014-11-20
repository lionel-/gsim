
library("gsim")
testthat::context("Outputs")


test_that("Simple assignment", {
  sims <- clone(new_sims)
  out <- sims({
    res <- switched
    I(res)
  })
  expect_identical(wells$switched, out)
})


test_that("Simple function, vectorized and unvectorized", {
  simple_loop <- array(dim = c(n_sims, 4))
  for (i in 1:n_sims)
    simple_loop[i, ] <- arm_sims@coef[i, ] + arm_sims@coef[i, ]

  sims <- clone(new_sims)
  add <- `+`
  simple_vectorized <- sims(I(beta + beta))
  simple_unvectorized <- sims(I(add(beta, beta)))

  expect_identical_output(simple_loop, simple_vectorized)
  expect_identical_output(simple_loop, simple_unvectorized)
})


test_that("Posterior predictive checks", {
  loop_fitted <- array(dim = c(n_sims, 3020))
  for (i in 1:n_sims) {
    loop_fitted[i, ] <- arm::invlogit(X %*% cbind(arm_sims@coef[i, ]))
  }

  loop_residuals <- sweep(loop_fitted, 2, wells$switched, function(a, b) b - a)
  loop_residuals_var <- apply(loop_residuals, 1, var)

  set.seed(123)
  rbinom(3020, 1, 0.1)  # To account for eval_first

  loop_y_rep <- array(dim = c(n_sims, 3020))
  loop_residuals_rep <- array(dim = c(n_sims, 3020))
  for (i in 1:n_sims) {
    loop_y_rep[i, ] <- rbinom(3020, 1, loop_fitted[i, ])
    loop_residuals_rep[i, ] <- loop_y_rep[i, ] - loop_fitted[i, ]
  }
  loop_residuals_var_rep <- apply(loop_residuals_rep, 1, var)
  
  dim(loop_residuals_var) <- c(n_sims, 1)
  dim(loop_residuals_var_rep) <- c(n_sims, 1)


  sims <- clone(new_sims)
  out <- sims({
    X <- cbind(intercept(), c_dist100, c_arsenic, c_dist100 * c_arsenic)
    fitted <- arm::invlogit(X %*% col_vector(beta))
    residuals <- switched - fitted
    residuals_var <- var(residuals)

    null <- set.seed(123)
    y_rep <- rbinom(3020, 1, fitted)
    residuals_rep <- y_rep - fitted
    residuals_var_rep <- var(residuals_rep)

    list(residuals_var, residuals_var_rep)
  })


  expect_identical_output(loop_residuals_var, sims(I(residuals_var)))
  expect_identical_output(loop_residuals_var_rep, sims(I(residuals_var_rep)))
})


test_that("Posterior predictive checks, hierarchical", {
  load(radon_sims_file)
})


test_that("Listed looped operations", {
  # TODO: Not clear what to do with calls such as
  #   list(3, X %*% beta)
  # or, in previous test,
  #   list(residuals_var, residuals_var_rep)

  # Probably need to differentiate between array operations and the
  # rest
})
