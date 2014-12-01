
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
    loop_fitted[i, ] <- inv_logit(X %*% cbind(arm_sims@coef[i, ]))
  }

  loop_residuals <- sweep(loop_fitted, 2, wells$switched, function(a, b) b - a)
  loop_residuals_var <- apply(loop_residuals, 1, var)

  set.seed(123)
  invisible(rbinom(3020, 1, 0.1))  # To account for eval_first

  loop_y_rep <- array(dim = c(n_sims, 3020))
  loop_residuals_rep <- array(dim = c(n_sims, 3020))
  for (i in 1:n_sims) {
    loop_y_rep[i, ] <- rbinom(3020, 1, loop_fitted[i, ])
    loop_residuals_rep[i, ] <- loop_y_rep[i, ] - loop_fitted[i, ]
  }
  loop_residuals_var_rep <- apply(loop_residuals_rep, 1, var)
  
  dim(loop_residuals_var) <- c(n_sims, 1)
  dim(loop_residuals_var_rep) <- c(n_sims, 1)

  loop_p <- mean(loop_residuals_var > loop_residuals_var_rep)


  sims <- clone(new_sims)
  out <- sims({
    X <- cbind(intercept(), c_dist100, c_arsenic, c_dist100 * c_arsenic)
    fitted <- inv_logit(X %*% col_vector(beta))
    residuals <- switched - fitted
    residuals_var <- var(residuals)

    null <- set.seed(123)
    y_rep <- rbinom(3020, 1, fitted)
    residuals_rep <- y_rep - fitted
    residuals_var_rep <- var(residuals_rep)

    I(list(residuals_var, residuals_var_rep))
  })

  p <- sims((residuals_var > residuals_var_rep) %% mean)

  expect_identical_output(loop_residuals_var, out$residuals_var)
  expect_identical_output(loop_residuals_var_rep, out$residuals_var_rep)
  expect_identical(loop_p, p)


  sims2 <- gsim(arm_sims, wells)
  out2 <- sims2({
    X <- cbind(intercept(), c_dist100, c_arsenic, c_dist100 * c_arsenic)
    fitted <- inv_logit(X %*% col_vector(beta))

    # Set seed and compensate for `eval_first`
    null <- set.seed(123)
    null <- rbinom(3020, 1, 0.1)

    I(check_bernoulli(y = switched, p = fitted, stat = var(y - p)))
  })

  expect_identical(loop_p, p(out2))
})


test_that("Direct ppc with bernoulli_check", {
  load(wells_sims_file)
  loop_fitted <- inv_logit(wells_sims$y_hat)
  loop_residuals <- sweep(loop_fitted, 2, wells$switched, function(a, b) b - a)
  loop_residuals_var <- apply(loop_residuals, 1, var)

  set.seed(123)
  loop_y_rep <- array(dim = c(n_sims, 3020))
  loop_residuals_rep <- array(dim = c(n_sims, 3020))
  for (i in 1:n_sims) {
    loop_y_rep[i, ] <- rbinom(3020, 1, loop_fitted[i, ])
    loop_residuals_rep[i, ] <- loop_y_rep[i, ] - loop_fitted[i, ]
  }
  loop_residuals_var_rep <- apply(loop_residuals_rep, 1, var)
  loop_p <- mean(loop_residuals_var > loop_residuals_var_rep)
  
  set.seed(123)
  out <- c(wells, wells_sims) %$%
    check_bernoulli(
      y = switched,
      p = inv_logit(y_hat),
      stat = sd(y - p)
    )

  expect_identical(loop_p, p(out))
})


test_that("Direct ppc with normal_check", {
  load(radon_sims_file)

  set.seed(123)
  loop_y_rep <- array(dim = c(n_sims, 919))
  for (i in 1:n_sims) {
    loop_y_rep[i, ] <- rnorm(919, radon_sims$y_hat[i, ],
      radon_sims$sigma_y[i])
  }
  loop_p <- mean(max(radon$y) > apply(loop_y_rep, 1, max))

  set.seed(123)
  out <- c(radon, radon_sims) %$%
    check_normal(y, y_hat, sigma_y, stat = max(y))

  expect_identical(loop_p, p(out))
})


test_that("Direct ppc with model_check", {
  skip("todo: manual loops")
  load(radon_sims_file)

  set.seed(123)
  out <- check_model(
    y = radon$y,
    y_rep = y_rep,
    params = radon_sims,
    stat = {
      y_sorted <- sort(y)
      abs(y_sorted[106] - y_hat) - abs(y_sorted[10] - y_hat)})

  set.seed(123)
  out2 <- c(radon, radon_sims) %$%
    check_normal(
      y = y, mu = y_hat, sigma = sigma_y,
      stat = {
        y_sorted <- sort(y)
        abs(y_sorted[106] - mu) - abs(y_sorted[10] - mu)})
})


test_that("Listed looped operations", {
  # TODO: Not clear what to do with calls such as
  #   list(3, X %*% beta)
  # or, in previous test,
  #   list(residuals_var, residuals_var_rep)

  # Probably need to differentiate between array operations and the
  # rest
})
