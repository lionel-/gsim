
context("mutate()")

test_that("new definitions are accurate", {
  sims <- radon_sims %>%
    mutate(
      shifted = mu_y + 10,
      resid_rep = y_rep - mu_y
    )

  shifted_expected <- array(dim = c(919, nsims))
  for (i in 1:nsims) {
    shifted_expected[, i] <- radon_sims$mu_y[i, ] + 10
  }
  resid_rep_expected <- array(dim = c(919, nsims))
  for (i in 1:nsims) {
    resid_rep_expected[, i] <- radon_sims$y_rep[i, ] - radon_sims$mu_y[i, ]
  }

  expect_equal_array(sims$shifted, shifted_expected)
  expect_equal_array(sims$resid_rep, resid_rep_expected)
})

test_that("extra data is in scope", {
  sims <- c(radon_sims, radon) %>% mutate(resid = y - mu_y)
  sims2 <- radon_sims %>% mutate(resid = y - mu_y, .context = radon)

  resid_expected <- array(dim = c(919, nsims))
  for (i in 1:nsims) {
    resid_expected[, i] <- radon$y - radon_sims$mu_y[i, ]
  }

  expect_equal_array(sims$resid, resid_expected)
  expect_equal_array(sims2$resid, resid_expected)
})

test_that("dimensionless parameters work", {
  sigma_out <- mutate(radon_sims, out = sigma) %>% .$out
  expect_that(mutate(radon_sims, sigma_sq = sigma^2), not(throws_error()))
  expect_identical(sigma_out, to_sims_major(radon_sims$sigma))
})

test_that("can subset parameters", {
  skip("fixme")
  sims <- mutate(radon_sims, beta = Beta[1, ])
})

test_that("apply() works inside mutate()", {
  skip("fixme")
  sims <- radon_sims %>% mutate(test = apply(Beta, 2, mean))
})
