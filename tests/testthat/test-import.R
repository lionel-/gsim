
context("Creation of sims objects")

check_packages("rstan", "rjags")

test_that("Jags and CODA simulations are correctly imported", {
  skip_on_cran()
  if (skip_heavy_computations) {
    skip("Skipping Jags simulations")
  }

  jags_radon_multiple_data <- radon_data() %>%
    define(
      y, county,
      X = design(1, x),
      U = design(1, u, .unjoin = county),
      N = nrow(.),
      J = n_distinct(county),
      L = 2, K = 2,
      W = diag(2)
    )
  jags_radon_model <- radon_model("jags", "multiple", TRUE)

  jags_model_array <- rjags::jags.model(jags_radon_model,
    data = jags_radon_multiple_data, n.chains = 3)
  rjags:::update.jags(jags_model_array, n.iter = 200)

  jags_samples <- rjags::jags.samples(jags_model_array,
    c("B_raw", "sigma_y"), n.iter = 200)
  jags_radon_sims <- as_sims(jags_samples)

  expect_equal(jags_samples[[1]][, , 1, 1], jags_radon_sims[[1]][, , 1])
  expect_equal(jags_samples[[1]][, , 200, 3], jags_radon_sims[[1]][, , 600])
  expect_equal(jags_samples[[2]][, 1, 1], jags_radon_sims[[2]][, 1])
  expect_equal(jags_samples[[2]][, 200, 3], jags_radon_sims[[2]][, 600])

  coda_samples <- rjags::coda.samples(jags_model_array,
    c("B_raw", "sigma_y"), n.iter = 200)
  coda_radon_sims <- as_sims(coda_samples)

  expected_first <- coda_samples[[1]][1, 1:170] %>% unname()
  actual_first <- coda_radon_sims[[1]][1, , ] %>% c()
  expect_equal(expected_first, actual_first)

  expected_last <- coda_samples[[3]][200, 1:170] %>% unname()
  actual_last <- coda_radon_sims[[1]][600, , ] %>% c()
  expect_equal(expected_last, actual_last)
})

test_that("Stan simulations are correctly imported", {
  skip_on_cran()
  if (skip_heavy_computations) {
    skip("Skipping Stan simulations")
  }

  # Temporary workaround
  import::from("Rcpp", cpp_object_initializer)

  # Radon simulations used in tests
  stan_radon_data <- radon_data() %>%
    define(
      y, county,
      X = design(1, x),
      U = design(1, u, .unjoin = county),
      N = nrow(.),
      J = n_distinct(county),
      P_X = ncol(X),
      P_U = ncol(U)
    )

  radon_m <- radon_model(language = "stan", variant = "centered")
  radon_stanfit <- rstan::stan(model_code = radon_m,
    data = stan_radon_data, iter = 100, chains = 2)

  radon_sims <- radon_stanfit %>%
    select(mu_y, y_rep, Beta, sigma, Gamma, sigma_Beta, Omega)
  radon_sims_stan <- rstan::extract(radon_stanfit)

  expect_equal_array(radon_sims_stan$Beta, radon_sims$Beta)
  expect_equal_array(radon_sims_stan$sigma_Beta, radon_sims$sigma_Beta)
  expect_equal_array(radon_sims_stan$sigma, radon_sims$sigma)
})
