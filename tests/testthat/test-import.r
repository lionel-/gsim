
library("gsim")
testthat::context("Correct import of simulations")

if (!requireNamespace("arm", quietly = TRUE))
  stop("The package arm must be installed", call. = FALSE)


test_that("Simulations from lm objects are correctly imported", {
  wells_lm_fit <- lm(switched ~ c_dist100 * c_arsenic, wells)

  set.seed(100)
  sims_lm_arm <- arm::sim(wells_lm_fit)
  sims_lm <- gsim(sims_lm_arm)

  expect_identical_output(sims_lm_arm@coef, sims_lm(I(beta)))
  expect_identical_output(sims_lm_arm@sigma, sims_lm(I(sigma)))


  set.seed(100)
  sims_lm_direct <- gsim(wells_lm_fit)
  sims_lm_direct2 <- gsim(wells_lm_fit, n_sims = 5)

  expect_identical_output(sims_lm_arm@coef, sims_lm_direct(I(beta)))
  expect_equal(dim(sims_lm_direct2(I(beta))), c(5, 4))
})


test_that("Simulations from glm objects are correctly imported", {
  wells_glm_fit <- glm(switched ~ c_dist100 * c_arsenic, binomial, wells)

  set.seed(100)
  sims_glm_arm <- arm::sim(wells_glm_fit)
  sims_glm <- gsim(sims_glm_arm)

  expect_identical_output(sims_glm_arm@coef, sims_glm(I(beta)))
  expect_identical_output(sims_glm_arm@sigma, sims_glm(I(sigma)))


  set.seed(100)
  sims_glm_direct <- gsim(wells_glm_fit)
  sims_glm_direct2 <- gsim(wells_glm_fit, n_sims = 5)

  expect_identical_output(sims_glm_arm@coef, sims_glm_direct(I(beta)))
  expect_equal(dim(sims_glm_direct2(I(beta))), c(5, 4))
})


test_that("Simulations from polr objects are correctly imported", {
  if (!requireNamespace("MASS", quietly = TRUE))
    stop("The package MASS must be installed", call. = FALSE)

  polr_fit <- polr(Sat ~ Infl + Type + Cont,
    weights = Freq, data = housing)

  set.seed(100)
  sims_polr_arm <- suppressMessages(arm::sim(polr_fit))
  sims_polr <- gsim(sims_polr_arm)

  expect_identical_output(sims_polr_arm@coef, sims_polr(I(beta)))
  expect_identical_output(sims_polr_arm@zeta, sims_polr(I(z_eta)))
})


test_that("Simulations from lmer objects are correctly imported", {
  radon <- get_radon_data()
  radon_lmer_fit <- lmer(y ~ x + u + x:u + (1 + x | county), radon)

  set.seed(100)
  sims_lmer_arm <- arm::sim(radon_lmer_fit)
  sims_lmer <- gsim(sims_lmer_arm, radon)

  expect_identical_output(sims_lmer_arm@fixef, sims_lmer(I(beta)))
  expect_identical_output(sims_lmer_arm@sigma, sims_lmer(I(sigma)))
  expect_identical_output(sims_lmer_arm@ranef$county, sims_lmer(P(county_coefs)))


  set.seed(100)
  sims_lmer_direct <- gsim(radon_lmer_fit)
  sims_lmer_direct2 <- gsim(radon_lmer_fit, n_sims = 5)

  expect_identical_output(sims_lmer_arm@fixef, sims_lmer_direct(I(beta)))
  expect_equal(dim(sims_lmer_direct2(I(beta))), c(5, 4))
  expect_is(sims_lmer_direct(P(fitted)), "posterior")


  radon2 <- radon %>% mutate(radon = ifelse(radon < mean(radon), 0, 1))
  radon_glmer_fit <- glmer(radon ~ x + u + x:u + (1 + x | county),
    radon2, binomial)

  set.seed(100)
  sims_glmer_arm <- arm::sim(radon_glmer_fit)
  set.seed(100)
  sims_glmer_direct <- gsim(radon_glmer_fit)

  expect_identical_output(sims_glmer_arm@fixef, sims_glmer_direct(I(beta)))
})


test_that("Stan simulations are correctly imported", {
  skip_on_cran()
  if (skip_heavy_computations)
    skip("Skipping Stan simulations")

  if (!requireNamespace("rstan", quietly = TRUE))
    stop("The package rstan must be installed", call. = FALSE)

  stan_data <- list(
    N = 3020,
    X = X,
    switched = wells$switched
  )
  stan_m <- "
    data {
      int N;
      int switched[N];
      matrix[N, 4] X;
    }
    
    parameters {
      vector[4] beta;
    }
    
    model {
      switched ~ bernoulli_logit(X * beta);
    }
  "

  stan_fit <- rstan::stan(model_code = stan_m, data = stan_data, iter = 200)
  sims_stan <- extract(stan_fit)
  sims <- gsim(stan_fit)
  expect_identical_output(sims_stan$beta, sims(I(beta)))


  stan_radon_data <- c(radon %>% select(-radon), list(N = nrow(radon)))
  stan_radon_m <- "
    data {
      int<lower=0> N; 
      int<lower=1, upper=85> county[N];
      vector[N] u;
      vector[N] x;
      vector[N] y;
    } 

    transformed data {
      vector[N] inter;
    
      inter <- u .* x;
    }

    parameters {
      vector[85] a;
      vector[85] b;
      vector[2] beta;
      real mu_a;
      real mu_b;
      real mu_beta;
      real<lower=0,upper=100> sigma_a;
      real<lower=0,upper=100> sigma_b;
      real<lower=0,upper=100> sigma_beta;
      real<lower=0,upper=100> sigma_y;
    } 

    transformed parameters {
      vector[N] y_hat;
    
      for (i in 1:N)
        y_hat[i] <- a[county[i]] + x[i] * b[county[i]] + beta[1] * u[i]     
          + beta[2] * inter[i];
    }

    model {
      mu_beta ~ normal(0, 1);
      beta ~ normal(100 * mu_beta, sigma_beta);
    
      mu_a ~ normal(0, 1);
      a ~ normal (mu_a, sigma_a);
    
      mu_b ~ normal(0, 1);
      b ~ normal (0.1 * mu_b, sigma_b);
    
      y ~ normal(y_hat, sigma_y);
    }
  "

  stan_radon_fit <- rstan::stan(model_code = stan_radon_m, data = stan_radon_data, iter = 200)
  sims_radon_stan <- extract(stan_radon_fit)
  sims_radon <- gsim(stan_radon_fit)
  expect_identical_output(sims_radon_stan$beta, sims_radon(I(beta)))
  expect_identical_output(sims_radon_stan$b, sims_radon(I(b)))
})


test_that("Jags and CODA simulations are correctly imported", {
  skip_on_cran()
  if (skip_heavy_computations)
    skip("Skipping Jags simulations")

  if (!requireNamespace("rjags", quietly = TRUE))
    stop("The package rjags must be installed", call. = FALSE)


  jags_data <- wells %$% list(
    N = 3020,
    c_dist100 = c_dist100,
    c_arsenic = c_arsenic,
    switched = switched
  )
  jags_radon_data <- c(radon %>% select(-radon), list(N = nrow(radon)))

  temp_file <- tempfile("jags-test", fileext = ".jag")
  temp_file_single_param <- tempfile("jags-test-single-param", fileext = ".jag")

  cat("
    model {
      for (n in 1:N) {
        mu[n] <- beta[1] + beta[2] * c_dist100[n] + beta[3] * c_arsenic[n] + beta[4] * c_dist100[n] * c_arsenic[n]
        switched[n] ~ dnorm(mu[n], sigma)
      }

      for (p in 1:4) {
        beta[p] ~ dnorm(0, 1e-4)
      }
      sigma ~ dunif(0, 10)
    }
  ", file = temp_file)
  cat("
    model {
      for (n in 1:N) {
        logit(pi[n]) <- beta[1] + beta[2] * c_dist100[n] + beta[3] * c_arsenic[n] + beta[4] * c_dist100[n] * c_arsenic[n]
        switched[n] ~ dbern(pi[n])
      }

      for (p in 1:4) {
        beta[p] ~ dnorm(0, 1e-4)
      }
    }
  ", file = temp_file_single_param)


  jags_model <- rjags::jags.model(temp_file, data = jags_data, n.chains = 3)
  jags_model_single_param <- rjags::jags.model(temp_file_single_param, data = jags_data, n.chains = 3)
  jags_model_single_chain <- rjags::jags.model(temp_file, data = jags_data, n.chains = 1)
  rjags:::update.jags(jags_model, n.iter = 200)
  rjags:::update.jags(jags_model_single_param, n.iter = 200)
  rjags:::update.jags(jags_model_single_chain, n.iter = 200)


  sims_jags <- rjags::jags.samples(jags_model, c("beta", "sigma"), n.iter = 200)
  sims <- gsim(sims_jags)
  expect_identical(sims_jags[[1]][1, 1, 1], sims(I(beta))[1, 1])
  expect_identical(sims_jags[[1]][4, 200, 3], sims(I(beta))[600, 4])
  expect_identical(sims_jags[[2]][1, 1, 1], sims(I(sigma))[1, 1])
  expect_identical(sims_jags[[2]][1, 200, 3], sims(I(sigma))[600])

  sims_jags_single_param <- rjags::jags.samples(jags_model_single_param, "beta", n.iter = 200)
  sims_single_param <- gsim(sims_jags_single_param)
  expect_identical(sims_jags_single_param[[1]][1, 1, 1], sims_single_param(I(beta))[1, 1])
  expect_identical(sims_jags_single_param[[1]][4, 200, 3], sims_single_param(I(beta))[600, 4])

  sims_jags_single_chain <- rjags::jags.samples(jags_model_single_chain, "beta", n.iter = 200)
  sims_single_chain <- gsim(sims_jags_single_chain)
  expect_identical(first(sims_jags_single_chain)[1, 1, 1], sims_single_chain(I(beta))[1, 1])
  expect_identical(first(sims_jags_single_chain)[4, 200, 1], sims_single_chain(I(beta))[200, 4])


  sims_coda <- rjags::coda.samples(jags_model, c("beta", "sigma"), n.iter = 200)
  sims2 <- gsim(sims_coda)
  expect_identical_output(sims_coda[[1]][1, 1], sims2(I(beta))[1, 1])
  expect_identical_output(sims_coda[[3]][200, 4], sims2(I(beta))[600, 4])
  expect_identical_output(sims_coda[[1]][1, 5], sims2(I(sigma))[1])
  expect_identical_output(sims_coda[[3]][200, 5], sims2(I(sigma))[600])

  sims_coda_single_chain <- rjags::coda.samples(jags_model_single_chain, c("beta", "sigma"), n.iter = 200)
  sims2_single_chain <- gsim(sims_coda_single_chain)
  expect_identical_output(sims_coda_single_chain[[1]][1, 1], sims2_single_chain(I(beta))[1, 1])
  expect_identical_output(sims_coda_single_chain[[1]][200, 4], sims2_single_chain(I(beta))[200, 4])
  expect_identical_output(sims_coda_single_chain[[1]][1, 5], sims2_single_chain(I(sigma))[1])
  expect_identical_output(sims_coda_single_chain[[1]][200, 5], sims2_single_chain(I(sigma))[200])


  temp_file_array <- tempfile("jags-test-array", fileext = ".jag")
  cat("
    model {
      for (n in 1:N) {
        y_hat[n] <- B[county[n], 1] + B[county[n], 2] * x[n]
        y[n] ~ dnorm(y_hat[n], tau_y)
      }

      tau_y <- pow(sigma_y, -2)
      sigma_y ~ dunif(0, 100)

      for (j in 1:85){
        B[j, 1:2] ~ dmnorm(B_hat[j, ], Tau_B)
        B_hat[j, 1] <- alpha_0 + alpha_1 * u[j]
        B_hat[j, 2] <- beta_0 + beta_1 * u[j]
      }

      alpha_0 ~ dnorm(0, 1e-4)
      alpha_1 ~ dnorm(0, 1e-4)
      beta_0 ~ dnorm(0, 1e-4)
      beta_1 ~ dnorm(0, 1e-4)
      sigma_a ~ dunif(0, 100)
      sigma_b ~ dunif(0, 100)
      rho ~ dunif(-1, 1)

      Tau_B[1:2, 1:2] <- inverse(Sigma_B[, ])
      Sigma_B[1, 1] <- pow(sigma_a, 2)
      Sigma_B[2, 2] <- pow(sigma_b, 2)
      Sigma_B[1, 2] <- rho * sigma_a * sigma_b
      Sigma_B[2, 1] <- Sigma_B[1, 2]
    }
  ", file = temp_file_array)

  jags_model_array <- rjags::jags.model(temp_file_array, data = jags_radon_data, n.chains = 3)
  rjags:::update.jags(jags_model_array, n.iter = 200)
  
  sims_jags_array <- rjags::jags.samples(jags_model_array, c("B", "Sigma_B", "sigma_b"), n.iter = 200)
  sims_array <- gsim(sims_jags_array)
  expect_identical(sims_jags_array[[1]][, , 1, 1], sims_array(I(B))[1, , ])
  expect_identical(sims_jags_array[[1]][, , 200, 3], sims_array(I(B))[600, , ])
  expect_identical(sims_jags_array[[3]][, 1, 1], sims_array(I(sigma_b))[1, ])
  expect_identical(sims_jags_array[[3]][, 200, 3], sims_array(I(sigma_b))[600, ])

  sims_coda_array <- rjags::coda.samples(jags_model_array, c("B", "Sigma_B", "sigma_b"), n.iter = 200)
  sims_array2 <- gsim(sims_coda_array)
  expect_identical_output(sims_coda_array[[1]][1, 1], sims_array2(I(B))[1, 1, 1])
  expect_identical_output(sims_coda_array[[3]][200, 1], sims_array2(I(B))[600, 1, 1])
  expect_identical_output(sims_coda_array[[1]][1, 170], sims_array2(I(B))[1, 85, 2])
  expect_identical_output(sims_coda_array[[2]][1, 175], sims_array2(I(sigma_b))[201])
})
