
# Wells simulations used in tests

wells <- wells_data() %>%
  define(
    y = switched,
    c_dist100,
    c_arsenic,
    N = nrow(.)
  )

stan_wells_m <- system.file("data-raw", "wells.stan", package = "gsim")
wells_stanfit <- rstan::stan(stan_wells_m, data = stan_wells_data,
  iter = 100, chains = 2)


wells_sims <- rstan::extract(wells_stanfit, c("y_hat", "beta"))

save(wells_sims, file = "./tests/testthat/wells-sims.rda")
