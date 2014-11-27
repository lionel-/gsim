
# Radon simulations used in tests

radon <- fetch_radon_data()
stan_radon_data <- c(
  radon %>% dplyr::select(y, x, county),
  radon %>%
    dplyr::group_by(county) %>%
    dplyr::distinct(u) %>%
    dplyr::ungroup() %>%
    dplyr::select(u),
  list(
    N = nrow(radon),
    J = dplyr::n_distinct(radon$county)
  )
)

stan_radon_m <- system.file("data-raw", "radon.stan", package = "gsim")
radon_stanfit <- rstan::stan(stan_radon_m, data = stan_radon_data,
  iter = 100, chains = 2)

radon_sims <- rstan::extract(radon_stanfit,
  c("y_hat", "y_rep", "a", "b", "alpha", "beta", "Sigma_county", "rho", "sigma_y")
)

save(radon_sims, file = "./tests/testthat/radon-sims.rda")
