
# Data for tests

radon <- fetch_radon_data()
stan_radon_data <- c(
  radon %>% select(y, x, county),
  radon %>% group_by(county) %>% distinct(u) %>% ungroup %>% select(u),
  list(
    N = nrow(radon),
    J = n_distinct(radon$county)
  )
)

stan_radon_m <- system.file("tests", "testthat", "radon.stan",
  package = "gsim")
radon_stanfit <- rstan::stan(stan_radon_m, data = stan_radon_data,
  iter = 100, chains = 2)

radon_sims <- rstan::extract(radon_stanfit,
  c("a", "b", "alpha", "beta", "Sigma_county", "rho", "sigma_y"))

## save(radon_sims, file = "./tests/testthat/radon-sims.rda")
