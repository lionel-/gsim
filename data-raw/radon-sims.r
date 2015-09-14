
library("gsim")
import::from("dplyr", n_distinct)
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
radon_stanfit <- rstan::stan(model_code = radon_m, data = stan_radon_data,
  iter = 100, chains = 2)

radon_sims <- radon_stanfit %>%
  select(mu_y, y_rep, Beta, sigma, Gamma, sigma_Beta, Omega)

saveRDS(radon_sims, file = "./tests/testthat/radon-sims.rds")
