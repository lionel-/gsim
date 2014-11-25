
skip_heavy_computations <- TRUE

wells <- fetch_wells_data()
wells_fit <- wells %$% glm(switched ~ c_dist100 * c_arsenic, binomial)
X <- wells %$% cbind(intercept(), c_dist100, c_arsenic, c_dist100 * c_arsenic)

n_sims <- 100
arm_sims <- arm::sim(wells_fit, n.sims = n_sims)
new_sims <- gsim(arm_sims, wells)

# See data-raw/radon-sims.r for generation of radon-sims.rda
radon <- fetch_radon_data()
radon_sims_file <- system.file("tests", "testthat", "radon-sims.rda",
  package = "gsim")
