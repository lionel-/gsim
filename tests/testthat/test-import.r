
library("gsim")
testthat::context("Correct import of simulations")


test_that("Simulations from lm object are correctly imported", {
  wells_lm_fit <- lm(switched ~ c_dist100 * c_arsenic, wells)
  sims_arm_lm_s4 <- arm::sim(wells_lm_fit)
  sims_arm_lm <- gsim(sims_arm_lm_s4, wells)

  expect_identical_output(sims_arm_lm_s4@coef, sims_arm_lm(I(beta)))
  expect_identical_output(sims_arm_lm_s4@sigma, sims_arm_lm(I(sigma)))
})

test_that("Simulations from glm object are correctly imported", {
  wells_glm_fit <- glm(switched ~ c_dist100 * c_arsenic, binomial, wells)
  sims_arm_glm_s4 <- arm::sim(wells_glm_fit)
  sims_arm_glm <- gsim(sims_arm_glm_s4, wells)

  expect_identical_output(sims_arm_glm_s4@coef, sims_arm_glm(I(beta)))
  expect_identical_output(sims_arm_glm_s4@sigma, sims_arm_glm(I(sigma)))
})

test_that("Simulations from Stan object are correctly imported", {})
