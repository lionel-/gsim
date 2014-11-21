
library("gsim")
testthat::context("Tidy outputs")


test_that("Data objects are correctly tidied", {
  sims <- clone(new_sims)

  out1 <- sims(switched)
  out2 <- sims(cbind(switched, educ))
  out3 <- sims(matrix(1, 1, 1))

  expect_identical(names(out1), "switched")
  expect_identical(names(out2), c("col1", "col2"))
})


test_that("Atomics and vectors are not data.frame'd", {
  sims <- clone(new_sims)
  
  out1 <- sims(1:3)
  out2 <- sims(matrix(1, 1, 1))

  expect_identical(out1, 1:3)
  expect_identical(out2, 1)
})


test_that("Posterior objects are correctly tidied", {
  load(radon_sims_file)
  sims <- gsim(radon_sims, radon)

  out1 <- sims(beta)
  out2 <- sims(b)
  out3 <- sims(Sigma_county)

  expect_identical(names(out1), c("sim", "beta1", "beta2"))
  expect_identical(names(out2), c("sim", paste0("b", 1:85)))
  expect_identical(names(out3), c("sim", paste0("Sigma_county", 1:2)))

  expect_identical(out2$sim, 1:100)
  expect_identical(out3$sim, rep(1:100, each = 2))
})
