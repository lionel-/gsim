
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
  sims2 <- clone(new_sims)

  out0 <- sims2(sigma)
  expect_identical(names(out0), c("sim", "sigma"))

  out1 <- sims(beta)
  expect_identical(names(out1), c("sim", "beta", "value"))
  expect_identical(levels(out1$beta), c("beta1", "beta2"))

  out1b <- sims2(beta)
  expect_identical(names(out1b), c("sim", "beta", "value"))
  expect_identical(levels(out1b$beta),
    c("intercept", "c_dist100", "c_arsenic", "c_dist100:c_arsenic"))

  out2 <- sims(b)
  expect_identical(names(out2), c("sim", "b", "value"))
  expect_identical(levels(out2$b), paste0("b", 1:85))
  expect_identical(out2$sim, rep(seq_len(n_sims), 85))

  out3 <- sims(Sigma_county)
  expect_identical(names(out3), c("sim", paste0("Sigma_county", 1:2)))
  expect_identical(out3$sim, rep(1:100, each = 2))
})


test_that("Tidying listed outputs", {
  sims <- clone(new_sims)

  out1 <- sims(list(beta, sigma))
  expect_identical(names(out1), c("beta", "sigma"))
  expect_identical(names(out1$beta), c("sim", "beta", "value"))

  out2 <- sims(list(beta * 2, hop = sigma))
  expect_identical(names(out2), c("beta...2", "hop"))
  expect_identical(names(out2$hop), c("sim", "hop"))

  out3 <- sims(list(I(beta), I(sigma)))
  expect_identical(names(out3), c("beta", "sigma"))
})


test_that("Tidying when default is array output", {
  sims <- gsim(arm_sims, wells, tidy_output = FALSE)

  out1 <- sims(list(
    ## beta,
    dup = T(beta)
  ))

  # test list with one named element

  out2 <- sims(T(sigma))

  expect_is(out1$beta, "matrix")
  expect_is(out1$dup, "data.frame")
  expect_is(out2, "data.frame")
})

