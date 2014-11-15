
library("gsim")
testthat::context("Creation and manipulation of containers")

test_that("Cloning a container makes them independent", {
  sims <- clone(new_sims)
  sims(new_var <- 1)

  sims2 <- clone(new_sims)
  expect_error(sims2(new_var), "object.*not found")

  sims2(I(beta))
  new_sims(I(beta))
})


test_that("Call stack gets cleaned on error", {
  sims <- clone(new_sims)
  expect_error(sims(void_var))

  out <- try(sims(I(beta)), silent = TRUE)
  expect_is(out, "matrix")
})


test_that("Listed output protects and names elements", {
  sims <- clone(new_sims)

  new_sims <- gsim(arm_sims, wells); sims <- clone(new_sims)
  out <- sims(list(
    I(beta),
    dup = P(beta),
    sigma,
    named = cbind(switched, c_dist100)
  ))

  expect_identical(names(out), c("beta", "dup", "sigma", "named"))
  expect_is(out[[1]], "matrix")
  expect_is(out[[2]], "posterior")
  expect_is(out[[3]], "data.frame")
})


test_that("Quoted input", {
  sims <- clone(new_sims)
  in1 <- quote(P(beta))
  expect_is(sims(in1), "posterior")

  # If quoted name also exists in storage, it gets priority
  beta <- quote(switched)
  expect_is(sims(beta), "data.frame")
})


test_that("Curly input", {
  sims <- clone(new_sims)

  out1 <- sims({P(beta)})
  expect_is(out1, "posterior")

  out2 <- sims({
    x1 <- cbind(switched, c_dist100)
    I(x1)
  })
  expect_is(out2, "matrix")
})


test_that("Quoted curly input with one or several elements", {
  new_sims <- gsim(arm_sims, wells); sims <- clone(new_sims)
  
  in1 <- quote({
    x1 <- cbind(switched, c_dist100)
  })
  sims(in1)
  expect_is(sims(I(x1)), "matrix")

  sims <- clone(new_sims)
  in2 <- quote({
    x1 <- cbind(switched, c_dist100)
    x2 <- cbind(x1, x1)
  })
  sims(in2)
  expect_equal(ncol(sims(P(x2))), 4)
})


test_that("Several curlies as input", {
  sims <- clone(new_sims)

  in1 <- quote({
    x1 <- cbind(switched, c_dist100)
  })
  in2 <- quote({
    x2 <- beta
  })

  sims(in1, in2)
  out1 <- sims(I(x1))
  out2 <- sims(I(x2))

  expect_identical(out1, wells %$% cbind(switched, c_dist100))
  expect_identical_output(out2, arm_sims@coef)
})


test_that("Omnibus", {
  sims <- clone(new_sims)
  
  in1 <- quote(x1 <- educ)
  in2 <- quote({
    x2 <- beta
  })
  in3 <- quote(switched)

  out <- sims(in1, in2, in3, x4 <- switched, {x5 <- c_dist100}, beta)
  out1 <- sims(I(x1))
  out2 <- sims(I(x2))
  out4 <- sims(I(x4))
  out5 <- sims(I(x5))

  expect_is(out, "data.frame")         # and not base::beta
  expect_identical(out1, wells$educ)
  expect_identical_output(out2, arm_sims@coef)
  expect_identical(out4, wells$switched)
  expect_identical(out5, wells$c_dist100)
})

