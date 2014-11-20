
library("gsim")
testthat::context("Reactive outputs")

y <- 3
loop_fitted <- array(dim = c(n_sims, 10))
for (x in 1:10) {
  X_loop <- cbind(1, x, y, x * y)
  for (i in 1:n_sims) {
    loop_fitted[i, x] <- arm::invlogit(X_loop %*% cbind(arm_sims@coef[i, ]))
  }
}

sims <- clone(new_sims)
  sims <- gsim(arm_sims, wells)
fun <- sims({
  x <- x()
  y <- y()
  X <- cbind(1, x, y, x * y)
  arm::invlogit(X %*% beta)
})


test_that("Multiple inputs / omnibus test", {
  set.seed(100)
  out1 <- fun(2, 3)
  out2 <- fun(2, 3, out = 50)
  out3 <- fun(2, 3, out = mean)

  set.seed(100)
  i <- sample(seq_len(n_sims), 1)
  loop_out1 <- loop_fitted[i, 2]
  loop_out2 <- loop_fitted[50, 2]
  loop_out3 <- mean(loop_fitted[, 2])

  expect_identical(loop_out1, out1)
  expect_identical(loop_out2, out2)
  expect_identical(loop_out3, out3)
})


test_that("Multiple inputs vectorized", {
  out <- fun(1:10, 3, out = 50)
  loop_out <- loop_fitted[50, ]

  expect_identical(loop_out, out)
  expect_error(fun(1:100, 3:4),
    "Reactive functions accept at most one vector")
})


test_that("Single input", {
  sims_single <- clone(new_sims)
  fun_single <- sims_single({
    x <- x()
    X <- cbind(1, x, 3, x * 3)
    arm::invlogit(X %*% beta)
  })

  out <- fun_single(10, out = 30)
  loop_out <- loop_fitted[30, 10]
  expect_identical(loop_out, out)
})


test_that("Inputs directly in arguments", {
  sims_direct <- clone(new_sims)
  fun_direct <- sims_direct({
    X <- cbind(1, x(), 3, x() * 3)
    arm::invlogit(X %*% beta)
  })

  out <- fun_direct(5, out = 25)
  loop_out <- loop_fitted[25, 5]
  expect_identical(loop_out, out)
})


test_that("Custom inputs", {
  sims_custom <- clone(new_sims)
  fun_custom <- sims_custom({
    my <- input("in")
    X <- cbind(1, my, 3, my * 3)
    arm::invlogit(X %*% beta)
  })

  out <- fun_custom(2, out = 15)
  loop_out <- loop_fitted[15, 2]
  expect_identical(loop_out, out)
})


test_that("Locked lhs are freed when a reactive function is created", {
  sims <- clone(new_sims)
  fun <- sims({
    x <- x()
    y <- y()
    X <- cbind(1, x, y, x * y)
    arm::invlogit(X %*% beta)
  })

  expect_true({sims(x <- 3, X <- 5); TRUE})
})
