
context("model checking")

my_stat <- function(x) {
  max(x) + mu_y * sigma
}

test_that("check is accurate", {
  skip("todo")

  expect_error(
    check_model(radon_sims, ~radon, 1:2, my_stat, .context = radon),
    "y_rep should be"
  )

  radon_sims %>% check_model("y", "y_rep", my_stat, .context = radon)

  radon_sims %>% check_model("y", "y_rep", max, .context = radon)
  radon_sims %>% check_model(~y, ~y_rep, max, .context = radon)

})


