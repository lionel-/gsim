
context("summarise()")

test_that("summaries are accurate", {
  summaries_data_major <- radon_sims %>%
    summarise(
      scalar = mean(sigma) - sd(sigma),
      vector = mean(sigma_Beta) - sd(sigma_Beta),
      matrix = mean(Beta) - sd(Beta)
    )
  summaries_sims_major <- radon_sims %>%
    to_sims_major() %>%
    summarise(
      scalar = mean(sigma) - sd(sigma),
      vector = mean(sigma_Beta) - sd(sigma_Beta),
      matrix = mean(Beta) - sd(Beta)
    )

  scalar <- mean(radon_sims$sigma) - sd(radon_sims$sigma)
  vector <-
    apply(radon_sims$sigma_Beta, 2, mean) -
    apply(radon_sims$sigma_Beta, 2, sd)
  matrix <-
    apply(radon_sims$Beta, 2:3, mean) -
    apply(radon_sims$Beta, 2:3, sd)

  expect_equal(summaries_sims_major$scalar, scalar)
  expect_equal(summaries_data_major$scalar, scalar)
  expect_equal(c(summaries_sims_major$vector), c(vector))
  expect_equal(c(summaries_data_major$vector), c(vector))
  expect_equal_array(summaries_sims_major$matrix, matrix)
  expect_equal_array(summaries_data_major$matrix, matrix)
})

test_that("operators are escaped", {
  summaries <- radon_sims %>% summarise(trans = mean(sigma^2 + 3))
  trans <- mean(radon_sims$sigma^2 + 3)
  expect_equal(summaries$trans, trans)
})

test_that("escaped functions are escaped", {
  skip("todo")
})
