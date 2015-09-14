
context("as_predicter()")

test_that("basic predicter works", {
  skip("todo")

  test <- radon_sims %>%
    as_predicter(function(x, y) {
      x * sigma  +  y * sigma
    })

  test(2, 3) %>% str()

})
