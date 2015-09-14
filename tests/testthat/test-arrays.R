
context("sims_array objects")

array_vec <- radon_sims[["mu_y"]]
array_mat <- radon_sims[["Beta"]]

test_that("dimensions are retrieved", {
  expect_equal(n_sims(array_vec), 100)

  expect_equal(data_dims(array_vec), 919)
  expect_equal(data_dims(array_mat), c(85, 2))
  sims_margin(array_vec)
})

test_that("arrays are permuted", {
  skip("todo")
  to_sims_major()
})
