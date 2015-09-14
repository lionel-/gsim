
# Remove all attributes except dimensions
expect_equal_array <- function(a, b) {
  expect_identical(trim_attr(a), trim_attr(b))
}

trim_attr <- function(x) {
  old_dim <- dim(x)
  names(old_dim) <- NULL
  attributes(x) <- NULL
  dim(x) <- old_dim
  x
}
