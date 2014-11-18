  
# Remove all attributes except dimensions
expect_identical_output <- function(a, b) {
  expect_identical(trim_attr(a), trim_attr(b))
}

trim_attr <- function(x) {
  old_dim <- dim(x)
  x <- c(x)
  dim(x) <- old_dim
  x
}

check_packages <- function(...) {
  packages <- c(...)
  lapply(packages, function(x) {
    if (!requireNamespace(x, quietly = TRUE))
      stop(paste("The package", x, "must be installed"), call. = FALSE)
  })
  invisible(NULL)
}
