#' Colwise and rowwise operations
#'
#' \code{cols_means()} and \code{rows_means()} are simple aliases to
#' \code{colMeans()} and \code{rowMeans()}. \code{colVars()} uses
#' \code{colMeans()} to provide an efficient way of computing the
#' variances of the columns of a matrix while \code{rowVars()} uses
#' \code{rowMeans()} to provide an efficient way of computing the
#' variances of the rows of a matrix.
#'
#' @param x numeric matrix.
#' @name colwise-rowise
NULL

#' @rdname colwise-rowise
#' @export
cols_variances <- function(x) {
  class(x) <- "matrix"  # Circumvents a bug
  n <- nrow(x)
  c <- ncol(x)
  x <- ((x - matrix(.colMeans(x, n, c), nrow = n, ncol = c, byrow = TRUE))^2)
  .colMeans(x, n, c) *
    n / (n - 1)
}

#' @rdname colwise-rowise
#' @export
rows_variances <- function(x) {
  class(x) <- "matrix"  # Circumvents a bug
  n <- nrow(x)
  c <- ncol(x)
  x <- ((x - matrix(.rowMeans(x, n, c), nrow = n, ncol = c, byrow = TRUE))^2)
  .rowMeans(x, n, c) *
    n / (n - 1)
}

#' @rdname colwise-rowise
#' @export
cols_means <- colMeans

#' @rdname colwise-rowise
#' @export
rows_means <- rowMeans


#' Make row vector
#'
#' Wrappers around \code{rbind()} and \code{cbind()} to produce a row
#' or column vector. They accept only one argument and are a clearer
#' semantic signal regarding the nature of the vector.
#'
#' @param x numeric vector.
#' @return Array of dimension 1 x n or n x 1
#' @name vector
NULL

#' @rdname vector
#' @export
row_vector <- function(x) {
  rbind(c(x))
}

#' @rdname vector
#' @export
col_vector <- function(x) {
  cbind(c(x))
}


#' Stan compatibility
#'
#' Stan functions that do not exist in R.
#' @name stan-compat
NULL

#' @rdname stan-compat
#' @param matrix A matrix.
#' @param vector A vector.
#' @export
quad_form_diag <- function(matrix, vector) {
  diag <- diag(vector)
  diag %*% matrix %*% diag
}
