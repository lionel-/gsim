
#' @export
intercept <- function(n = NULL) {
  if (is.null(n))
    n <- get_n()

  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol 
  stopifnot(is.wholenumber(n))
  
  res <- rep(1, n)

  if (class(try(get_nsims(), silent = TRUE)) == "try-error")
    res
  else
    gs(res, "data", colnames = "intercept")
}


#' @export
ones <- function(x, preds = list(NULL)) {
  n <- length(unique(x))
  nrow <- length(x)

  ones <- vector(length =  nrow * n)
  for (i in seq_len(n)) {
    from <- 1 + (i-1) * nrow
    to <- nrow + (i-1) * nrow
    ones[seq(from, to)] <- (x == i) + 0
  }
  ones <- matrix(ones, nrow, n)

  res <- lapply(preds, function(x) {
    if (is.null(x))
      ones
    else {
      temp <- matrix(NA, nrow, n)
      for (j in seq_len(ncol(ones)))
        temp[, j] <- ones[, j] * x
      temp
    }
  })
  
   do.call(cbind, res)
}

ones_ <- function(x, times = 1) {
  x <- get_in_input(deparse(substitute(x)))
  ones(x, times = times) %>% gs("data")
}


#' @export
get_omega <- function(resid, sigma) {
  omega <- (sd(resid) / mean(sigma))^2
  pmin(omega, 1)
}


#' @export
row_vector <- function(x) {
  rbind(c(x))
}


#' @export
col_vector <- function(x) {
  cbind(c(x))
}
