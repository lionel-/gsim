
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
get_omega <- function(resid, sigma) {
  omega <- (sd(resid) / mean(sigma))^2
  pmin(omega, 1)
}
