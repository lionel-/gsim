

#' @export
nobs.gs <- function(gs) {
  n <- attr(gs, "nobs")
  if (is.null(n))
    stop("invalid nobs attribute")
  else n
}

#' @export
nsims <- function(gs) {
  n <- attr(gs, "nsims")
  if (is.null(n))
    stop("invalid nobs attribute")
  else n
}


get_n <- function(n_up = 1) get("..n..", envir = parent.frame(n_up + 1))
get_nsims <- function(n_up = 1) get("..nsims..", envir = parent.frame(n_up + 1))


sample_nsims <- function(data, nsims, n = 5) {
  extract(data, , sample(seq(nsims), n))
}
