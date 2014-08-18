
gen_norm <- function(n, mean, sd) {
  stopifnot(is.gsresult(mean) && is.gsparam(sd))

  nobs <- get_n()
  nsims <- get_nsims()

  res <- matrix(NA, nobs, nsims)
  for (i in seq_len(nobs)) {
    res[i, ] <- rnorm(nsims, mean[i, ], sd)
  }

  gs(res, "gsresult")
}
