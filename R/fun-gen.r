
gen_norm <- function(n, mean, sd) {
  ## todo: check shapes
  stopifnot(is.posterior(mean) && is.posterior(sd))
  force(n)

  do_by_sims(mean, sd, fun = rnorm, args = list(n = n))
}
