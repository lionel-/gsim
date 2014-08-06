
rnorm <- function(n, mean, sd) {
  stopifnot(is.gsresult(mean) && is.gsparam(sd))

  res <- matrix(NA, get_n(), get_nsims())
  for (i in seq(n)) {
    res[i, ] <- stats::rnorm(1, mean[i, ], sd)
  }

  gs(res, "gsresult")
}
