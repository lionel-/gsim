

# FIXME: atomic posteriors with dim = NULL and dim = n_sims
# Is there a way to further vectorise this function? Ask on stack
summarise_sims <- function(x, fun) {
  dims <- dim(x) %||% length(x)

  nsims <- dims[1] 
  len <- prod(dims[-1])

  res <- vector("numeric", len)
  for (i in seq_len(len))
    res[i] <- fun(x[seq(i, nsims * len - (len - i), by = len)])

  class(res) <- "numeric"
  dim(res) <- dims[-1] %||% NULL
  res
}

`%%` <- summarise_sims


bernoulli_check <- function(y, p, stat, p_value = TRUE) {
  stopifnot(is.numeric(y) && is.posterior(p))
  browser(expr = getOption("debug_on"))

  stat <- substitute(stat)
  n_sims <- dim(p)[1]
  n <- length(y)

  y_rep <- array(NA, dim = c(n_sims, n))
  for (i in seq_len(n_sims)) {
    y_rep[i, ] <- rbernoulli(n, p[i, ])
  }

  tmp <- eval(stat, env = list(p = p[1, ]))
  dim_stat <- dim(tmp) %||% length(tmp)

  y_stat <- array(NA, dim = c(n_sims, dim_stat))
  for (i in seq_len(n_sims)) {
    env <- list(p = p[i, ])
    y_stat[i, ] <- eval(stat, env)
  }

  y_rep_stat <- array(NA, dim = c(n_sims, dim_stat))
  for (i in seq_len(n_sims)) {
    env <- list(
      y = y_rep[i, ],
      p = p[i, ]
    )
    y_rep_stat[i, ] <- eval(stat, env)
  }

  y_stat

  if (p_value)
    colMeans(y_stat > y_rep_stat)
  else
    list(stat = y_stat, stat_rep = posterior(y_rep_stat))
}
