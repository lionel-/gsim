

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


# TODO: Abstract the checking logic in a function factory

#' @export
bernoulli_check <- function(y, p, stat) {
  stopifnot(is.numeric(y) && is.array(p))

  # Need lazyeval so that it works when called inside gsim
  lazy <- lazyeval::lazy(stat)
  enclos <- new.env(parent = lazy$env)
  enclos$y <- y
  stat <- lazy$expr

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
    y_stat[i, ] <- eval(stat, env, enclos)
  }

  y_rep_stat <- array(NA, dim = c(n_sims, dim_stat))
  for (i in seq_len(n_sims)) {
    env <- list(
      y = y_rep[i, ],
      p = p[i, ]
    )
    y_rep_stat[i, ] <- eval(stat, env, enclos)
  }

  out <- list(stat = y_stat, stat_rep = y_rep_stat)
  attr(out, "p-value") <- colMeans(y_stat > y_rep_stat)
  out
}


#' @export
normal_check <- function(y, mu, sigma, stat) {
  stopifnot(is.numeric(y) && is.array(mu) && is.array(sigma))

  # Need lazyeval so that it works when called inside gsim
  lazy <- lazyeval::lazy(stat)
  enclos <- new.env(parent = lazy$env)
  enclos$y <- y
  stat <- lazy$expr

  n_sims <- dim(mu)[1]
  n <- length(y)

  y_rep <- array(NA, dim = c(n_sims, n))
  for (i in seq_len(n_sims)) {
    y_rep[i, ] <- rnorm(n, mu[i, ], sigma[i])
  }

  tmp <- eval(stat, env = list(mu = mu[1, ], sigma = sigma[i]))
  dim_stat <- dim(tmp) %||% length(tmp)

  y_stat <- array(NA, dim = c(n_sims, dim_stat))
  for (i in seq_len(n_sims)) {
    env <- list(mu = mu[i, ], sigma = sigma[i])
    y_stat[i, ] <- eval(stat, env, enclos)
  }

  y_rep_stat <- array(NA, dim = c(n_sims, dim_stat))
  for (i in seq_len(n_sims)) {
    env <- list(
      y = y_rep[i, ],
      mu = mu[i, ],
      sigma = sigma[i]
    )
    y_rep_stat[i, ] <- eval(stat, env, enclos)
  }

  out <- list(stat = y_stat, stat_rep = y_rep_stat)
  attr(out, "p-value") <- colMeans(y_stat > y_rep_stat)
  out
}


#' @export
p <- function(check) attr(check, "p_value")
