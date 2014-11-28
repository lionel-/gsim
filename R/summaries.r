

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


#' @export
check_model <- function(y, y_rep, params, stat) {
  lazy <- lazyeval::lazy(stat)
  stat <- lazy$expr

  enclos <- new.env(parent = lazy$env)
  enclos$y <- y
  
  n_sims <- dim(first(params))[1]
  n <- length(y)
  evaled_stat <- eval(stat, env = pick_sims(params, 1))
  dim_stat <- dim(evaled_stat) %||% length(evaled_stat)

  y_stat <- array(NA, dim = c(n_sims, dim_stat))
  for (i in seq_len(n_sims)) {
    env <- pick_sims(params, i)
    y_stat[i, ] <- eval(stat, env, enclos)
  }

  y_rep_stat <- array(NA, dim = c(n_sims, dim_stat))
  for (i in seq_len(n_sims)) {
    env <- pick_sims(params, i)
    env$y <- y_rep[i, ]
    y_rep_stat[i, ] <- eval(stat, env, enclos)
  }

  out <- list(stat = y_stat, stat_rep = y_rep_stat)
  attr(out, "p-value") <- mean(y_stat > y_rep_stat)
  out
}

#' @export
check_normal <- function(y, mu, sigma, params = NULL, stat) {
  stopifnot(is.numeric(y) && is.array(mu) && is.array(sigma))

  if (is.null(params))
    params <- list(mu = mu, sigma = sigma)

  n_sims <- dim(mu)[1]
  n <- length(y)

  y_rep <- array(NA, dim = c(n_sims, n))
  for (i in seq_len(n_sims)) {
    y_rep[i, ] <- rnorm(n, mu[i, ], sigma[i])
  }

  check_model(y, y_rep, params, stat)
}

#' @export
check_bernoulli <- function(y, p, params = NULL, stat) {
  stopifnot(is.numeric(y) && is.array(p))

  if (is.null(params))
    params <- list(p = p)

  n_sims <- dim(p)[1]
  n <- length(y)

  y_rep <- array(NA, dim = c(n_sims, n))
  for (i in seq_len(n_sims)) {
    y_rep[i, ] <- rbernoulli(n, p[i, ])
  }

  check_model(y, y_rep, params, stat)
}


#' @export
p <- function(check) attr(check, "p-value")
