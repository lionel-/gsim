
# 0.5s for producing 4000 indexes for 4 variables (16e3 iterations)
pick_sim_index <- function(x, sim) {
  param_dim <- dim(x)[-1]
  param_length <- prod(param_dim)
  nsims <- dim(x)[1]

  to <- (param_length - 1) * nsims + sim
  seq(from = sim, to, by = nsims)
}


pick_sim <- function(x, sim) {
  index <- pick_sim_index(x, sim)
  param_dim <- dim(x)[-1]
  x <- x[index]
  dim(x) <- param_dim
  x
}

# Arrays are virtually always NAM(2) objects, so using primitive
# replacement functions would not result in gains anyway.
`pick_sim<-` <- function(x, sim, value) {
  if (is.empty(x))
    x <- init_posterior(value)

  index <- pick_sim_index(x, sim)
  x[index] <- value
  x
}


perm_dims <- function(x) {
  isims <- dim_length(x)
  perm <- c(isims, seq(1, isims - 1))
  .Internal(aperm(x, perm = perm, resize = TRUE))
}

# Simulations need to be the first dimension. But it is easier to add
# a new dimension to the right than to the left. So, we append to the
# right then we permute the array.
init_posterior <- function(x, nsims = NULL) {
  if (is.null(nsims))
    nsims <- nsims()

  n <- length(x)
  param_dim <- dim(x)

  if (is.null(param_dim))
    param_dim <- c(length(x), 1)

  x <- c(x, rep(NA, n * (nsims - 1)))
  dim(x) <- c(param_dim, nsims)

  structure(perm_dims(x), class = "posterior")
}

as.posterior <- function(x, nsims = NULL) {
  if (is.null(nsims))
    nsims <- nsims()

  dims <- dim(x)
  res <- rep(x, nsims)

  dim(res) <- 
    if (is.null(dims))
      c(length(x), nsims)
    else
      c(dims, nsims)

  structure(perm_dims(res), class = "posterior")
}
