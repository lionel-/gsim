
posterior <- function(object = NULL) {
  structure(object, class = "posterior")
}

is.posterior  <- function(x) {
  inherits(x, "posterior")
}

as.posterior <- function(x, nsims = NULL) {
  if (is.null(nsims))
    nsims <- context("nsims")

  dims <- dim(x)
  res <- rep(x, nsims)

  dim(res) <- 
    if (is.null(dims))
      c(length(x), nsims)
    else
      c(dims, nsims)

  structure(perm_dims(res), class = "posterior")
}

# Simulations' array dimension need to be the first one. But it is
# easier to add a new dimension to the right than to the left. So, we
# append to the right then we permute the array.
init_posterior <- function(x, nsims = NULL) {
  if (is.posterior(x))
    return(x)

  if (is.null(nsims))
    nsims <- context("nsims")

  old_class <- setdiff(class(x), "matrix")
  n <- length(x)
  dim <- dim(x)

  if (is.null(dim))
    dim <- length(x)
  else if (length(dim) == 1 && dim == 1)
    dim <- c(1, 1)
  else if (last(dim) == 1)
    dim <- dim[-length(dim)]

  x <- c(x, rep(NA, n * (nsims - 1)))
  dim(x) <- c(dim, nsims)
  x <- perm_dims(x)

  structure(x, class = c("posterior", old_class))
}


# 0.5s for producing 4000 indexes for 4 variables (16e3 iterations)
pick_sim_index <- function(x, sim) {
  dims <- dim(x)
  if (is.null(dims) || length(dims) == 1)
    sim
  else {
    param_dim <- dims[-1]
    param_length <- prod(param_dim)
    nsims <- dims[1]

    to <- (param_length - 1) * nsims + sim
    seq(from = sim, to, by = nsims)
  }
}


pick_sim <- function(x, sim) {
  index <- pick_sim_index(x, sim)
  param_dim <- dim(x)[-1] %||% 1
  x <- unclass(x)[index]
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


# unary_rbind a little trickier? because the additional single-element
# dimension is internal
unary_cbind <- function(x) {
  old <- dim(x)
  dim(x) <- c(old[1], prod(old[-1]), 1)
  x
}


`[.posterior` <- function(x, i, ...) {
  x <- NextMethod(drop = FALSE)

  # Drop first dimension, keeping the others intact
  if (length(i) == 1)
    dim(x) <- dim(x)[-1] %||% 1
  else
    class(x) <- "posterior"

  x
}

as.data.frame.posterior <- function(x, ...) {
  if (!is.null(dim(x)))
    class(x) <- "array"
  else
    class(x) <- "numeric"
  x <- reshape2::melt(x)
  x
}

# Convenience functions for debugging
print.posterior <- function(x) {
  i <- sample(dim(x)[1], 1)
  x <- pick_sim(x, i)
  NextMethod()
}

head.posterior <- function(x, n = 6) {
  sims_n <- dim(x)[1] %||% length(x)
  i <- sample(sims_n, 1)
  x <- pick_sim(x, i)
  NextMethod()
}

tail.posterior <- function(x, n = 6) {
  i <- sample(dim(x)[1], 1)
  x <- pick_sim(x, i)
  NextMethod()
}
