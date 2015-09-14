#' Compute summaries a quantity
#'
#' These tools are especially useful in conjunction with
#' \code{\link{summarise}()}. \code{mean_sd()} returns a list with
#' components \code{mean} and \code{sd}.
#'
#' @param x A quantity to summarise.
#' @param rm_na Whether to remove missing values.
#' @name summaries
#' @examples
#' radon_sims %>%
#'   summarise(
#'     mu_y = list(q025(mu_y), q975(mu_y)),
#'     Beta = mean_sd(Beta)
#'   )
NULL

#' @rdname summaries
#' @export
mean_sd <- function(x, rm_na = TRUE) {
  list(
    mean = mean(x, na.rm = na.rm),
    sd = sd(x, na.rm = na.rm)
  )
}

#' @rdname summaries
#' @export
q025 <- function(x) {
  quantile(x, 0.025)
}

#' @rdname summaries
#' @export
q975 <- function(x) {
  quantile(x, 0.975)
}


#' @export
get_omega <- function(resid, sigma) {
  omega <- (sd(resid) / mean(sigma))^2
  pmin(omega, 1)
}


#' @export
logit <- function(x) {
  log(x/(1 - x))
}

#' @export
inv_logit <- function(x) {
  1 / (1 + exp(-x))
}


### Aliases

#' Bernoulli distribution wrappers
#'
#' Wrappers for dbinom, pbinom, qbinom and rbinom with \code{size} set
#' to 1.
#' @inheritParams stats::dbinom
#' @name Bernoulli
NULL

#' @rdname Bernoulli
#' @export
dbernoulli <- function(x, prob, log = FALSE) {
  dbinom(x, size = 1, prob, log)
}

#' @rdname Bernoulli
#' @export
pbernoulli <- function(q, prob, lower.tail = TRUE, log.p = FALSE) {
  pbinom(q, size = 1, prob, lower.tail, log.p)
}

#' @rdname Bernoulli
#' @export
qbernoulli <- function(p, prob, lower.tail = TRUE, log.p = FALSE) {
  qbinom(p, size = 1, prob, lower.tail, log.p)
}

#' @rdname Bernoulli
#' @export
rbernoulli <- function(n, prob) {
  rbinom(n, size = 1, prob)
}
