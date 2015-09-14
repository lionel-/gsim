# Perform a posterior predictive check
#
# \code{check_model()} computes a statistic or a discrepancy on both
# the outcome variables and its replications under a model.
#
# @param .y The actual outcome variable. Either a numeric
#   vector/array, or a name or formula. In the latter cases, the
#   variable will be retrieved from \code{.context} or from the
#   calling environment, in that order.
# @param .y_rep Replications of \code{.y} under a model. Either a
#   simulations array, or a name or formula. In the latter cases, the
#   array will be retrieved from \code{.s} or from the calling
#   environment, in that order.
# @param .statistic A statistic or discrepancy function to be applied
#   to \code{.y} and \code{.y_rep}. This function can depend on other
#   variables as well as parameters.
# @param ... Further arguments passed on to \code{.statistic}.
# @inheritParams by_sim
# @export
check_model <- function(.s = list(), .y, .y_rep, .statistic, ...,
                        .context = list(), .bindings = NULL) {
  if (!is.numeric(.y)) {
    if (is_formula(.y)) {
      .y <- process_formula(.y)
    }
    if (!is.character(.y)) {
      stop(".y should be a numeric vector, a formula, or a string",
        call. = FALSE)
    }
    .y <- .context[[.y]]
  }

  if (is_sims_array(.y_rep)) {
    .s$y_rep <- .y_rep
  } else {
    if (is_formula(.y_rep)) {
      .y_rep <- process_formula(.y_rep)
    }
    if (!(is.character(.y_rep) && length(.y_rep) == 1)) {
      stop(".y_rep should be a simulations array, a formula, or a string",
        call. = FALSE)
    }

    .bindings = c(.bindings, y_rep = .y_rep)
  }

  # Add parameters to .statistic's signature so that it can be used
  # with by_sim()
  if (!is.function(.statistic)) {
    stop(".statistic must be a function", call. = FALSE)
  }
  f <- adjust_signature(.statistic, .s)

  # Compute statistics and p-value
  y_stat <- by_sim(.s, .statistic, .y, .context = .context, .bindings = .bindings)
  y_rep_stat <- by_sim(.s, .statistic, .y_rep, .context = .context, .bindings = .bindings)

  out <- list(stat = y_stat, stat_rep = y_rep_stat)
  attr(out, "p-value") <- mean(y_stat > y_rep_stat)
  out
}

process_formula <- function(formula) {
  if (length(formula) != 2) {
    stop("formula should only have a right hand side", call. = FALSE)
  }
  as.character(formula[2])
}

#' @export
p <- function(check) attr(check, "p-value")
