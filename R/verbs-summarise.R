#' Summarise posterior distributions
#'
#' \code{summarise()} can be used to apply a function
#' simulationwise. This is especially useful to create posterior
#' summaries of your paramaters.
#'
#' gsim's \code{summarise()} method is different from the one in dplyr
#' in several respects. While dplyr's method discards non-relevant
#' columns, gsim's method keeps all data. \code{sims} objects can
#' contain both simulation arrays and ordinary
#' objects. \code{summarise()} makes use of this possibility by adding
#' the summarised quantities alongside the posterior arrays.
#'
#' Note that the functions mentioned in \code{...} are applied across
#' the posterior simulations. If you need to transform your quantities
#' datawise, mention the relevant functions in \code{.escaped} (though
#' it is generally better to transform beforehand with
#' \code{\link{mutate}()}). All binary operators such as \code{+} or
#' \code{^} are escaped by default because it doesn't make sense to
#' apply them across simulations.
#' @param .escaped Names of functions that should be escaped in
#'   addition to the binary operators.
#' @inheritParams sims-verbs
#' @importFrom dplyr summarise_
#' @seealso \code{\link{across_sims}()}
#' @name summarise
#' @export
#' @examples
#' library("dplyr")
#'
#' radon_sims %>%
#'   summarise(
#'     sigma_lower = mean(sigma) - sd(sigma),
#'     sigma_m = mean(sigma),
#'     sigma_upper = sigma_m + sd(sigma),
#'     sigma_975 = q975(sigma),
#'     sigma_025 = q025(sigma)
#'   )
#'
#' radon_sims %>% summarise(sigma_est = mean_sd(Beta))
summarise_.sims <- function(.data, ..., .dots, .context = list(),
                            .escaped = NULL) {
  calling_env <- parent.frame()
  .dots <- lazyeval::auto_name(.dots)

  # Pull out .context argument from dots
  is_context <- ".context" == names(.dots)
  if (any(is_context)) {
    .context <- c(.context, lazyeval::lazy_eval(.dots$.context))
    .dots <- .dots[!is_context]
  }

  # Create calls to across_sims()
  out <- .data
  for (param in names(.dots)) {
    eval_env <- list2env(c(out, .data, .context), calling_env)
    expr <- across_sims_call(.dots[[param]][["expr"]],
      eval_env, .escaped)
    out[[param]] <- eval(expr, eval_env)
  }

  out
}

#' @export
#' @rdname summarise
summarise_.stanfit <- function(.data, ..., .dots, .context = list()) {
  summarise_(as_sims(.data), ..., .dots = .dots, .context = .context)
}

across_sims_call <- function(expr, env, escaped) {
  if (acts_on_sims(expr, env, escaped)) {
    fun <- expr[[1]]
    first <- expr[[2]]
    remaining <- as.list(expr[-c(1, 2)])
    expr_elems <- splice("across_sims", first, fun, remaining)
    do.call(call, expr_elems, TRUE)
  } else if (is.call(expr)) {
    expr[-1] <- map(expr[-1], across_sims_call, env, escaped)
    expr
  } else {
    expr
  }
}

acts_on_sims <- function(expr, env, escaped) {
  if (!is.call(expr) || length(expr) < 2) {
    return(FALSE)
  }

  arg <- expr[[2]]
  if (should_escape(arg, escaped)) {
    acts_on_sims(arg, env, escaped)
  } else if (is.name(arg)) {
    obj <- eval(arg, env)
    is_sims_array(obj)
  } else {
    FALSE
  }
}

should_escape <- function(expr, escaped) {
  is.call(expr) || return(FALSE)
  call_fun(expr) %in% c(escaped, `_escaped`)
}

`_escaped` <- c("+", "-", "*", "/", "^")
