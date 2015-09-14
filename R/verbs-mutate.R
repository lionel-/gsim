#' Verbs for manipulating posterior simulations
#'
#' These functions provide dplyr-like methods for manipulating lists
#' of posterior simulations (\code{sims} object). See
#' \code{\link{mutate}()}, \code{\link{summarise}()} and
#' \code{\link{select}()} .
#' @param .data A list of simulations arrays.
#' @param ... Name-value pairs of expressions.
#' @param .dots Used to work around non-standard evaluation. See
#'   dplyr's \code{vignette("nse")} for details.
#' @param .context Additional data on which expressions depend.
#' @return A list of simulations arrays.
#' @name sims-verbs
NULL


#' Transform simulations arrays
#'
#' Performs computations datawise.
#' @inheritParams sims-verbs
#' @importFrom dplyr mutate_
#' @export
#' @seealso dplyr's \code{\link[dplyr]{mutate}()}
mutate_.sims <- function(.data, ..., .dots, .context = list()) {
  .data <- as_sims(.data, sims_major = TRUE)
  .dots <- lazyeval::auto_name(.dots)

  # Pull out .context argument from dots
  is_context <- ".context" == names(.dots)
  if (any(is_context)) {
    .context <- c(.context, lazyeval::lazy_eval(.dots$.context))
    .dots <- .dots[!is_context]
  }

  for (param in names(.dots)) {
    .data[[param]] <- by_sim(.data, .dots[[param]][["expr"]],
      .context = .context)
  }
  .data
}

#' @rdname sims-verbs
#' @export
mutate_.stanfit <- function(.data, ..., .dots, .context = list()) {
  mutate_(as_sims(.data), ..., .dots = .dots, .context = .context)
}
