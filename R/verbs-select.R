#' Select/rename parameters
#'
#' Select parameters from a list of
#' simulations. \code{\link{select}()} keeps only the variables
#' mentioned in \code{...}. \code{\link{rename}()} keeps all
#' variables.
#'
#' @section Special functions:
#' From dplyr's man page: As well as using existing functions like
#' \code{:} and \code{c}, there are a number of special functions that
#' only work inside \code{select}
#'
#' \itemize{
#'  \item \code{starts_with(x, ignore.case = TRUE)}:
#'    names starts with \code{x}
#'  \item \code{ends_with(x, ignore.case = TRUE)}:
#'    names ends in \code{x}
#'  \item \code{contains(x, ignore.case = TRUE)}:
#'    selects all variables whose name contains \code{x}
#'  \item \code{matches(x, ignore.case = TRUE)}:
#'    selects all variables whose name matches the regular expression \code{x}
#'  \item \code{num_range("x", 1:5, width = 2)}:
#'    selects all variables (numerically) from x01 to x05.
#'  \item \code{one_of("x", "y", "z")}:
#'    selects variables provided in a character vector.
#'  \item \code{everything()}:
#'    selects all variables.
#' }
#' @inheritParams sims-verbs
#' @rdname select
#' @importFrom dplyr select_
#' @seealso dplyr's \code{\link[dplyr]{select}()} and
#'   \code{\link[dplyr]{rename}()}
#' @export
select_.sims <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- select_vars_(names(.data), dots)
  .data[vars]
}

#' @export
#' @rdname select
select_.stanfit <- function (.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- dplyr::select_vars_(.data@model_pars, dots)
  as_sims(.data, sims_major = FALSE, vars = vars)
}


#' @rdname select
#' @importFrom dplyr rename_
#' @export
rename_.sims <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- rename_vars_(names(.data), dots)
  .data[vars] %>% set_names(names(vars))
}

#' @rdname select
#' @export
rename_.stanfit <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  as_sims(.data, sims_major = FALSE) %>% rename_(..., .dots = dots)
}
