#' Gorgeous Simulations
#'
#' @name gsim-package
#' @docType package
#' @useDynLib gsim
#' @importFrom Rcpp sourceCpp
#' @importFrom purrr map map_dbl map_chr map_lgl map_if walk2 every splice
#' @importFrom purrr map_call slice_rows by_slice keep negate is_formula
#' @importFrom purrr compose flatmap
NULL

#' Pipe
#'
#' See \code{\link[magrittr]{\%>\%}}.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %>%
#' @export
#' @usage lhs \%>\% rhs()
NULL
