
#' @export
across_sims <- function(.x, .f, ...) {
  UseMethod("across_sims")
}

#' @export
across_sims.sims_array <- function(.x, .f, ...) {
  n_dims <- length(dim(.x))
  if (n_dims == 1) {
    .f(.x, ...)
  } else if (is_sims_major(.x)) {
    dims <- dim(.x)[-n_dims]
    apply(.x, seq(1, n_dims - 1), .f, ...) %>% set_dim(dims)
  } else {
    dims <- dim(.x)[-1]
    apply(.x, seq(2, n_dims), .f, ...) %>% set_dim(dims)
  }
}
