#' Map a function to a list of simulations
#'
#' @export
by_sim <- function(.s, .f, ..., .data = NULL, .bindings = NULL) {
  .s <- as_sims(.s, major_order = TRUE)

  # Rename parameters according to bindings
  param_names <- names(.s)
  for (i in seq_along(.bindings)) {
    binding_index <- match(.bindings[i], param_names)
    names(.s)[binding_index] <- names(.bindings)[i]
  }

  # Set up evaluation environment with .data contents in scope
  calling_env <- calling_env()
  eval_env <-
    if (is.null(.data)) {
      calling_env
    } else {
      list2env(data, calling_env)
    }

  n_data <- map_v(.s, function(array) {
    dims <- dim(array)
    prod(dims[-length(dims)])
  }, .type = numeric(1))
  names(n_data) <- names(.s)

  dims_first <- dim(.s[[1]])
  n_sims <- dims_first[length(dims_first)]

  by_sim_impl(.s, n_data, n_sims, .f, eval_env)
}
