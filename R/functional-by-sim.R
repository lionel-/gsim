#' Map a function simulation-wise
#'
#' \code{by_sim()} is similar to \code{apply()} in that it applies a
#' function \code{.f} to the margins of an array. \code{by_sim()} is
#' specialised to apply the function on each simulation of a
#' \code{sims_array}. It can be also be applied to a list of
#' simulations, in which case \code{.f} may depend on several
#' parameters.
#' @param .x A list of simulations arrays, or a simulations array.
#' @param .f A function or an expression to be applied to each
#'   simulation.
#' @param ... Further arguments passed on to \code{.f}.
#' @param .context A list or a data frame that contains additional
#'   variables used in \code{.f}.
#' @param .bindings A named vector linking the arguments of \code{.f}
#'   to the names of the simulations arrays in \code{.x}, in case they
#'   do not match.
#' @param .sims A list of simulations arrays containing additional
#'   parameters on which \code{.statistic} depends.
#' @export
by_sim <- function(.x, .f, ..., .context = list(), .bindings = NULL) {
  UseMethod("by_sim")
}

#' @rdname by_sim
#' @export
by_sim.list <- function(.x, .f, ..., .context = list(),
                        .bindings = NULL) {
  .x <- as_sims(.x)
  .x <- to_sims_major(.x)

  # A sims object may contain additional data
  is_sims_array <- map_lgl(.x, is_sims_array)
  .context <- c(.context, .x[!is_sims_array])
  .x <- .x[is_sims_array]

  n_sims <- unique(map_dbl(.x, n_sims))
  if (length(n_sims) != 1) {
    stop("Arrays do not contain the same number of simulations",
      call. = FALSE)
  }

  # Rename parameters according to bindings
  param_names <- names(.x)
  for (i in seq_along(.bindings)) {
    binding_index <- match(.bindings[i], param_names)
    names(.x)[binding_index] <- names(.bindings)[i]
  }

  if (is.language(.f)) {
    fun <- function() {}
    body(fun) <- .f
    .f <- fun
  }
  .f <- adjust_signature(.f, .x)

  # Set up evaluation environment with .context contents in scope
  calling_env <- parent.frame()
  environment(.f) <- list2env(.context, parent = environment(.f))

  data_dims <- map(.x, data_dims)
  by_sim_impl(.x, data_dims, n_sims, .f, calling_env, list(...))
}

#' @rdname by_sim
#' @export
by_sim.sims <- by_sim.list

#' @rdname by_sim
#' @export
by_sim.sims_array <- function(.x, .f, ..., .context = list(),
                              .sims = list(), .bindings = NULL) {
  .x <- permute_sims(.x, to_major = TRUE)

  param_name <- names(formals(args(.f)))[1]
  param_name %||% stop(".f should have at least one argument", call. = FALSE)

  # Handle primitive functions because they don't have formals.
  # It's okay to wrap them because their function bodies won't
  # ever contain parameters anyway.
  if (is.primitive(.f)) {
    args <- splice(name = ".f", quote(`_array`), signature(.f))
    cl <- do.call(call, args, quote = TRUE)
    f <- function(...) eval(cl)
  } else {
    f <- .f
  }

  # Handle functions taking dots, such as max()
  if (param_name == "...") {
    formals(f) <- c(list(`_array` = substitute()), formals(f))
    param_name <- "_array"
  }

  .sims[[param_name]] <- .x
  by_sim.list(.sims, f, ..., .context = .context, .bindings = .bindings)
}


# A version of formals() that handles primitive functions and dots
# correctly. Any signature returned by signature() can be fed back to
# do.call("call", ..., quote = TRUE) to recreate a function with the
# same signature.
signature <- function(fun) {
  signature <-
    if (is.primitive(fun)) {
      formals(args(fun))
    } else {
      formals(fun)
    }

  is_dots <- which("..." == names(signature))
  signature[[is_dots]] <- quote(...)
  names(signature)[is_dots] <- ""

  signature
}

# Add relevant parameters to .f's signature.
adjust_signature <- function(f, x) {
  # Wrap primitive functions because by_sim() expects a normal
  # function
  if (is.primitive(f)) {
    old_f <- f
    f <- function(...) {}
    body(f) <- quote({old_f(...)})
  } else {
    params_names <- find_names(body(f)) %>% intersect(names(x))
    params_names <- params_names %>% setdiff(names(formals(f)))
    params <- rep(list(substitute()), length(params_names))
    names(params) <- params_names
    formals(f) <- c(formals(f), params)
  }
  f
}
