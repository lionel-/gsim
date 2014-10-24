

wrap_posterior <- function(x, dims) {
  call <- do.call(call, c(list("[", x, quote(`_i`)), replicate(length(dims) - 1, substitute())), quote = TRUE)
  # call$drop <- FALSE
  call
}

eval_first <- function(expr) {
  assign("_i", 1, envir = input_env())
  eval_in_input(expr)
}

process_call <- function(x) {
  if (is.name(x)) {
    obj <- get_in_input(deparse(x))

    if (is.posterior(obj))
      wrap_posterior(x, dim(obj))
    else
      x
  }

  else if (is.vectorized(x))
    x

  else if (is.assignment(x)) {
    browser(expr = getOption("debug_on"))
    # write _last logic here?
    lhs <- x[[2]]
    rhs <- process_call(x[[3]])

    ## if (is.saved(rhs) && (is.reactive(rhs) || is.vectorized(rhs)))
    # imbroglio: reactives are saved... cannot distinguish posterior and non-posterior
    # solution: rename `saved` to `to_loop`. What about vectorized? Vectorized is to_loop also.
    if (is.vectorized(rhs))
      vectorized(call("<-", lhs, rhs))
    else if (is.)
    else {
      # Need to get dimensions of rhs to construct the lhs subsetting
      rhs_obj <- eval_first(rhs) %>% init_posterior
      assign_in_input(deparse(lhs), rhs_obj)
      lhs <- wrap_posterior(lhs, dim(rhs_obj))
      call("<-", lhs, rhs)
    }
  }

  else {
    if (is.call(x)) {
      # If the arguments of a call are all vectorized operations, this
      # call is itself vectorized. If the arguments are mixed, then we
      # need to loop over the vectorised arguments as well.

      is_vectorized <- vapply(x, is.vectorized, logical(1))
      if (!all(is_vectorized)) {
        # Get dimensions of vectorized arguments to wrap them
        x[is_vectorized] <- lapply(x[is_vectorized], function(item) {
          obj <- eval_first(item) %>% init_posterior
          wrap_posterior(item, dim(obj))
        })
        x[!is_vectorized] <- lapply(x[!is_vectorized], process_call)
      }
    }

    x
  }
}
