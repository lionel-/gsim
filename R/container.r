
container <- function(...) {
  `_anchor` <- TRUE
  dots <- eval(substitute(alist(...)))

  is.curly <- function(x) inherits(x, "{")
  is.quoted <- function(x) {
    if (is.curly(x) || !is.name(x))
      return(FALSE)
    evaled <- try(eval(x, calling_env()), silent = TRUE)
    is.language(evaled) || inherits(evaled, "{")
  }
  is.in_storage <- function(x) {
    name <- as.character(x)
    name %in% names(storage())
  }

  # Ignore names except if last statement (can be either quoted or
  # storaged)
  irrelevant <- papply(dots, function(item) is.name(item) && !is.quoted(item))
  last(irrelevant) <- FALSE
  dots <- dots[!irrelevant]

  # Give priority to objects in storage
  if (is.quoted(last(dots)) && !is.in_storage(last(dots)))
    last(dots) <- eval(last(dots), calling_env())

  # Get actual expressions
  is_quoted <- papply(dots, function(x) is.quoted(x) && !is.in_storage(x))
  dots[is_quoted] <- lapply(dots[is_quoted], eval, envir = calling_env())

  # Get everything inside one single curly
  is_curly <- papply(dots, is.curly)
  dots[is_curly] <- lapply(dots[is_curly], function(item) as.list(item[-1]))
  dots <- unlist2(dots, recursive = FALSE)
  curly <- do.call(call, c(list("{"), dots), quote = TRUE)

  res <- eval_curly(curly)

  out_names <- get_output_names(last(dots))
  res <- maybe_tidy(res, out_names)

  if (is.reactive_fun(res))
    clear_reactive_data()

  invisible(res)
}


eval_curly <- function(x) {
  force(x)

  if (length(x) == 2)
    eval_statement(x[[2]], last_statement = TRUE)
  else {
    others <- x[seq(2, max(length(x) - 1, 2))]
    last <- last(x)
    for (i in seq_along(others))
      enclos <- eval_statement(others[[i]])
    eval_statement(last, last_statement = TRUE)
  }
}

eval_statement <- function(x, last_statement = FALSE) {
  if (is.assignment(x))
    pass1_assignment(lhs = deparse(x[[2]]), rhs = x[[3]])
  else if (last_statement)
    pass1_assignment(lhs = "_last", rhs = x)

  if (last_statement) {
    # null() so that storage() recognizes that it should return NULL
    if (is.assignment(x))
      assign_in_storage("_last", null())

    stack <- context("call_stack")
    stack <- pass2_stack(stack)
    clear_call_stack()
    clear_refs(stack)

    if ("_last" %in% context("locked"))
      make_reactive_function()
    else
      storage("_last")
  }
  else
    NULL
}


#' @export
summary.gsim_container <- function(x) {
  storage <- environment(x)$storage
  storage[grep("^_", names(storage))] <- NULL

  lapply(storage, summary.default)
}

#' @export
head.gsim_container <- function(x, ...) {
  storage <- environment(x)$storage
  storage[grep("^_", names(storage))] <- NULL

  lapply(storage, head, ...)
}

#' @export
print.gsim_container <- function(x) {
  storage <- environment(x)$storage
  storage[grep("^_", names(storage))] <- NULL

  is_posterior <- vapply(storage, is.posterior, logical(1))
  n_posterior <- sum(is_posterior)
  n_data <- length(storage) - n_posterior

  names_posterior <- names(storage)[is_posterior]
  names_posterior <- do.call(paste, c(as.list(names_posterior), list(sep = ", ")))
  names_posterior <- paste0("(", names_posterior, ")")
  names_data <- names(storage)[!is_posterior]
  names_data <- do.call(paste, c(as.list(names_data), list(sep = ", ")))
  names_data <- paste0("(", names_data, ")")

  cat(paste0("gsim container with: \n\n", n_posterior), "parameters",
    names_posterior, "and", n_data, "variables", names_data, "\n")
}

#' @export
clone <- function(old_fun) {
  if (!inherits(old_fun, "gsim_container"))
    stop("This is not a `gsim` container")

  context <- environment(old_fun)$context
  storage <- environment(old_fun)$storage
  rm(old_fun)

  fun <- container
  environment(fun) <- environment()
  class(fun) <- c("gsim_container", "function")
  invisible(fun)
}
