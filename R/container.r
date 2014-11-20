
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

  # Ignore names except if last statement (can be either quoted or
  # storaged)
  irrelevant <- papply(dots, function(item) is.name(item) && !is.quoted(item))
  last(irrelevant) <- FALSE
  dots <- dots[!irrelevant]

  # Give priority to objects in storage
  last_name <- as.character(last(dots))
  if (is.quoted(last(dots)) && is.null(storage(last_name)))
    last(dots) <- eval(last(dots), calling_env())

  # Get actual expressions
  is_quoted <- papply(dots, is.quoted)
  dots[is_quoted] <- lapply(dots[is_quoted], eval, envir = calling_env())

  # Get everything inside one single curly
  is_curly <- papply(dots, is.curly)
  dots[is_curly] <- lapply(dots[is_curly], function(item) as.list(item[-1]))
  dots <- unlist2(dots, recursive = FALSE)
  curly <- do.call(call, c(list("{"), dots), quote = TRUE)

  res <- eval_curly(curly)

  out_name <- 
    if (is.name(last(dots)))
      as.character(last(dots))
    else
      NULL
  res <- maybe_tidy(res, out_name)

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
  storage$`_i` <- NULL
  storage$`_ref_stack` <- NULL

  lapply(storage, head)
}

#' @export
print.gsim_container <- function(x) {
  storage <- environment(x)$storage
  storage$`_i` <- NULL
  storage$`_ref_stack` <- NULL

  is_posterior <- vapply(storage, is.posterior, logical(1))
  n_posterior <- sum(is_posterior)
  n_data <- length(storage) - n_posterior

  cat("gsim container with", n_data, "variables and", n_posterior, "parameters\n")
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
