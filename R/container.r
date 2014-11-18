
container <- function(...) {
  `_anchor` <- TRUE
  dots <- eval(substitute(alist(...)))
  browser(expr = getOption("debug_on"))

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

  res <- pass1_curly(curly)

  res <- 
    if (is.AsIs(res))
      deprotect(res, AsIs = TRUE)

    else if (is.protected(res))
      deprotect(res)

    else if (is.list(res)) {
      is_as_is <- papply(res, is.AsIs)
      is_protected <- papply(res, is.protected)
      to_tidy <- !is_as_is & !is_protected

      res[is_as_is] <- lapply(res[is_as_is], deprotect, AsIs = TRUE)
      res[is_protected] <- lapply(res[is_protected], deprotect)
      res[to_tidy] <- lapply(res[to_tidy], as.data.frame)

      res
    }

    else if (!is.null(res) && !is.reactive_fun(res))
      as.data.frame(res)

  invisible(res)
}

pass1_curly <- function(x) {
  force(x)
  if (length(x) == 2)
    pass1_statement(x[[2]], last_statement = TRUE)
  else {
    others <- x[seq(2, max(length(x) - 1, 2))]
    last <- last(x)
    for (i in seq_along(others))
      enclos <- pass1_statement(others[[i]])
    pass1_statement(last, last_statement = TRUE)
  }
}

P <- function(x) {
  class(x) <- c(class(x), "protected")
  x
}

is.AsIs <- function(x) {
  inherits(x, "AsIs")
}

is.protected <- function(x) {
  inherits(x, "protected")
}

deprotect <- function(x, AsIs = FALSE) {
  class_diff <- 
    if (AsIs)
      c("AsIs", "posterior")
    else
      "protected"

  class(x) <- setdiff(class(x), class_diff) %||% "numeric"
  x
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
  class(fun) <- c("gsim_fun", "function")
  invisible(fun)
}
