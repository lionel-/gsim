
dots <- function (...) {
  eval(substitute(alist(...)))
}

set_dim <- function(x, dims) {
  dim(x) <- dims
  x
}

set_names <- function(x, ...) {
  value <- c(...)
  names(x) <- value
  x
}

first <- function(x) {
  x[[1]]
}

`first<-` <- function(x, value) {
  x[[1]] <- value
  x
}

last <- function(x) {
  x[[length(x)]]
}

`last<-` <- function(x, value) {
  x[[length(x)]] <- value
  x
}

dim_length <- function(x) {
  length(dim(x))
}

`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) > 0)
    a
  else
    b
}

check_packages <- function(...) {
  packages <- c(...)
  is_installed <- map_lgl(packages, requireNamespace, quietly = TRUE)

    if (sum(!is_installed) > 1) {
      msg <- "Please install the following packages: "
    } else {
      msg <- "Please install "
    }
  packages <- paste(c(as.list(packages[!is_installed]), collapse = ", "))

  if (any(!is_installed)) {
    stop(paste0(msg, packages), call. = FALSE)
  }
  invisible(NULL)
}

is_formula <- function(x) {
  inherits(x, "formula")
}

call_fun <- function(call) {
  as.character(call[[1]])
}
