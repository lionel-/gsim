
#' @export
print.sims_array <- function(x, ...) {
  if (is_sims_major(x)) {
    order <- "(sims-major order)"
  } else {
    order <- "(data-major order)"
  }

  data_dims <- paste(data_dims(x), collapse = " x ")
  if (identical(data_dims, "")) {
    data_dims <- paste0("[scalar]")
  } else {
    data_dims <- paste0("[", data_dims, "]")
  }

  i <- sample(seq_len(n_sims(x)), 1)
  cat("Simulation", paste0(i, "/", n_sims(x), ":"), data_dims, order, "\n\n")
  print_array(x[[i]])

  invisible(NULL)
}

print_array <- function(x) {
  dims <- dim(x) %||% length(x)
  if (length(dims) == 1) {
    dims <- c(dims, 1)
    dim(x) <- dims
  }

  nrows <- min(10, dims[1])
  ncols <- dims[2]

  args <- rep(list(1), length(dims) - 2)
  args <- splice(x, seq_len(nrows), substitute())
  subset <- map_call(args, `[`)

  df <- dplyr::trunc_mat(as.data.frame(subset), nrows)
  df <- map(df$table, function(col) unclass(col[-1]))
  names(df) <- seq_along(df)
  class(df) <- "data.frame"

  print(df)
  invisible(NULL)
}

#' @export
str.sims_array <- function(object, ...) {
  if(is_sims_major(object)) {
    order <- "simulation-major order"
  } else {
    order <- "data-major order"
  }
  cat("'sims_array':", n_sims(object), "sims", "in", order, "\n")
  object <- unclass(object)
  NextMethod()
}

#' @export
`[[.sims_array` <- function(x, i) {
  if (missing(i) || is.null(i)) {
    i <- sample(seq_len(n_sims(x)), 1)
  }
  args <- rep(list(substitute()), length(data_dims(x)))
  if (is_sims_major(x)) {
    args <- splice(unclass(x), args, i)
  } else {
    args <- splice(unclass(x), i, args)
  }
  map_call(args, `[`)
}

#' @export
`[.sims_array` <- function(x, ..., drop = FALSE) {
  # Need NSE here because list(...) cannot capture empty arguments
  dots <- unclass(lazyeval::lazy_dots(...))

  is_empty_arg <- map_lgl(dots, ~!identical(.x$expr, substitute()))
  dots <- map_if(dots, is_empty_arg, lazyeval::lazy_eval)
  dots[!is_empty_arg] <- list(substitute())

  if (!length(dots) %in% c(1, data_dims(x))) {
    stop("incorrect number of dimensions", call. = FALSE)
  }

  if (length(dots) == 1) {
    out <- subset_address(x, dots[[1]])
  } else {
    out <- subset_dims(x, dots, drop = drop)
  }

  structure(out,
    class = "sims_array",
    sims_major = is_sims_major(x)
  )
}

subset_address <- function(x, seq) {
  nsims <- n_sims(x)
  ndata <- prod(data_dims(x))

  if (is_sims_major(x)) {
    q <- ndata * (seq_len(nsims) - 1)
    ind <- rep(seq, nsims) + rep(q, each = length(seq))
    unclass(x)[ind]
  } else {
    out <- flatmap(seq, function(index) {
      start <- (index - 1) * nsims + 1
      seq <- seq(start, start + nsims - 1)
      unclass(x)[seq]
    })
    dim(out) <- c(nsims, length(seq))
    sims_array(out, sims_major = FALSE)
  }
}

subset_dims <- function(x, dots, drop) {
  if (is_sims_major(x)) {
    dots <- splice(dots, substitute(), drop = drop)
  } else {
    dots <- splice(substitute(), dots, drop = drop)
  }
  map_call(c(list(unclass(x)), dots), `[`)
}
