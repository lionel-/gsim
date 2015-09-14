#' Coerce to a sims object
#'
#' Converts posterior simulations and fitted models to a \code{sims}
#' object, a list of simulation arrays.
#' @param x An object to convert.
#' @param ... Further arguments passed to coercion functions.
#' @export
as_sims <- function(x, ...) {
  UseMethod("as_sims")
}

#' @rdname as_sims
#' @export
as_sims.sims <- function(x, ...) {
  x
}

#' @rdname as_sims
#' @param sims_major When converting a list of arrays to a \code{sims}
#'   object, this indicates whether your arrays are stored in
#'   simulation-major order.
#' @export
as_sims.list <- function(x, sims_major, ...) {
  if (every(x, inherits, "mcarray")) {
    return(as_sims.jagslist(x))
  }

  if (missing(sims_major)) {
    stop("Specify whether your arrays are stored in sim-major order",
      call. = FALSE)
  }
  x <- map(x, sims_array, sims_major = sims_major)
  structure(x, class = "sims")
}

#' @export
`[.sims` <- function(x, i) {
  structure(NextMethod(), class = "sims")
}

#' @export
c.sims <- function(..., recursive = FALSE) {
  out <- structure(NextMethod(), class = "sims")
  to_sims_major(out)
}

#' @rdname as_sims
#' @export
is_sims <- function(x) inherits(x, "sims")

#' @export
print.sims <- function(x, ...) {
  sims <- keep(x, is_sims_array)
  n_sims <- map_dbl(sims, n_sims) %>% unique()
  if (length(n_sims) > 1) {
    stop("number of simulations not consistent throughout list",
      call. = FALSE)
  }
  cat(n_sims, "simulations\n\n")

  max_length <- map_dbl(names(sims), nchar) %>% max()
  names(sims) <- map_chr(names(sims), function(name) {
    pad <- paste(rep(" ", max_length - nchar(name)), collapse = "")
    paste(pad, name)
  })

  data_dims <- map(sims, function(x) {
    dims <- dim(x)
    if (length(dims) == 1) {
      1
    } else if (is_sims_major(x)) {
      dims[seq(1, length(dims) - 1)]
    } else {
      dims[seq(2, length(dims))]
    }
  })

  walk2(names(sims), data_dims, function(n, d) {
    d <- paste(paste0("1:", d), collapse = ", ")
    cat(n, ": [", d, "]\n", sep = "")
  })

  names_other <- names(keep(x, negate(is_sims_array)))
  if (length(names_other) > 0) {
    cat("\nOther objects:", paste(names_other, collapse = ", "), "\n")
  }
  invisible(NULL)
}

#' @rdname as_sims
#' @export
n_sims.sims <- function(x) {
  is_array <- map_lgl(x, is_sims_array)
  n <- unique(map_dbl(x[is_array], n_sims))
  if (length(n) > 1) {
    stop("The sims arrays do not have the same length",
      call. = FALSE)
  }
  n
}

#' @rdname to_sims_major
#' @export
to_sims_major.sims <- function(x) {
  map_if(x, is_sims_array, to_sims_major)
}

#' @rdname to_sims_major
#' @export
to_data_major.sims <- function(x) {
  map_if(x, is_sims_array, to_data_major)
}
