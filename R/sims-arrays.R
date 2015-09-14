#' Construct a simulations array object
#'
#' @param array An array of simulations.
#' @param sims_major If \code{TRUE}, this indicates that the array is
#'   stored in simulations-major order as opposed to data-major order.
#' @seealso \code{\link{permute_sims}()}
#' @export
sims_array <- function(array, sims_major) {
  if (!is.array(array)) {
    stop("object is not an array", call. = FALSE)
  }
  stopifnot(is.logical(sims_major))

  structure(array,
    class = c("sims_array", "array"),
    sims_major = sims_major
  )
}

#' @rdname sims_array
#' @export
is_sims_array <- function(x) inherits(x, "sims_array")

#' @rdname sims_array
#' @export
as_sims_array <- function(x, ...) {
  UseMethod("as_sims_array")
}


#' @export
n_sims <- function(x) {
  UseMethod("n_sims")
}
#' @rdname sims_array
#' @export
n_sims.sims_array <- function(x) {
  dims <- dim(x)
  if (is_sims_major(x)) {
    dims[[length(dims)]]
  } else {
    dims[[1]]
  }
}

#' @rdname sims_array
#' @export
data_dims <- function(x) {
  check_sims_array(x)
  dims <- dim(x)
  if (is_sims_major(x)) {
    dims[-length(dims)]
  } else {
    dims[-1]
  }
}

#' @rdname sims_array
#' @export
is_sims_major <- function(array) {
  sims_major <- attr(array, "sims_major")
  if (is.null(sims_major)) {
    stop("Not a sims_array object", call. = FALSE)
  }
  sims_major
}

#' @rdname sims_array
#' @export
sims_margin <- function(x) {
  check_sims_array(x)
  n_dims <- length(dim(x))
  if (n_dims == 1) {
    numeric(0)
  } else if (is_sims_major(x)) {
    seq(1, n_dims - 1)
  } else {
    seq(2, n_dims)
  }
}

check_sims_array <- function(x) {
  if (!is_sims_array(x)) {
    stop("Not a sims_array object", call. = FALSE)
  }
  invisible(NULL)
}

#' @export
print.sims_array <- function(x, ...) {
  if (is_sims_major(x)) {
    order <- "(sims-major order)"
  } else {
    order <- "(data-major order)"
  }
  cat("Simulation array", order, "\n\n")

  data_dims <- paste(data_dims(x), collapse = " x ")
  if (identical(data_dims, "")) {
    cat(n_sims(x), "sims scalar\n")
  } else {
    cat(n_sims(x), "sims x", data_dims, "\n")
  }
  invisible(NULL)
}

#' Rearrange the dimensions of an MCMC array
#'
#' \code{to_data_major()} and \code{to_sims_major()} rearrange the
#' dimensions of an array so that the simulations are indexed along
#' the first dimension (data-major order) or the last
#' (simulation-major order).
#'
#' Indexing the simulations in the right dimension is important when
#' performance is critical. Data-wise operations (such as taking a
#' linear combination of regression coefficients ) are faster when the
#' first dimension indexes simulations. On the other hand,
#' simulation-wise operations (such as taking the posterior mean of a
#' quantity) are faster when the simulations are indexed along the
#' last dimension.
#' @param x A sims or sims_array object.
#' @export
to_sims_major <- function(x) {
  UseMethod("to_sims_major")
}

#' @rdname to_sims_major
#' @export
to_sims_major.sims_array <- function(x) {
  if (attr(x, "sims_major")) {
    return(x)
  }

  dims <- seq_along(dim(x))
  dims <- c(dims[-1], 1)
  aperm(x, dims) %>% sims_array(sims_major = TRUE)
}

#' @rdname to_sims_major
#' @export
to_data_major <- function(x) {
  dims <- seq_along(dim(x))
  dims <- c(1, dims[-length(dims)])

  aperm(x, dims) %>% sims_array(sims_major = FALSE)
}

#' @rdname to_sims_major
#' @export
is_sims_major <- function(x) {
  attr(x, "sims_major") %||% stop("not a simulations array", call. = FALSE)
}
