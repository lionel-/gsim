#' Intercept column
#'
#' Create a column of ones. This is useful to make a design matrix.
#'
#' \code{n} is an optional argument: when \code{intercept} is nested
#' inside \code{cbind}, \code{data.frame} or \code{data_frame}, it
#' examines its surroundings to automatically determine its length.
#'
#' @param n
#' @examples
#' cbind(intercept(), 1:5)
#'
#' @export
intercept <- function(n = NULL) {
  if (is.null(n)) {
    is.name2 <- function(x) {
      is.name(x) || (is.call(x) && identical(x[[1]], as.name("$")))
    }
    is.call2 <- function(x) {
      is.call(x) && !identical(x[[1]], as.name("intercept"))
    }

    throw <- stopper("Without an informative context, need to specify `n`")
    call <- sys.call(-1)

    if (length(call) > 0 && as.character(first(call)) %in%
          c("cbind", "data.frame")) {
      args <- call[-1]
      name_arg <- Find(is.name2, args)
      call_arg <- Find(is.call2, args)
      env <- parent.frame()
    } 

    # dplyr::data_frame needs special treament due to `lazy_dots`
    else if (try(sys.call(-4)[[1]], TRUE) == as.name("data_frame_")) {
      cols <- sys.frame(-4)$columns

      name_lazy <- Find(function(x) is.name2(x$expr), cols)
      call_lazy <- Find(function(x) is.call2(x$expr), cols)
      name_arg <- name_lazy$expr
      call_arg <- call_lazy$expr
      env <- name_lazy$env %||% call_lazy$env
    }

    else
      throw()

    obj <- name_arg %||% call_arg %||% throw()
    n <- length(eval(obj, env))
  }

  rep(1, n)
}


#' Design matrices for hierarchical models
#'
#' TODO: Document me
#' @export
ones <- function(x, preds = list(NULL)) {
  n <- length(unique(x))
  nrow <- length(x)

  ones <- vector(length =  nrow * n)
  for (i in seq_len(n)) {
    from <- 1 + (i-1) * nrow
    to <- nrow + (i-1) * nrow
    ones[seq(from, to)] <- (x == i) + 0
  }
  ones <- matrix(ones, nrow, n)

  res <- lapply(preds, function(x) {
    if (is.null(x))
      ones
    else {
      temp <- matrix(NA, nrow, n)
      for (j in seq_len(ncol(ones)))
        temp[, j] <- ones[, j] * x
      temp
    }
  })
  
   do.call(cbind, res)
}


#' Make row vector
#'
#' Wrapper around rbind to produce a row vector. \code{row_vector(x)}
#' is equivalent to \code{rbind(x)} but it only accepts one argument
#' and is a clearer semantic signal.
#'
#' @param x numeric vector.
#' @return Array of dimension 1 x n.
#' @export
row_vector <- function(x) {
  rbind(c(x))
}

#' Make column vector
#'
#' Wrapper around cbind to produce a column vector. \code{col_vector(x)}
#' is equivalent to \code{cbind(x)} but it only accepts one argument
#' and is a clearer semantic signal.
#'
#' @param x numeric vector.
#' @return Array of dimension n x 1.
#' @export
col_vector <- function(x) {
  cbind(c(x))
}


#' Optimised variance computation
#'
#' colVars uses colMeans to provide an efficient way of computing the
#' variances of the columns of a matrix.
#'
#' @param x numeric matrix.
#' @export
colVars <- function(x) {
  class(x) <- "matrix"
  n <- nrow(x)
  c <- ncol(x)
  x <- ((x - matrix(.colMeans(x, n, c), nrow = n, ncol = c, byrow = TRUE))^2)
  .colMeans(x, n, c) *
    n / (n - 1)
}

#' Optimised variance computation
#'
#' rowVars uses rowMeans to provide an efficient way of computing the
#' variances of the rows of a matrix.
#'
#' @param x a numeric matrix
#' @export
rowVars <- function(x) {
  class(x) <- "matrix"  # Circumvents a bug
  n <- nrow(x)
  c <- ncol(x)
  x <- ((x - matrix(.rowMeans(x, n, c), nrow = n, ncol = c, byrow = TRUE))^2)
  .rowMeans(x, n, c) *
    n / (n - 1)
}


# TODO: still useful?
rbind_cols <- function(object) {
  if (!dim_length(object) == 2)
    stop("Can only rbind the cols of a matrix")
  if (dim(object)[2] == 1)
    return(object)

  ncols <- ncol(object)
  nrows <- nrow(object)
  array(object, c(nrows * ncols, 1))
}


#' Quantile helper functions
#'
#' These functions provide shortcuts to compute specific quantiles of
#' a numeric vector.
#' @name quantiles
#' @param x numeric vector.
NULL

#' @rdname quantiles
#' @export
q025 <- function(x) {
  quantile(x, 0.025)
}

#' @rdname quantiles
#' @export
q975 <- function(x) {
  quantile(x, 0.975)
}


#' @export
get_omega <- function(resid, sigma) {
  omega <- (sd(resid) / mean(sigma))^2
  pmin(omega, 1)
}


#' @export
inv_logit <- function(x) {
  1 / (1 + exp(-x))
}


#' Bernoulli distribution wrappers
#'
#' Wrappers for dbinom, pbinom, qbinom and rbinom. Specify
#' \code{size = 1}.
#' @name Bernoulli
NULL

#' @rdname Bernoulli
#' @export
dbernoulli <- function(n, prob, log = FALSE) {
  dbinom(x, size = 1, prob, log)
}

#' @rdname Bernoulli
#' @export
pbernoulli <- function(n, prob, lower.tail = TRUE, log.p = FALSE) {
  pbinom(q, size = 1, prob, lower.tail, log.p)
}

#' @rdname Bernoulli
#' @export
qbernoulli <- function(n, prob, lower.tail = TRUE, log.p = FALSE) {
  qbinom(n, size = 1, prob, lower.tail, log.p)
}

#' @rdname Bernoulli
#' @export
rbernoulli <- function(n, prob) {
  rbinom(n, size = 1, prob)
}
