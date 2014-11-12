
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
    else if (sys.call(-4)[[1]] == as.name("data_frame_")) {
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


#' @export
get_omega <- function(resid, sigma) {
  omega <- (sd(resid) / mean(sigma))^2
  pmin(omega, 1)
}


#' @export
row_vector <- function(x) {
  rbind(c(x))
}

#' @export
col_vector <- function(x) {
  cbind(c(x))
}


## Faster than apply because uses colMeans
#' @export
colVars <- function(a) {
  class(a) <- "matrix"
  n <- nrow(a)
  c <- ncol(a)
  .colMeans(((a - matrix(.colMeans(a, n, c), nrow = n, ncol = c, byrow = TRUE))^2), n, c) * n / (n - 1)
}

#' @export
rowVars <- function(a) {
  class(a) <- "matrix"  # Circumvents a bug
  n <- nrow(a)
  c <- ncol(a)
  .rowMeans(((a - matrix(.rowMeans(a, n, c), nrow = n, ncol = c, byrow = TRUE))^2), n, c) * n / (n - 1)
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
