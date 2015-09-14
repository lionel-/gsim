
#' @rdname as_sims
#' @export
as_sims.stanfit <- function(x, sims_major = NULL, vars = NULL, ...) {
  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("Package `rstan` is not installed", call. = FALSE)
  }

  if (is.null(vars)) {
    out <- rstan::extract(x)
  } else {
    out <- rstan::extract(x, vars)
  }
  out <- out %>%
    map(sims_array, sims_major = FALSE) %>%
    structure(class = "sims")

  if (sims_major %||% FALSE) {
    out <- permute_sims(out, to_major = TRUE)
  }

  out
}

#' @rdname as_sims
#' @export
as_sims.jagslist <- function(x, ...) {
  sims <- map(x, as_sims_array)
  structure(sims, class = "sims")
}

#' @rdname sims_array
#' @export
as_sims_array.mcarray <- function(x, ...) {
  dim <- dim(x)
  n_dims <- length(dim)
  n_iter <- dim[n_dims] * dim[n_dims - 1]
  new_dim <- c(dim[seq_len(n_dims - 2)], n_iter)

  x <- array(x, new_dim)
  sims_array(x, sims_major = TRUE)
}

#' @rdname as_sims
#' @export
#' @importFrom stringr str_extract str_match str_match_all str_replace_all
as_sims.mcmc.list <- function(x, ...) {
  x <- do.call(rbind, x)
  n_sims <- dim(x)[1]

  names <- dimnames(x)[[2]]
  attr(x, "dimnames") <- NULL

  indices <- names %>%
    str_extract("\\[([[:digit:]],?)+\\]$") %>%
    str_replace_all("\\[|\\]", "")
  indices[is.na(indices)] <- "1"
  indices <- str_match_all(indices, "([[:digit:]]*),?") %>%
    map(~ .x[, 2]) %>%
    map(compose(c, na.omit, as.numeric))

  param_names <- str_match(names, "([^\\[]+)(\\[|$)")[, 2]
  param_start <- match(unique(param_names), param_names)

  create_indices <- function(param, beg) {
    end <- sum(param_names == param) + beg - 1
    seq(beg, end)
  }
  param_indices <- map2(unique(param_names), param_start, create_indices)

  sims <- map(param_indices, function(index) {
    ind <- indices[index]
    ind <- map_call(ind, rbind)

    dims <- apply(ind, 2, max)
    while(!is.null(dims) && last(dims) == 1) {
      if (length(dims) == 1) {
        dims <- NULL
      } else {
        dims <- dims[-length(dims)]
      }
    }

    array(x[, index], c(n_sims, dims))
  })

  sims <- map(sims, sims_array, sims_major = FALSE)
  structure(sims, class = "sims", names = unique(param_names))
}
