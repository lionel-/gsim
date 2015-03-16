#' Coerce a set of MCMC arrays to a sims object
#'
#' Converts posterior simulations and fitted models to an
#' mclist. Simulations from fitted models are obtained through
#' \code{sim} from the package arm.
#' @param n_sims Passed to sim from the arm package. Number of
#'   simulations to obtain from a fitted model.
#' @param major_order If \code{TRUE}, the simulations array is
#'   arranged in simulation-major order. If \code{FALSE}, the array is
#'   arranged in data-major order. If \code{NULL}, the order of the
#'   original data structure is retained.
#' @return An mclist. This is a list of posterior simulations with one
#'   element per parameter. Each list element is an array whose the
#'   first dimension represents the simulations.
#' @export
as_sims <- function(x, major_order = NULL, ...) {
  UseMethod("as_sims")
}

#' @describeIn as_sims Convert Stan simulations to mclist
#' @export
as_sims.stanfit <- function(x, major_order = NULL, ...) {
  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("Package `rstan` is not installed", call. = FALSE)
  }

  out <- rstan::extract(x) %>% map(sims_array, major_order = FALSE)
  if (major_order) {
    out <- permute_sims(out, to_major = TRUE)
  }
  structure(out, class = "sims")
}

#' @describeIn as_sims Convert arm::sim simulations to mclist
#' @export
as_sims.sim <- function(x, major_order = NULL, ...) {
  sims <- process_arm_sims(x)
  names(sims)[match("coef", names(sims))] <- "beta"
  names(sims)[match("zeta", names(sims))] <- "z_eta"
  dimnames(sims$beta)[[2]] <- clean_coefnames(dimnames(sims$beta)[[2]])
  class(sims) <- "sims"
  sims
}

#' @export
as_sims.sim.polr <- function(x, major_order = NULL, ...) {
  suppressMessages(as_sims.sim(x, ...))
}

#' @describeIn as_sims Simulate from a lm fitted model and convert
#' to mclist
#' @export
as_sims.lm <- function(x, major_order = NULL, n_sims = 100) {
  check_packages("arm", "MASS")

  # FIXME: until arm puts mvrnorm in importFrom
  was_MASS_attached <- "package:MASS" %in% search()
  suppressMessages(library("MASS"))
  sims <- arm::sim(x, n.sims = n_sims)

  if (!was_MASS_attached)
    detach("package:MASS")

  as_sims.sim(sims)
}

#' @describeIn as_sims Simulate from a glm fitted model and
#' convert to mclist
#' @export
as_sims.glm <- as_sims.lm

#' @describeIn as_sims Simulate from a glm fitted model and
#' convert to mclist
#' @export
as_sims.polr <- as_sims.lm

process_arm_sims <- function(sims) {
  p_names <- slotNames(sims)

  params <- lapply(p_names, function(param) slot(sims, param))
  names(params) <- p_names
  Filter(f = function(x) !(is.null(first(x))), params)
}

#' @importMethodsFrom arm fitted
#' @export
as_sims.sim.merMod <- function(x, major_order = NULL, obj = NULL, ...) {
  sims <- process_arm_sims(x)
  n_sims <- first(dim(x@fixef))

  ranef <- lapply(sims$ranef, function(x) {
    npar <- dim(x)[3]
    x <- lapply(seq_len(npar), function(i) x[, , i]) %>%
      set_names(dimnames(x)[[3]])

    nobs <- dim(first(x))[2]
    res <- do.call(c, x)
    res <- do.call(array, list(res, c(n_sims, nobs, length(x))))
    dimnames(res) <- list(NULL, NULL, names(x) %>% clean_coefnames)
    res
  })

  ranef <- set_list_dimnames(ranef)
  names(ranef) <- paste0(names(ranef), "_coefs")

  sims$ranef <- NULL
  names(sims)[match("fixef", names(sims))] <- "beta"
  dimnames(sims$beta)[[2]] <- clean_coefnames(dimnames(sims$beta)[[2]])
  sims <- set_list_dimnames(sims)

  if (!is.null(obj)) {
    fitted <- fitted(x, obj)
    attr(fitted, "dimnames") <- NULL
    sims$fitted <- t(fitted)
  }

  c(sims, ranef)
}

set_list_dimnames <- function(list) {
  Map(function(x, name) {
    if (is.null(dim(x)))
      x
    else {
      names(dimnames(x))[dim_length(x)] <- name
      x
    }
  }, list, names(list))
}

#' @describeIn as_sims Simulate from a lmer fitted model and
#' convert to mclist
#' @export
as_sims.merMod <- function(x, major_order = NULL, n_sims = 100) {
  check_packages("arm", "MASS")

  # FIXME: until arm puts mvrnorm in importFrom
  was_MASS_attached <- "package:MASS" %in% search()
  suppressMessages(library("MASS"))
  sims <- arm::sim(x, n.sims = n_sims)

  if (!was_MASS_attached)
    detach("package:MASS")

  as_sims.sim.merMod(sims, x)
}

#' @describeIn as_sims Simulate from a glmer fitted model and
#' convert to mclist
as_sims.glmerMod <- as_sims.merMod


clean_coefnames <- function(names) {
  patterns <- c(
    "\\(Intercept\\)",
    "(I\\()([^)]+)(\\^2\\))",
    "(I\\()([^)]+)(\\^)([[:digit:]])(\\))"
  )
  replacements <- c(
    "intercept",
    "\\2_quad",
    "\\2_poly\\4"
  )

  Map(function(p, r) {
    names <<- sub(p, r, names)
  }, patterns, replacements)
  names
}


#' @describeIn as_sims Convert Jags or Stan simulation lists to mclist
#' @export
as_sims.list <- function(x, major_order = NULL, ...) {
  is_mcarray <- papply(x, function(item) inherits(item, "mcarray"))
  if (all(is_mcarray))
    as_sims.jagslist(x)

  else {
    all_array <- all(papply(x, is.array))
    sim_len <- lapply(x, function(item) dim(item)[1])
    same_sim_len <- all(sim_len == sim_len[[1]])

    if (all_array && same_sim_len) {
      class(x) <- "mclist"
      x
    }
    else
      stop("List is not a Jags/Stan object", call. = FALSE)
  }
}

as_sims.jagslist <- function(x) {
  mclist <- lapply(x, function(item) {
    dim <- dim(item)
    len <- length(dim)

    iter <- dim[len] * dim[len - 1]
    dim <- c(dim[seq_len(len - 2)], iter)

    item <- array(item, dim)
    perm_dims(item)
  })

  mclist
}


#' @describeIn as_sims Convert Coda simulations to mclist
#' @export
#' @importFrom stringr str_extract str_match str_match_all str_replace_all
as_sims.mcmc.list <- function(x, major_order = NULL, ...) {
  x <- do.call(rbind, x)
  n_sims <- dim(x)[1]

  names <- dimnames(x)[[2]]
  attr(x, "dimnames") <- NULL

  indices <- names %>%
    str_extract("\\[([[:digit:]],?)+\\]$") %>%
    str_replace_all("\\[|\\]", "")
  indices[is.na(indices)] <- "1"

  indices <- str_match_all(indices, "([[:digit:]]*),?") %>%
    apluck(, 2) %>%
    lapply(as.numeric)


  p_names <- str_match(names, "([^[]+)(\\[|$)")[, 2]
  p_start <- match(unique(p_names), p_names)
  p_indices <- Map(function(param, beg) {
    end <- sum(p_names == param) + beg - 1
    seq(beg, end)
  }, unique(p_names), p_start)

  sims <- lapply(p_indices, function(index) {
    ind <- indices[index]
    ind <- do.call(rbind, ind)

    dims <- apply(ind, 2, max)
    while (!is.null(dims) && last(dims) == 1) {
      dims <-
      if (length(dims) == 1)
        NULL
      else
        dims[-length(dims)]
    }

    array(x[, index], c(n_sims, dims))
  })
  names(sims) <- unique(p_names)

  sims
}


#' @rdname as_sims
#' @export
is.mclist <- function(x) inherits(x, "mclist")


# sims_list and sims?
# sims and param?
# sims and mcarray?
# sims and sims_array?


#' Construct a simulations array object
#'
#' @param array An array of simulations.
#' @param major_order If \code{TRUE}, this indicates that the array is
#'   stored in simulations-major order as opposed to data-major order.
#' @seealso \code{\link{permute_sims}()}
#' @export
sims_array <- function(array, major_order) {
  if (!is.array(array)) {
    stop("object is not an array", call. = FALSE)
  }

  structure(array,
    class = c("sims_array", "array"),
    major_order = major_order
  )
}


#' Rearrange the dimensions of an MCMC array
#'
#' \code{permute_sims()} rearranges the dimensions of an array,
#' switching between simulations-major order (where simulations are
#' indexed by the first array dimension) and data-major order (where
#' they are indexed by the last dimension). If \code{to_major} is
#' \code{NULL}, the \code{sims_major_order} attribute is consulted to
#' determine in which dimension the simulations are currently stored.
#'
#' Indexing the simulations in the right dimension is important when
#' performance is critical. Data-wise operations (such as taking a
#' linear combination of regression coefficients ) are faster when the
#' first dimension indexes simulations. On the other hand,
#' simulation-wise operations (such as taking the posterior mean of a
#' quantity) are faster when the simulations are indexed along the
#' last dimension.
#' @param sims Either a list of simulation arrays, or one such array.
#' @param to_major If \code{TRUE}, the array is permuted so that the
#'   first dimension becomes the last one. \code{FALSE} yields the
#'   opposite permutation.
#' @export
permute_sims <- function(sims, to_major = TRUE) {
  UseMethod("permute_sims")
}

#' @describeIn permute_sims
#' @export
permute_sims.sims <- function(sims, to_major = TRUE) {
  map(sims, permute_sims.sims_array, to_major = to_major)
}

#' @describeIn permute_sims
#' @export
permute_sims.sims_array <- function(x, to_major = TRUE) {
  is_major <- attr(x, "major_order")
  if (to_major == is_major) {
    return(sims_array(x, major_order = to_major))
  }

  dims <- seq_along(dim(x))[-1]
  dims <-
    if (to_major) {
      c(dims, 1)
    } else {
      c(1, dims)
    }

  aperm(x, dims) %>% sims_array(major_order = to_major)
}
