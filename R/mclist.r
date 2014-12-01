#' mclist coercion
#'
#' Converts posterior simulations and fitted models to an
#' mclist. Simulations from fitted models are obtained through
#' \code{sim} from the package arm.
#'
#' @param n_sims Passed to sim from the arm package. Number of
#' simulations to obtain from a fitted model.
#' @return An mclist. This is a list of posterior simulations with one
#' element per parameter. Each list element is an array whose the
#' first dimension represents the simulations.
#' @export
as.mclist <- function(x, ...) {
  UseMethod("as.mclist")
}

#' @describeIn as.mclist Convert Stan simulations to mclist
#' @export
as.mclist.stanfit <- function(x, ...) {
  if (!requireNamespace("rstan", quietly = TRUE))
    stop("Package `rstan` is not installed", call. = FALSE)

  out <- rstan::extract(x)
  class(out) <- "mclist"
  out
}


#' @describeIn as.mclist Convert arm::sim simulations to mclist
#' @export
as.mclist.sim <- function(sims, ...) {
  mclist <- process_arm_sims(sims)
  names(mclist)[match("coef", names(mclist))] <- "beta"
  names(mclist)[match("zeta", names(mclist))] <- "z_eta"
  dimnames(mclist$beta)[[2]] <- clean_coefnames(dimnames(mclist$beta)[[2]])
  class(mclist) <- "mclist"
  mclist
}

#' @export
as.mclist.sim.polr <- function(sims, ...) {
  suppressMessages(as.mclist.sim(sims, ...))
}

#' @describeIn as.mclist Simulate from a lm fitted model and convert
#' to mclist
#' @export
as.mclist.lm <- function(x, n_sims = 100) {
  check_packages("arm", "MASS")

  # FIXME: until arm puts mvrnorm in importFrom
  was_MASS_attached <- "package:MASS" %in% search()
  suppressMessages(library("MASS"))
  sims <- arm::sim(x, n.sims = n_sims)

  if (!was_MASS_attached)
    detach("package:MASS")
  
  as.mclist.sim(sims)
}

#' @describeIn as.mclist Simulate from a glm fitted model and
#' convert to mclist
#' @export
as.mclist.glm <- as.mclist.lm

#' @describeIn as.mclist Simulate from a glm fitted model and
#' convert to mclist
#' @export
as.mclist.polr <- as.mclist.lm

process_arm_sims <- function(sims) {
  p_names <- slotNames(sims)

  params <- lapply(p_names, function(param) slot(sims, param))
  names(params) <- p_names
  Filter(f = function(x) !(is.null(first(x))), params)
}

#' @importMethodsFrom arm fitted 
#' @export
as.mclist.sim.merMod <- function(sims, obj = NULL, ...) {
  mclist <- process_arm_sims(sims)
  n_sims <- first(dim(sims@fixef))

  ranef <- lapply(mclist$ranef, function(x) {
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
  
  mclist$ranef <- NULL
  names(mclist)[match("fixef", names(mclist))] <- "beta"
  dimnames(mclist$beta)[[2]] <- clean_coefnames(dimnames(mclist$beta)[[2]])
  mclist <- set_list_dimnames(mclist)

  if (!is.null(obj)) {
    fitted <- fitted(sims, obj)
    attr(fitted, "dimnames") <- NULL
    mclist$fitted <- t(fitted)
  }

  c(mclist, ranef)
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

#' @describeIn as.mclist Simulate from a lmer fitted model and
#' convert to mclist
#' @export
as.mclist.merMod <- function(x, n_sims = 100) {
  check_packages("arm", "MASS")

  # FIXME: until arm puts mvrnorm in importFrom
  was_MASS_attached <- "package:MASS" %in% search()
  suppressMessages(library("MASS"))
  sims <- arm::sim(x, n.sims = n_sims)

  if (!was_MASS_attached)
    detach("package:MASS")

  as.mclist.sim.merMod(sims, x)
}

#' @describeIn as.mclist Simulate from a glmer fitted model and
#' convert to mclist
as.mclist.glmerMod <- as.mclist.merMod


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


#' @describeIn as.mclist Convert Jags or Stan simulation lists to mclist
#' @export
as.mclist.list <- function(x, ...) {
  is_mcarray <- papply(x, function(item) inherits(item, "mcarray"))
  if (all(is_mcarray))
    as.mclist.jagslist(x)

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

as.mclist.jagslist <- function(x) {
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


#' @describeIn as.mclist Convert Coda simulations to mclist
#' @export
#' @importFrom stringr str_extract str_match str_match_all str_replace_all
as.mclist.mcmc.list <- function(x, ...) {
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

  mclist <- lapply(p_indices, function(index) {
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
  names(mclist) <- unique(p_names)

  mclist
}


#' @rdname as.mclist
#' @export
is.mclist <- function(x) inherits(x, "mclist")
