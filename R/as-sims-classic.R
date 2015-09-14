# @describeIn as_sims
# @export
as_sims.sim <- function(x, sims_major = NULL, ...) {
  sims <- process_arm_sims(x)
  names(sims)[match("coef", names(sims))] <- "beta"
  names(sims)[match("zeta", names(sims))] <- "z_eta"
  dimnames(sims$beta)[[2]] <- clean_coefnames(dimnames(sims$beta)[[2]])
  class(sims) <- "sims"
  sims
}

# @export
as_sims.sim.polr <- function(x, sims_major = NULL, ...) {
  suppressMessages(as_sims.sim(x, ...))
}

# @describeIn as_sims
# to mclist
# @export
as_sims.lm <- function(x, sims_major = NULL, n_sims = 100, ...) {
  check_packages("arm", "MASS")

  # FIXME: until arm puts mvrnorm in importFrom
  was_MASS_attached <- "package:MASS" %in% search()
  suppressMessages(library("MASS"))
  sims <- arm::sim(x, n.sims = n_sims)

  if (!was_MASS_attached)
    detach("package:MASS")

  as_sims.sim(sims)
}

# @describeIn as_sims
# @export
as_sims.glm <- as_sims.lm

# @describeIn as_sims
# @export
as_sims.polr <- as_sims.lm

process_arm_sims <- function(sims) {
  p_names <- slotNames(sims)

  params <- lapply(p_names, function(param) slot(sims, param))
  names(params) <- p_names
  Filter(f = function(x) !(is.null(first(x))), params)
}

# @importMethodsFrom arm fitted
# @export
as_sims.sim.merMod <- function(x, sims_major = NULL, obj = NULL, ...) {
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

# @describeIn as_sims
# @export
as_sims.merMod <- function(x, sims_major = NULL, n_sims = 100, ...) {
  check_packages("arm", "MASS")

  # FIXME: until arm puts mvrnorm in importFrom
  was_MASS_attached <- "package:MASS" %in% search()
  suppressMessages(library("MASS"))
  sims <- arm::sim(x, n.sims = n_sims)

  if (!was_MASS_attached)
    detach("package:MASS")

  as_sims.sim.merMod(sims, x)
}

# @describeIn as_sims
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
