

as.mclist <- function(x, ...) {
  UseMethod("as.mclist")
}

as.mclist.stanfit <- function(x) {
  if (!requireNamespace("rstan", quietly = TRUE))
    stop("Package `rstan` is not installed", call. = FALSE)

  out <- rstan::extract(x)
  class(out) <- "mclist"
  out
}

as.mclist.sim.merMod <- function(sims) {
  mclist <- process_arm_sims(sims)
  nsims <- first(dim(sims@fixef))

  ranef <- lapply(mclist$ranef, function(x) {
    npar <- dim(x)[3]
    x <- lapply(seq_len(npar), function(i) x[, , i]) %>%
      set_names(dimnames(x)[[3]])

    nobs <- dim(first(x))[2]
    res <- do.call(c, x)
    res <- do.call(array, list(res, c(nsims, nobs, length(x))))
    dimnames(res) <- list(NULL, NULL, names(x) %>% clean_coefnames)
    res
  })

  ranef <- set_list_dimnames(ranef)
  names(ranef) <- paste0(names(ranef), "_coefs")
  
  mclist$ranef <- NULL
  names(mclist)[match("fixef", names(mclist))] <- "beta"
  dimnames(mclist$beta)[[2]] <- clean_coefnames(dimnames(mclist$beta)[[2]])
  mclist <- set_list_dimnames(mclist)

  c(mclist, ranef)
}

as.mclist.sim <- function(sims) {
  mclist <- process_arm_sims(sims)
  names(mclist)[match("coef", names(mclist))] <- "beta"
  dimnames(mclist$beta)[[2]] <- clean_coefnames(dimnames(mclist$beta)[[2]])
  class(mclist) <- "mclist"
  mclist
}


process_arm_sims <- function(sims) {
  params <- slotNames(sims)

  lapply(params, function(param) slot(sims, param)) %>%
    set_names(params) %>%
    Filter(f = function(x) !(is.null(first(x))))
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
    names <<- stringr::str_replace(names, p, r)
  }, patterns, replacements)
  names
}


is.mclist <- function(x) inherits(x, "mclist")
