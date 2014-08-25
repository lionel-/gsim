
as.mclist <- function(x, ...) {
  UseMethod("as.mclist")
}

as.mclist.sim.merMod <- function(sims) {
  params <- slotNames(sims)
  nsims <- first(dim(sims@fixef))

  mclist <- lapply(params, function(param) slot(sims, param)) %>%
    set_names(params) %>%
    Filter(f = function(x) !(is.null(first(x))))

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

  names(ranef) %<>% paste0("_coefs")
  mclist$ranef <- NULL
  names(mclist)[match("fixef", names(mclist))] <- "beta"
  dimnames(mclist$beta)[[2]] %<>% clean_coefnames

  c(mclist, ranef)
}


process_sims <- function(mclist) {
  browser(expr = getOption("debug_on"))

  fill_dims <- function(x) {
    dims <- dim(x)
    if (is.null(dims)) {
      dims <- c(1, length(x), 1)
    } else {
      n <- length(dims)
      if (n < 3)
        dims <- c(dims, rep(1, length.out = 3 - n))
    }
    dims
  }

  make_mcarray <- function(mcarray, names) {
    names(dim(mcarray)) <- c("", "iteration", "chain")
    dimnames(mcarray) <- list(names, NULL, NULL)
    class(mcarray) <- "mcarray" 
    mcarray
  }

  ## matrices_idx <- vapply(mclist, is.matrix, logical(1))
  ## mclist[matrices_idx]

  dims <- lapply(mclist, fill_dims)
  dim_names <- lapply(mclist, function(l) dimnames(l)[[1]])

  mclist %<>% Map(f = array, ., dims) %>%
    Map(f = make_mcarray, ., dim_names)

  class(mclist) <- c("list", "mclist")
  mclist
}


as.mclist.sim <- function(sims) {
  ## browser(expr = getOption("debug_on"))
  params <- slotNames(sims)
  mclist <- lapply(params, function(param) slot(sims, param)) %>%
    set_names(params) %>%
    Filter(f = function(x) !(is.null(first(x))))

  fill_dims <- function(x) {
    dims <- dim(x)
    if (is.null(dims)) {
      dims <- c(1, length(x), 1)
    } else {
      n <- length(dims)
      if (n < 3)
        dims <- c(dims, rep(1, length.out = 3 - n))
    }
    dims
  }

  make_mcarray <- function(mcarray, names) {
    names(dim(mcarray)) <- c("", "iteration", "chain")
    dimnames(mcarray) <- list(names, NULL, NULL)
    class(mcarray) <- "mcarray" 
    mcarray
  }

  matrices_idx <- vapply(mclist, is.matrix, logical(1))
  mclist[matrices_idx] %<>% lapply(t)

  dims <- lapply(mclist, fill_dims)
  dim_names <- lapply(mclist, function(l) dimnames(l)[[1]])

  mclist %<>% Map(f = array, ., dims) %>%
    Map(f = make_mcarray, ., dim_names)

  class(mclist) <- c("list", "mclist")
  mclist
}

as.mclist.stanfit <- function(fit) {
  require(stringr)

  # Create mclist objects from stanfits
  fit_array <- as.array(fit)
  fit_list <- fit_array %>%
    alply(3, identity) %>%
    set_names(dimnames(fit_array)$parameters)

  # Parse parameters in order to group them
  param_names <- names(fit_list)
  matched <- str_match(param_names, "([a-zA-Z_.]+)\\[([0-9]+)\\]")
  vec_names <- unique(na.omit(matched[, 2]))
  scalar_names <- param_names[is.na(matched[, 1])] %>%
    setdiff("lp__")


  make_mcarray <- function(index) {
    mcarray <- fit_array[, , index, drop = FALSE] %>%
      aperm(c(3, 1, 2))
    names(dim(mcarray)) <- c("", "iteration", "chain")
    class(mcarray) <- "mcarray" 
    mcarray
  }

  sims <- list()
  for (vec in vec_names) {
    index <- matched[, 2] == vec & !is.na(matched[, 2])
    sims[[vec]] <- make_mcarray(index)
  }
  for (scalar in scalar_names) {
    index <- param_names == scalar
    sims[[scalar]] <- make_mcarray(index)
  }
  class(sims) <- c("list", "mclist")

  sims
}


clean_coefnames <- function(names) {
  library("stringr")

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

  Map(function(p, r) names <<- str_replace(names, p, r), patterns, replacements)
  names
}
