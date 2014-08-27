
as.mclist <- function(x, ...) {
  UseMethod("as.mclist")
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

  names(ranef) %<>% paste0("_coefs")
  mclist$ranef <- NULL
  names(mclist)[match("fixef", names(mclist))] <- "beta"
  dimnames(mclist$beta)[[2]] %<>% clean_coefnames

  c(mclist, ranef)
}

as.mclist.sim <- function(sims) {
  mclist <- process_arm_sims(sims)
  names(mclist)[match("coef", names(mclist))] <- "beta"
  dimnames(mclist$beta)[[2]] %<>% clean_coefnames
  mclist
}

process_arm_sims <- function(sims) {
  params <- slotNames(sims)

  lapply(params, function(param) slot(sims, param)) %>%
    set_names(params) %>%
    Filter(f = function(x) !(is.null(first(x))))
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


## fixme; temporary fix until new arm is released
setMethod("sim", signature(object = "lm"),
    function(object, n.sims=100) {
    object.class <- class(object)[[1]]
    summ <- summary (object)
    coef <- summ$coef[,1:2,drop=FALSE]
    dimnames(coef)[[2]] <- c("coef.est","coef.sd")
    sigma.hat <- summ$sigma
    beta.hat <- coef[,1,drop = FALSE]
    V.beta <- summ$cov.unscaled
    n <- summ$df[1] + summ$df[2]
    k <- summ$df[1]
    sigma <- rep (NA, n.sims)
    beta <- array (NA, c(n.sims,k))
    dimnames(beta) <- list (NULL, clean_coefnames(rownames(beta.hat))) # Replace names by rownames
    for (s in 1:n.sims){
      sigma[s] <- sigma.hat*sqrt((n-k)/rchisq(1,n-k))
      beta[s,] <- mvrnorm (1, beta.hat, V.beta*sigma[s]^2)
    }
    
    ans <- new("sim", 
                coef = beta,
                sigma = sigma)
    return (ans)
    }
)
