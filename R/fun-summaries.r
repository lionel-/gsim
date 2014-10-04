
## Problem: omega <- (sd(a_resid) / mean(sigma_a))^2
## mean(sigma_a) is a scalar and will be expanded to an n-vector

## "[.data" <- function(gs, ...) {
##   res <- gs[...]
##   gs(res, "data")
## }

## "[.gsresult" <- function(gs, ...) {
##   res <- gs[..., ]
##   gs(res, "gsresult")
## }

## "[.posterior" <- function(gs, ...) {
##   if (is.grouped_gs(gs)) {
##     res <- gs[..., ]
##     gs  <- gs(res, "gsresult")
##   } else {
##     res <- gs[...]
##     gs  <- gs(res, "posterior")
##   }
## }


## should we recognize the array nature of gs objects and reimplement
## summarise and mutate accordingly?

## summarise_sims should work elementwise (to get posterior mean of
## covariance matrix for example), while summarise_obs takes whole
## arrays as argument?

## What about mutate?


summarise_obs <- function(gs, ...) {
  class <- gsim_class(gs)

  if (!is.data.frame(gs))
    gs <- as.data.frame(gs)

  if (class == "data")
    summarise.gs(gs, ...)
  else if (is.posterior(gs)) {
    gs %>%
      dplyr::group_by(sim, add = TRUE) %>%
      summarise.gs(...)
  }
}

summarise_sims <- function(gs, fun) {
  if (gsim_class(gs) == "data")
    stop("No simulations to summarise")

  old_attr <- attributes(first(gs))

  nsims <- length(gs)
  len <- length(first(gs))
  flat <- unlist(gs, use.names = FALSE)

  res <- vector("numeric", len)
  for (i in seq_len(len))
    res[i] <- fun(flat[seq(i, nsims * len - (len - i), by = len)])

  attributes(res) <- old_attr
  res %>%
    set_class("array")
}

## Hacky-dirty until Hadley includes programmable versions in dplyr
summarise.gs <- function(gs, ...) {
  dots <- dots_q(...)

  ## yah (yet another hack)
  exc <- vapply(attr(gs, "vars"), function(x) if (is.character(x)) x else "", character(1))
  exc <- c(exc, "sim", "obs")
  name <- colnames(gs) %>%
    setdiff(exc)

  ## Conversion of dots to first colname
  ## fixme that Ugly hack
  dots <- lapply(dots, function(x) {
    txt <- deparse(x)
    gsub("\\(\\.\\)", paste0("(", name, ")"), txt)
  })

  
  args <- paste0(names(dots), " = ",
                vapply(dots, identity, character(1)))
  args <- do.call(paste, c(args, list(sep = ", ")))

  if (!is.data.frame(gs))
    gs <- as.data.frame(gs)

  gs %>% summarise_s(args)
}



#' @export
by_sims <- function(fun, ...) {
  function(x, ...) {
    res <- apply(x, 1, fun, ...)
    gs(res, "gsresult")
  }
}

#' @export
by_obs <- function(fun, ...) {
  function(x, ...) {
    res <- apply(x, 2, fun, ...)
    gs(res, "gsresult")
  }
}


var <- function(x, ...) {
  UseMethod("var")
}

var.data <- function(gs, ...) {
  if (is.grouped_gs(gs)) {
    res <- lapply(indices(gs), function(ind) {
      stats::var(gs[ind], ...)
    })
    names(res) <- labels(gs)
    gs(res, "data", group(gs))

  } else {
    stats::var(gs, ...)
  }
}

var.gsresult <- function(gs, along = "sims", ...) {
  var_fun <-
    if (along == "sims") rowVars
    else colVars

  if (is.grouped_gs(gs)) {
    res <- lapply(indices(gs), function(ind) var_fun(gs[ind, ]))
    names(res) <- labels(gs)
  } else {
    res <- var_fun(gs)
  }

  class <-
    if (along == "sims") "data"
    else "posterior"

  gs(res, class, group = group(gs))
}


### template for future summarizing functions using apply
## var.gsresult <- function(gs, by = "sim", ...) {
##   if (by == "sim") i <- 1
##   else i <- 2

##   if (is.grouped_gs(gs)) {
##     res <- lapply(indices(gs), function(ind) {
##       apply(gs[ind, ], i, stats::var, ...)
##     })
##   } else {
##     res <- apply(gs, i, stats::var, ...)
##   }

##   gs(res, "data", group = group(gs))
## }


sd <- function(x, ...) {
  UseMethod("sd")
}

sd.default <- function(...) {
  stats::sd(...)
}

sd.gsresult <- function(gs, along = "sim") {
  if (along == "sim") i <- 1 else i <- 2
  res <- apply(gs, i, sd.default)
  gs(res, "data", group = group(gs))
}

sd.posterior <- function(gs, along = "sim") {
  if (along == "sim") i <- 1 else i <- 2
  res <- apply(gs, i, sd.default)
  gs(res, "data", group = group(gs))
}



rescale <- function(x) {
  UseMethod("rescale")
}

rescale.data <- function(gs) {
  data <- arm::rescale(gs)
  gs(data, "data", group(gs))
}



mean.gsresult <- function(gs) {
  ## if (is.null(dim(gs)) && is.grouped_gs(gs)) {
  ##   res <- data.frame(gs = gs, group = get(group(gs))) %>%
  ##     group_by(school) %>%
  ##     summarize(mean = mean) %>%
  ##     select(mean)
  ## } else {
  res <- rowMeans(gs)
  gs(res, "data", group = group(gs))
}

mean.data <- function(gs) {
  mean.default(gs)
}

mean.posterior <- function(gs) {
  if (is.grouped_gs(gs)) {
    res <- apply(gs, 1, mean)
  } else {
    res <- NextMethod(gs)
  }
  ## When we aggregate over sims and get an object which does not
  ## embed posterior uncertainty, it becomes a "data"
  gs(res, "data", group = group(gs))
}


#' @export
head.gsresult <- function(x, n = 6) {
  x[1:n, 1:5]
}

#' @export
tail.gsresult <- function(x, n = 6) {
  if (inherits(x, "grouped_gs")) {
    l <- nrow(x[[1]])
    x[(l-n):l, 1:5]
  } else {
    l <- nrow(x)
    x[(l-n):l, 1:5]
  }
}


#' @export
head.posterior <- function(x, n = 6) {
  x <- x[[sample(seq_along(x), 1)]]

  if (dim_length(x) == 2) {
    n <- min(n, nrow(x))
    n2 <- min(5, ncol(x))
    x[1:n, 1:n2, drop = FALSE]
  }
  else
    x[seq(1, min(n, length(x)))]
}

#' @export
tail.posterior <- function(x, n = 6) {
  l <- nrow(x)
  if (inherits(x, "grouped_gs")) {
    x[(l-n):l, 1:5]
  } else {
    x[(l-n)]
  }
}


#' @export
print.posterior <- function(x) {
  x <- x[[sample(seq_along(x), 1)]]
  print(x)
}
