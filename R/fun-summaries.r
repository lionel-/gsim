
## Problem: omega <- (sd(a_resid) / mean(sigma_a))^2
## mean(sigma_a) is a scalar and will be expanded to an n-vector

## "[.gsvar" <- function(gs, ...) {
##   res <- gs[...]
##   gs(res, "gsvar")
## }

## "[.gsresult" <- function(gs, ...) {
##   res <- gs[..., ]
##   gs(res, "gsresult")
## }

## "[.gsparam" <- function(gs, ...) {
##   if (is.grouped_gs(gs)) {
##     res <- gs[..., ]
##     gs  <- gs(res, "gsresult")
##   } else {
##     res <- gs[...]
##     gs  <- gs(res, "gsparam")
##   }
## }


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

var.gsvar <- function(gs, ...) {
  if (is.grouped_gs(gs)) {
    res <- lapply(indices(gs), function(ind) {
      stats::var(gs[ind], ...)
    })
    names(res) <- labels(gs)
    gs(res, "gsvar", group(gs))

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
    if (along == "sims") "gsvar"
    else "gsparam"

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

##   gs(res, "gsvar", group = group(gs))
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
  gs(res, "gsvar", group = group(gs))
}

sd.gsparam <- function(gs, along = "sim") {
  if (along == "sim") i <- 1 else i <- 2
  res <- apply(gs, i, sd.default)
  gs(res, "gsvar", group = group(gs))
}



rescale <- function(x) {
  UseMethod("rescale")
}

rescale.gsvar <- function(gs) {
  data <- arm::rescale(gs)
  gs(data, "gsvar", group(gs))
}



mean.gsresult <- function(gs) {
  ## if (is.null(dim(gs)) && is.grouped_gs(gs)) {
  ##   res <- data.frame(gs = gs, group = get(group(gs))) %>%
  ##     group_by(school) %>%
  ##     summarize(mean = mean) %>%
  ##     select(mean)
  ## } else {
  res <- rowMeans(gs)
  gs(res, "gsvar", group = group(gs))
}

mean.gsvar <- function(gs) {
  mean.default(gs)
}

mean.gsparam <- function(gs) {
  if (is.grouped_gs(gs)) {
    res <- apply(gs, 1, mean)
  } else {
    res <- NextMethod(gs)
  }
  ## When we aggregate over sims and get an object which does not
  ## embed posterior uncertainty, it becomes a "gsvar"
  gs(res, "gsvar", group = group(gs))
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
head.gsparam <- function(x, n = 6) {
  if (inherits(x, "grouped_gs") || inherits(x, "matrix_gs")) {
    x[1:n, 1:5]
  } else {
    x[1:n]
  }
}

#' @export
tail.gsparam <- function(x, n = 6) {
  l <- nrow(x)
  if (inherits(x, "grouped_gs")) {
    x[(l-n):l, 1:5]
  } else {
    x[(l-n)]
  }
}

as.data.frame.gs <- function(x, ...) as.data.frame.numeric(x, ...)


