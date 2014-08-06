
operator <- function(fun) {
  function(a, b) {

    ## Need to remove gs class to provoke method dispatch later on.
    ## NextMethod does not work because it evaluates directly with
    ## fun, whereas we need a functional of fun. We would need S4
    ## classes to get the name of the next method and feed it to the
    ## functional.
    class(a) <- setdiff(class(a), "gs")
    class(b) <- setdiff(class(b), "gs")

    ## ## Make gsim operations compatible with numeric objects
    ## ## Maybe not necessary since attributes rework
    ## if (is.numeric(a)) {
    ##   a <- gs(a, "gsvar")
    ## } else if (is.numeric(b)) {
    ##   b <- gs(b, "gsvar")
    ## }

    classes  <- c(gsim_class(a), gsim_class(b))

    is_a_grouped <- inherits(a, "grouped_gs")
    is_b_grouped <- inherits(b, "grouped_gs")

    a_group <- gsim_group(a)
    b_group <- gsim_group(b)

    if (length(a_group) > 1 || length(b_group) > 1) {
      stop("Todo: One of the objects belongs to more than one group")
    } else if (!any(c(is.null(a_group), is.null(b_group))) &&
               a_group != b_group) {
      stop("Objects belong to different groups")
    }

    if (is_a_grouped != is_b_grouped) {
      ## Objects loose their grouped character when combined with
      ## ungrouped objects
      result <- operate_on_gs(a, b, fun)
    } else if (is_a_grouped && is_b_grouped) {
      result <- operate_on_grouped_gs(a, b, fun)
    } else {
      result <- operate_on_gs(a, b, fun)
    }

    result
  }
}

operate_on_gs <- function(a, b, fun) {
  class_a <- gsim_class(a)
  class_b <- gsim_class(b)

  flatten <- function(list) {
    if (is.list(list) && length(list) == 1)
      list <- list[[1]]
    list
  }
  a <- flatten(a)
  b <- flatten(b)

  ## Maybe work this out through should_expand to prevent expanding
  ## when the grouped object is operated with a scalar
  is_any.scalar <- function(...) {
    any(vapply(list(...), function(x) {
      is.null(dim(x)) && length(x) == 1
    }, logical(1)))
  }

  if (is_any.scalar(a, b)) {
    result <- fun(a, b)
    result <- gs(result, "gsvar")
    return(result)
  }

  should_expand <- function(x) is.grouped_gs(x) && (is.gsvar(x) || is.gsresult(x))
  if (should_expand(a)) {
    a <- expand_grouped_gs(a)
  } else if (should_expand(b)) {
    b <- expand_grouped_gs(b)
  }


  if (class_a == class_b) {
    if (is.grouped_gs(a) && is.gsparam(a)) {
      a <- expand_grouped_gs(a)
    } else if (is.grouped_gs(b) && is.gsparam(b)) {
      b <- expand_grouped_gs(b)
    }
    result <- fun(a, b)
    result <- gs(result, class_a)
    return(result)
  }

  ## Todo: clean up this mess
  if (class_a == "gsvar" && class_b == "gsparam") {
    ## Using outer is more efficient but does not work with grouped
    ## gsparam
    if (is.grouped_gs(b)) {
      b <- expand_grouped_gs(b)
      result <- fun(a, b)
    } else {
      result <- outer(a, b, fun)
    }
  } else if (class_a == "gsparam" && class_b == "gsvar") {
    if (is.grouped_gs(a)) {
      a <- expand_grouped_gs(a)
      result <- fun(b, a)
    } else {
      result <- outer(b, a, fun)
    }
  } else if (class_a == "gsparam" && class_b == "gsresult") {
    if (inherits(a, "grouped_gs")) {
      a <- expand_grouped_gs(a)
    }
    result <- fun(a, b)
  } else if (class_a == "gsresult" && class_b == "gsparam") {
    if (is.grouped_gs(b)) {
      b <- expand_grouped_gs(b)
    }
    result <- fun(a, b)
  } else if (class_a == "gsvar" && class_b == "gsresult") {
    result <- apply(b, 2, fun, a)
  } else if (class_a == "gsresult" && class_b == "gsvar") {
    result <- apply(a, 2, fun, b)
  } else if (class_a == "numeric") {
    result <- fun(a + b)
  } else if (class_b == "numeric") {
    result <- fun(a + b)
  } else stop(paste("Class of a:", class_a, "\nClass of b:", class_b))

## browser()
  gs(result, "gsresult")
}

expand_grouped_gs <- function(gs) {
  assert_that(!is.null(gsim_group(gs)))
  if (is.gsvar(gs)) {
    gs <- gs[get(gsim_group(gs))]
  } else {
    gs <- gs[get(gsim_group(gs)), ]
  }

  gs
}


operate_on_grouped_gs <- function(a, b, fun) {
  classes <- c(gsim_class(a), gsim_class(b))

  if (classes[1] == classes[2]) {
    result <- operate_on_identically_grouped(a, b, fun)
  } else if (all(classes %in% c("gsvar", "gsparam"))) {
    result <- operate_on_grouped_param(a, b, fun)
  } else if (all(classes %in% c("gsvar", "gsresult"))) {
    result <- operate_on_grouped_var_result(a, b, fun)
  } else if (all(classes %in% c("gsparam", "gsresult"))) {
    result <- operate_on_grouped_param(a, b, fun)
  } else stop("Unknown objects")

  result
}

operate_on_identically_grouped <- function(a, b, fun) {
  class <- gsim_class(a)
  result <- fun(a, b)

  gs(result, class, group = gsim_group(a))
}

operate_on_grouped_param <- function(a, b, fun) {

  if (!is.grouped_gs(a) || !is.grouped_gs(b)) {
    if (gsim_class(a) == "gsvar") {
      var   <- a
      param <- expand_grouped_gs(b)
    } else {
      var   <- b
      param <- expand_grouped_gs(a)
    }
    result <- fun(param, var)
  } else if (is.grouped_gs(a) && is.grouped_gs(b)) {
    if (is_any.gsresult(a, b)) {
      ## Problem: Recognize between varying parameters, with dimension
      ## (ngroup, niter), and constant parameters, with dimension niter.

      if (is.gsresult(a) && is.null(dim(b))) {
        ## assert_that(length(b) == b$properties$niter)
        param  <- b
        result <- a
        result <- sweep(result, 2, param, fun)
      } else if (is.gsresult(b) && is.null(dim(a))) {
        ## assert_that(length(a) == a$properties$niter)
        param  <- a
        result <- b
        result <- sweep(result, 2, param, fun)
      } else if (is.gsresult(b) && length(dim(a)) == 2) {
        ## assert_that(dim(a) == c(ngroups, niter))
        param  <- a
        result <- b
        result <- fun(param, result)
      }

    } else if (gsim_class(a) == "gsvar") {
      var   <- a
      param <- b
      result <- outer(var, param, fun)
    } else {
      var   <- b
      param <- a
      result <- outer(var, param, fun)
    }
  } else {
    warning("Verify this")
    result <- fun(a, b)
  }

  gs(result, "gsresult", group = gsim_group(a))
}

operate_on_grouped_var_result <- function(a, b, fun) {
  result <- fun(a, b)
  gs(result, "gsresult", group = gsim_group(a))
}


"+.gs" <- operator(`+`)
"-.gs" <- operator(`-`)
"*.gs" <- operator(`*`)
"/.gs" <- operator(`/`)
"^.gs" <- operator(`^`)


"%*%" <- function(x, y) {
  nobs <- get_n(1)
  nsims <- get_nsims(1)
  res <- array(NA, c(nsims, nobs))

  if (!is.bound_gs(x) && is.gsvec(x)) x <- cvec(x)
  if (!is.bound_gs(y) && is.gsvec(y)) y <- cvec(y)

  if (is.gsparam(x)) {
    for (i in seq(nsims)) {
      res[i, ] <- base::`%*%`(x[i, ], y)
    }
  } else if (is.gsparam(y)) {
    for (i in seq(nsims)) {
      res[i, ] <- base::`%*%`(x, y[i, ])
    }
  } else {
    res <- x %*% y
  }

  t(res) %>%
    gs("gsresult", nobs = nobs, nsims = nsims)
}
