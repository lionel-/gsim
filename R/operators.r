# todo: binary op with matrix_gs and gs

## Function factory for binary element-wise operators 
operator <- function(fun) {
  function(...) {
    if (any(vapply(list(...), is.seq_gs, logical(1))))
      seq_operate(..., fun = function(...) operate(..., fun = fun))
    else
    operate(..., fun = fun)
  }
}


sanitize <- function(object, dim_other) {
  if (is.posterior(object))
    return(do_by_sims(object, fun = sanitize))


  if (length(dim(object)) == 2 && all(dim(object) == c(1, 1)))
    object <- as.vector(object)
  
  if (inherits(object, "numeric"))
    class(object) <- "data"

  ## Need to remove gs class to provoke method dispatch later on.
  ## NextMethod does not work because it evaluates directly with fun,
  ## while we need it to evaluate with the returned functional of
  ## fun. We would need S4 classes to get the name of the next method
  ## and feed it to the functional.
  class(object) <- setdiff(class(object), "gs")

  object
}


operate <- function(..., fun) {
  dots <- dots(...) %>%
    lapply(sanitize)

  if (all(vapply(dots, function(x) gsim_class(x) == "data", logical(1))))
    do.call("fun", dots) %>% gs("data")
  else
    do.call("do_by_sims", c(dots, list(fun = fun)))
}


do_by_sims <- function(..., fun, args = NULL) {
  dots <- dots(...) %>%
    lapply(function(x) {
      if (is.data(x))
        list(x)
      else
        x
    })

  ## Small perf advantage for lapply for unary mappings
  res <- 
    if (length(dots) == 1)
      do.call("lapply", c(list(FUN = fun), dots, args))
    else
      do.call("Map", c(list(f = fun), dots, args))

  res %>%
    set_gsim_attributes("posterior")
}


"+.gs" <- operator(`+`)
"-.gs" <- operator(`-`)
"*.gs" <- operator(`*`)
"/.gs" <- operator(`/`)
"^.gs" <- operator(`^`)
"%*%.gs" <- operator(`%*%`)
"cbind.gs" <- operator(cbind)


`%*%` <- function(x, y) UseMethod("%*%")
`%*%.default` <- function(x, y) base::`%*%`(x, y)

