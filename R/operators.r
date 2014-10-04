

operator <- function(fun) {
  function(...) {
    is_any_seq <- any(vapply(list(...), is.seq_gs, logical(1)))
    if (is_any_seq)
      seq_operate(..., fun = function(...) operate(..., fun = fun))
    else
      operate(..., fun = fun)
  }
}


sanitize <- function(object, dim_other) {
  if (is.posterior(object)) {
    res <- do_by_sims(object, fun = function(x) {
      if (length(dim(x)) == 2 &&
          all(dim(x) == c(1, 1)))
        x <- as.vector(x)

      ## Speeds up computation (no need of `data` dispatch at this point)
      x %>% set_class(NULL)
    })

    class(res) <- "posterior"
    return(res)
  }


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
  dots <- dots(...) %>% lapply(sanitize)
  all_data <- all(vapply(dots, function(x) gsim_class(x) == "data", logical(1)))

  if (all_data)
    do.call("fun", dots) %>% gs("data")
  else
    do.call("do_by_sims", c(dots, list(fun = fun)))
}

do_by_sims <- function(..., fun, args = NULL) {
  ## Normally the enclosing environment gets passed along with the
  ## promises contained in dots. This sometimes does not happen when
  ## `do_by_sims` is called directly within a vsim object. The
  ## following is a workaround for this situation. Also, sometimes
  ## lazy_dots can not follow symbols and will fail, while usual
  ## methods of capturing dots work...

  try_dots <- try({
    dots <- lazy_dots(..., .follow_symbols = TRUE)
    env <- input_env()
    dots <- dots %>% pluck("expr") %>% lapply(eval, envir = env)
    dots
  }, silent = TRUE)

  if (class(try_dots) == "try-error")
    dots <- dots(...)

  dots <- lapply(dots, function(x) {
    if (is.data(x))
      list(x %>% set_class("numeric"))
    else
      x
  })


  ## Workaround for weird dispatching bug: if fun is a S3 generic, it
  ## won't find the method. Defining a local copycat resolves
  ## this. (???) See note in vapply help page?

  ## Note: tried to change the enclosing env of fun to the execution
  ## env of do_by_sims (`environment(fun) <- environment()`), but it
  ## didn't work.
  fnu <- function(...) fun(...)


  ## Small perf advantage for lapply for unary mappings
  res <- 
    if (length(dots) == 1)
      do.call("lapply", c(list(FUN = fnu), dots, args))
    else
      do.call("Map", c(list(f = fnu), dots, args))

  res %>%
    set_gsim_attributes("posterior")
}


make_beep <- function(fun) {
  function(...) {
    library("beepr")
    beep()
    fun(...)
  }
}

`%*%` <- function(x, y) UseMethod("%*%")
`%*%.default` <- function(x, y) base::`%*%`(x, y)

`+.gs` <- operator(`+`)
`-.gs` <- operator(`-`)
`*.gs` <- operator(`*`)
`/.gs` <- operator(`/`)
`^.gs` <- operator(`^`)
`cbind.gs` <- operator(cbind)
`rbind.gs` <- operator(rbind)
`%*%.gs` <- operator(`%*%`)
`subset.gs` <- operator(`[`)
inv_logit <- operator(ilogit)

