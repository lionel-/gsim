

## Look up objects dynamically through the calling stack
dyn_get <- function(obj) {
  res <- dyn_get_(obj)
  res$object
}

dyn_get_ <- function(obj) {
  n <- 1
  env <- parent.frame(1)

  while(!identical(env, globalenv())) {
    if (exists(obj, envir = env, inherits = FALSE))
      return(list(
        object = get(obj, envir = env),
        env = env
      ))

    n <- n + 1
    env <- parent.frame(n)
  }

  stop(paste("dyn_get cannot find", sQuote(obj)), call. = FALSE)
}


container_env <- function() {
  browser(expr = getOption("debug_on"))
  env <- dyn_get_("_anchor")$env
  parent.env(env)
}

context <- function(object = NULL) {
  env <- container_env()
  if (is.null(object))
    env$context
  else
    env$context$object
}

storage <- function(object = NULL) {
  env <- container_env()
  if (is.null(object))
    env$storage
  else
    env$storage[[object]]
}

## nsims <- function() {
##   context()$nsims
## }


eval_in_storage <- function(x) {
  context <- list2env(context(), parent = asNamespace("gsim"))
  eval(x, storage(), context)
}

assign_in_storage <- function(object, value) {
  env <- container_env()
  env$storage[[object]] <- value
  NULL
}

check_in_storage <- function(x, predicate) {
  obj <- get_in_storage(deparse(x))

  if (predicate(obj))
    x
  else
    NULL
}

get_in_storage <- function(what) {
  storage()[[what]]
}


is.col_vector <- function(x) (dim(x)[2] == 1) %||% FALSE

dots_q <- function(...) eval(substitute(alist(...)))

quickdf <- function(x) {
  x %>%
    set_class("data.frame") %>%
    set_attr("row.names", .set_row_names(length(x[[1]])))
}
