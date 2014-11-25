
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
        env = env,
        n = n
      ))

    n <- n + 1
    env <- parent.frame(n)
  }

  stop(paste("dyn_get cannot find", sQuote(obj)), call. = FALSE)
}


container_env <- function() {
  env <- dyn_get_("_anchor")$env
  parent.env(env)
}

calling_env <- function() {
  n <- dyn_get_("_anchor")$n
  env <- parent.frame(n + 1)

  # Needed for testthat. Maybe in other circumstances as well?
  if (identical(ls(env), c("enclos", "envir", "expr")))
    env$envir
  else
    env
}

context <- function(object = NULL) {
  env <- container_env()
  if (is.null(object))
    env$context
  else
    env$context[[object]]
}

context_getter <- function(object) {
  function(element = NULL) {
    obj <- context(object)

    if (is.null(element))
      obj
    else
      obj[[match(element, obj)]]
  }
}


storage <- function(object = NULL) {
  env <- container_env()

  if (is.null(object))
    env$storage
  else {
    out <- env$storage[[object]]

    # Get object in calling environment if not found in
    # storage. Null2 occurs if last statement was an assignment
    if (is.null(out))
      tryCatch(get(object, envir = calling_env()), error =
        function(c) stop(c$message, call. = FALSE))
    else if (is.null2(out))
      NULL
    else
      out
  }
}

assign_in_context <- function(object, value) {
  env <- container_env()
  env$context[[object]] <- value
  NULL
}

eval_in_storage <- function(x) {
  env <- container_env()
  gsim <- list2env(as.list(asNamespace("gsim")), parent = calling_env())
  context <- list2env(env$context, parent = gsim)
  storage <- list2env(env$storage, parent = context)

  res <- eval(x, storage)
  env$storage <- as.list(storage)
  res
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

stack_error <- function(c) {
  clear_call_stack()
  stop(c$message, call. = FALSE)
}
