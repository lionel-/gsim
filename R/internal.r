

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

  stop(paste("Cannot find", obj))
}


container_env <- function() {
  container_env <- dyn_get_("_gsim_container")$env
  env <- parent.env(container_env)$`_enclos_env`
  env
}

input_env <- function() {
  env <- container_env()
  env$`_input`
}

metadata_getter <- function(obj) {
  function() {
    env <- container_env()
    get(obj, envir = env)
  }
}

nsims <- metadata_getter("_nsims")


eval_in_input <- function(x) {
  eval(x, input_env())
}

assign_in_input <- function(a, b) {
  env <- input_env()
  assign(a, b, envir = env)
}

check_in_input <- function(x, predicate) {
  obj <- get_in_input(deparse(x))

  if (predicate(obj))
    x
  else
    NULL
}

get_in_input <- function(what) {
  get(what, envir = input_env())
}


as.gsarray <- function(x) {
  if (is.array(x))
    x
  else {
    nm <- names(x)
    ind <- seq(0, length(x))
    x <- array(x, c(length(x), 1))
    attr(x, "blocks_names") <- nm
    attr(x, "blocks_indices") <- ind
    x
  }
}


is.col_vector <- function(x) (dim(x)[2] == 1) %||% FALSE

dots_q <- function(...) eval(substitute(alist(...)))

quickdf <- function(x) {
  x %>%
    set_class("data.frame") %>%
    set_attr("row.names", .set_row_names(length(x[[1]])))
}
