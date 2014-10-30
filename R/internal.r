

## Look up objects dynamically through the calling stack
dyn_get <- function(obj) {
  res <- .dyn_get(obj)
  res$object
}

.dyn_get <- function(obj) {
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
  container_env <- .dyn_get("_gsim_container")$env
  env <- parent.env(container_env)$`_enclos_env`
  env
}

input_env <- function() {
  env <- container_env()
  env$input
}

metadata_getter <- function(obj) {
  function() {
    env <- container_env()
    get(obj, envir = env)
  }
}

n <- get_n <- metadata_getter("_n")
nsims <- get_nsims <- metadata_getter("_nsims")
seq_index <- metadata_getter("_seq_index")


eval_in_input <- function(x) {
  eval(x, input_env())
}

assign_in_input <- function(a, b) {
  env <- container_env()
  assign(a, b, envir = env$input)
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

dots <- function(..., simplify = FALSE) {
  args <- list(...)
  
  # Transform numeric values to datas
  args <- lapply(args, function(arg) {
    if (is.numeric(arg))
      gs(arg, "data")
    else
      arg
  })

  if (simplify)
    simplify_list(args)
  else
    args
}

simplify_list <- function(list) {
  stopifnot(list %>% is.list)
  if (length(list) == 1)
    first(list)
  else
    list
}


left_join.posterior <- function(x, y, by = NULL, ...) {
  x %<>% set_class("data.frame", append = TRUE)
  y %<>% set_class("data.frame", append = TRUE)
  left_join(tbl_df(x), y, by = by, ...)
} 


quickdf <- function(x) {
  x %>%
    set_class("data.frame") %>%
    set_attr("row.names", .set_row_names(length(x[[1]])))
}


do_naked <- function(x, expr) {
  old_attr <- attributes(x)
  attributes(x) <- NULL
  x <- eval(substitute(expr))
  attributes(x) <- old_attr
  x
}

do_unclassed <- function(x, expr) {
  old_class <- class(x)
  class(x) <- NULL
  x <- eval(substitute(expr))
  class(x) <- old_class
  x
}
