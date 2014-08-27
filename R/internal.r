
## Look up objects dynamically through the calling stack
dyn_get <- function(obj) {
  n <- 1
  env <- parent.frame(n)

  while(!identical(env, globalenv())) {
    if (exists(obj, envir = env))
      return(get(obj, envir = env))

    n <- n + 1
    env <- parent.frame(n)
  }

  stop(paste("Cannot find", obj))
}

get_metadata <- function(obj) {
  function() dyn_get(obj)
}

n <- get_n <- get_metadata("..n..")
nsims <- get_nsims <- get_metadata("..nsims..")


get_in_input <- function(what) {
  get(what, envir = list2env(dyn_get("..eval_env..")$input))
}


convert_numeric <- function(x) {
  UseMethod("convert_numeric")
}

convert_numeric.default <- identity

convert_numeric.numeric <- function(x) {
  gs(x, "data")
}

convert_numeric.list <- function(list) {
  lapply(list, function(item)
    if (is.numeric(item))
      gs(item, "data")
    else
      item)
}

convert_numeric.list_gs <- convert_numeric.list



ensure_same_length <-
  ensuring(all(lapply(., length) == length(.[[1]]))) 



safe_rep.int <- failwith(NULL, f = base::rep.int, quiet = TRUE)


set_attr <- `attr<-`


## From https://gist.github.com/skranz/9681509
dplyr_hack_eval <- function(.data, fun_name, ...) {
  fun_name <- paste0("dplyr::", fun_name)
  args <- list(...) %>% unlist
  code <- paste0(fun_name, "(.data,", paste0(args, collapse = ","), ")")
  eval(parse(text = code, srcfile = NULL))
}

arrange_s <- function(.data, ...) dplyr_hack_eval(.data, "arrange", ...)
summarise_s <- function(.data, ...) dplyr_hack_eval(.data, "summarise", ...)


## fixme; Avoid conflict with tidyr
extract <- magrittr::extract


single <- function(x) {
  stopifnot(length(x) == 1)
  first(x)
}



is.scalar <- function(x) gsim_class(x) == "numeric" && is.null(dim(x)) && length(x) == 1
is_any.scalar <- function(...) any(vapply(list(...), is.scalar, logical(1)))


## From Hadley
`%||%` <- function(a, b) {
  if (!is.null(a) && !is.na(a) && length(a) > 0)
    a
  else
    b
}


dim_length <- function(x) length(dim(x))


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
