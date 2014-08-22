
extract_dots <- function(..., simplify = FALSE) {
  args <- list(...)
  
  # Transform numeric values to gsvars
  args <- lapply(args, function(arg) {
    if (is.numeric(arg))
      gs(arg, "gsvar")
    else
      arg
  })

  if (simplify)
    simplify_list(args)
  else
    args
}


simplify_list <- function(list) {
  if (length(list) == 1) list[[1]]
  else list
}


convert_numeric <- function(x) {
  UseMethod("convert_numeric")
}

convert_numeric.default <- identity

convert_numeric.numeric <- function(x) {
  gs(x, "gsvar")
}

convert_numeric.list <- function(list) {
  lapply(list, function(item)
    if (is.numeric(item))
      gs(item, "gsvar")
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


extract <- magrittr::extract


single <- function(x) {
  stopifnot(length(x) == 1)
  first(x)
}



is.scalar <- function(x) gsim_class(x) == "numeric" && is.null(dim(x)) && length(x) == 1
is_any.scalar <- function(...) any(vapply(list(...), is.scalar, logical(1)))
