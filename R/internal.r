
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

