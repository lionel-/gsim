
extract_dots <- function(...) {
  args <- list(...)
  if (length(args) == 1) args[[1]]
  else args
}

