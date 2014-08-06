
#' @export
is.gsvec      <- function(x) inherits(x, "gsvec")

#' @export
is.bound_gs    <- function(x) inherits(x, "bound_gs")


## #' @param ... objects to concatenate
#' @export
vec <- function(...) {
  args <- extract_dots(...)
  class <- gsim_class(args[[1]])

  same_type <- lapply(args, gsim_class) == class
  stopifnot(all(same_type))

  gs(args, class, nobs = get_n(2), nsims = get_nsims(2))
}


cvec <- function(...) {
  UseMethod("cvec")
}

#' @export
cvec.gsvar <- function(...) {
  args <- extract_dots(...)
  if (!is.gsvec(args))
    args <- vec(args)

  res <- Reduce(cbind, args, NULL)
  class(res) <- c("gsvar", "bound_gs")
  res
}

#' @export
cvec.gsparam <- function(...) {
  args <- extract_dots(...)
  if (!is.gsvec(args))
    args <- vec(args)

  res <- Reduce(cbind, args, NULL)
  class(res) <- c("gsparam", "bound_gs")
  res
}
