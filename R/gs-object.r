

gs <- function(object, class, group = NULL, name = NULL, nobs = NULL, nsims = NULL) {
  ## if (name == "size") browser()
  ## todo: check that the structure of gs is right
  assert_that(class %in% c("gsvar", "gsparam", "gsresult"))

  # gsresults are the only legit matrices in gsim
  if (is.matrix(object) && !class == "gsresult") {
    names <- dimnames(object)[[1]]
    if (is.null(names)) names <- list(NULL)

    object %<>% apply(1, list)
    object <- Map(function(obj, ...) gs(obj[[1]], ...),
                 object, class = class, name = names, nobs = nobs, nsims = nsims)

  } else {
    object <- structure(
      object,
      group = group,
      name = name,
      nobs = nobs,
      nsims = nsims
    )
  }

  object <- set_gs_class(object, class, grouped = !is.null(group))
  object
}


#' @export
is.gs         <- function(x) inherits(x, "gs")

#' @export
is.grouped_gs <- function(x) inherits(x, "grouped_gs")

#' @export
is.gsvar      <- function(x) inherits(x, "gsvar")

#' @export
is.gsparam    <- function(x) inherits(x, "gsparam")

#' @export
is.gsresult   <- function(x) inherits(x, "gsresult")

#' @export
is_any.gsresult <- function(...) {
  any(vapply(list(...), is.gsresult, logical(1)))
}


## Todo?: replace set_gs_class with gsim_class<-

#' @export
gsim_class <- function(x) {
  if (is.gsparam(x)) return("gsparam")
  else if (is.gsvar(x)) return("gsvar")
  else if (is.gsresult(x)) return("gsresult")
  else if (is.numeric(x)) return("numeric")
  else stop("Not a gsim object")
}

set_gs_class <- function(gs, class, grouped = NULL) {
  class(gs) <- c("gs", class)
  if (grouped) {
    class(gs) <- c(class(gs), "grouped_gs")
  }
  if (is.list(gs)) {
    class(gs) <- c(class(gs), "gsvec")
  }
  gs
}


gsim_group <- function(gs) attr(gs, "group")

set_group <- function(gs, group) {
  attr(gs, "group") <- group
  gs
}

get_gs_group <- function(data, groups, type) {
  names_by_group <- lapply(groups, "[[", type)

  get_group <- function(x) {
    res <- NULL
    for (group in names(names_by_group)) {
      temp <- if (x %in% names_by_group[[group]]) group else NULL
      res <- c(res, temp)
    }
    assert_that(is.null(res) || length(res) == 1)
    res
  }

  lapply(names(data), get_group)
}


#' @export
`[.gsvec` <- function(x, ...) {
  class <- class(x)
  class(x) <- "list"
  x <- x[...]
  class(x) <- class
  x
}
