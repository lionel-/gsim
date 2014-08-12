
#' @export
is.vec_gs <- function(x) inherits(x, "vec_gs")

#' @export
is.matrix_gs <- function(x) inherits(x, "matrix_gs")


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
  # todo: named arguments
  args <- extract_dots(...)
  if (!is.vec_gs(args))
    args <- vec(args)

  res <- Reduce(cbind, args, NULL)
  class(res) <- c("gsvar", "matrix_gs")

  colnames(res) <- vapply(args, name, character(1)) %>%
    make_names_unique

  res
}

#' @export
cvec.gsparam <- function(...) {
  args <- extract_dots(...)
  if (!is.vec_gs(args))
    args <- vec(args)

  res <- Reduce(cbind, args, NULL)
  class(res) <- c("gsparam", "matrix_gs")
  res
}


as.data.frame.vec_gs <- function(gs, ...) {
  if (is.gsparam(gs)) {
    res <- as.data.frame.list(gs) %>%
      set_names(names(gs))

    if (is.grouped_gs(gs)) {
      group <- group(gs)
      nsims <- get("..nsims..", parent.env(parent.frame(2))$env)

      res <- gather_(res, group, name(gs), names(gs)) %>%
        regroup(list(group)) %>%
        mutate(sim = seq_len(nsims))
      res[group] <- factor(res[[group]], sort(unique(as.character(res[[group]]))))
    }

  } else if (is.gsvar(gs)) {
    res <- as.data.frame.list(gs) %>%
      gather_(group(gs), name(gs), seq_len(length(gs)))
  }

  res
}

as.data.frame.matrix_gs <- function(gs, ...) {
  if (is.gsvar(gs)) {
    as.data.frame.matrix(gs, ...)
  }
}


#' @export
`[.vec_gs` <- function(x, ...) {
  class <- class(x)
  class(x) <- "list"
  x <- x[...]
  class(x) <- class
  x
}
