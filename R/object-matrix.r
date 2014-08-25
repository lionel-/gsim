

## todo: named arguments
cbind.gsvar <- function(...) {
  dots <- extract_dots(...)

  ## Handling sequences
  seq_p <- vapply(dots, is.seq_gs, logical(1))
  if (any(seq_p))
    return(do.call("seq_operate", c(dots, list(fun = cbind.gsvar))))

  names <- vapply(dots, get_name, character(1)) %>%
    make_names_unique

  dots %>%
    set_names(names) %>%
    convert_numeric %>%
    list_gs %>%
    ensure_same_length
}


cbind.gs_seq <- function(...) {
  stop()
}

cbind.gsparam <- function(...) {
  args <- extract_dots(...)
  if (!is.list_gs(args))
    args <- concatenate(args)

  res <- Reduce(cbind, args, NULL)
  class(res) <- c("gsparam", "matrix_gs")
  res
}



rbind.gsvar <- function(...) {
  dots <- extract_dots(...)

  seq_p <- vapply(dots, is.seq_gs, logical(1))
  if (any(seq_p))
    return(do.call("seq_operate", c(dots, list(fun = rbind.gsvar))))

  do.call("c", dots) %>%
    convert_numeric
}


rbind.gsparam <- function(...) {
  dots <- extract_dots(...)

  seq_p <- vapply(dots, is.seq_gs, logical(1))
  if (any(seq_p))
    return(do.call(seq_operate, c(dots, list(fun = rbind.gsparam))))

  do.call(cbind, lapply(dots, unclass))
}



rbind_cols <- function(list_gs) {
  res <- do.call("rbind", list_gs)

  attr(res, "blocks_names") <- last(dimnames(res))
  ## Todo: non constant gaps
  ##       objects of dim > 1
  gap <- dim(first(list_gs)) %>% extract(length(.)) %||% 1
  attr(res, "blocks_indices") <- cumsum(rep(gap, length(list_gs)))
  attr(res, "dimnames") <- NULL
  res
}



as.data.frame.matrix_gs <- function(gs, ...) {
  if (is.gsvar(gs)) {
    as.data.frame.matrix(gs, ...)
  }
}


#' @export
is.matrix_gs <- function(x) inherits(x, "matrix_gs")


as.matrix.list_gs <- function(x) {
  old_class <- c("gs", gsim_class(x))
  old_name <- name(x)

  (do.call(cbind, x) %>%
    set_class(old_class) %>%
    set_name(old_name))
}
