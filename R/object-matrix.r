

bind_cols <- function(...) {
  UseMethod("bind_cols")
}

## todo: named arguments
bind_cols.gsvar <- function(...) {
  dots <- extract_dots(...)

  ## Handling sequences
  pos <- vapply(dots, is.seq_gs, logical(1))
  if (any(pos))
    return(seq_operate_variadic(dots, pos, bind_cols.gsvar))

  names <- vapply(dots, get_name, character(1)) %>%
    make_names_unique

  dots %>%
    set_names(names) %>%
    convert_numeric %>%
    make_list %>%
    ensure_same_length
}

bind_cols.numeric <- bind_cols.gsvar

bind_cols.gs_seq <- function(...) {
  browser()

  test <- list(...)

  gs
}

bind_cols.gsparam <- function(...) {
  args <- extract_dots(...)
  if (!is.list_gs(args))
    args <- concatenate(args)

  res <- Reduce(cbind, args, NULL)
  class(res) <- c("gsparam", "matrix_gs")
  res
}



bind_rows <- function(...) {
  UseMethod("bind_rows")
}

bind_rows.gsvar <- function(...) {
  dots <- extract_dots(...)

  ## Handling sequences
  pos <- vapply(dots, is.seq_gs, logical(1))
  if (any(pos))
    return(seq_operate_variadic(dots, pos, bind_cols.gsvar))

  do.call("c", dots) %>%
    convert_numeric
}

bind_rows.numeric <- bind_rows.gsvar



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
