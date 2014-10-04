

## ## todo: named arguments
## cbind.data <- function(...) {
##   dots <- extract_dots(...)

##   ## Handling sequences
##   seq_p <- vapply(dots, is.seq_gs, logical(1))
##   if (any(seq_p))
##     return(do.call("seq_operate", c(dots, list(fun = cbind.data))))

##   names <- vapply(dots, get_name, character(1)) %>%
##     make_names_unique

##   dots %>%
##     set_names(names) %>%
##     convert_numeric %>%
##     list_gs %>%
##     ensure_same_length
## }


cbind.gs_seq <- function(...) {
  stop()
}

## cbind.posterior <- function(...) {
##   args <- extract_dots(...)
##   if (!is.list_gs(args))
##     args <- concatenate(args)

##   res <- Reduce(cbind, args, NULL)
##   class(res) <- c("posterior", "matrix_gs")
##   res
## }



rbind.data <- function(...) {
  dots <- dots(...)

  seq_p <- vapply(dots, is.seq_gs, logical(1))
  if (any(seq_p))
    return(do.call("seq_operate", c(dots, list(fun = rbind.data))))

  do.call("c", dots) %>%
    convert_numeric
}


rbind.posterior <- function(...) {
  dots <- dots(...)

  seq_p <- vapply(dots, is.seq_gs, logical(1))
  if (any(seq_p))
    return(do.call(seq_operate, c(dots, list(fun = rbind.posterior))))

  do.call(cbind, lapply(dots, unclass))
}


rbind_cols <- function(x, ...) UseMethod("rbind_cols")

rbind_cols.data <- function(object) {
  if (!dim_length(object) == 2)
    stop("Can only rbind the cols of a matrix")

  if (dim(object)[2] == 1)
    return(object)

  ncols <- ncol(object)
  nrows <- nrow(object)
  res <- object %>%
    array(c(nrows * ncols, 1)) %>%
    gs("data")

  attr(res, "blocks_names") <- colnames(object) %||% rep("", ncols)
  attr(res, "blocks_indices") <- c(0, cumsum(rep(nrows, ncols)))
  attr(res, "dimnames") <- NULL
  colnames(res) <- last(names(dimnames(object)))
  res
}

rbind_cols.matrix <- rbind_cols.data

rbind_cols.posterior <- function(object) {
  do_by_sims(object, fun = rbind_cols.data) 
}

cbind_blocks <- function(x) UseMethod("cbind_blocks")

cbind_blocks.data <- function(object) {
  if (!dim_length(object) == 2)
    stop("Can only cbind the blocks of a matrix or a column vector")

  if (dim(object)[2] > 1)
    browser(expr = getOption("debug_on"))

  dims <- dim(object)
  blocks <- attr(object, "blocks_names")
  indices <- attr(object, "blocks_indices")
  gap <- indices[2]

  res <- array(object, c(gap, length(blocks) * last(dims)))
  colnames(res) <- blocks
  res
}

cbind_blocks.posterior <-
  function(object) do_by_sims(object, cbind_blocks(.$object))



as.data.frame.matrix_gs <- function(gs, ...) {
  if (is.data(gs)) {
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
