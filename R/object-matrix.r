
`_rbind_cols` <- function(object) {
  if (!dim_length(object) == 2)
    stop("Can only rbind the cols of a matrix")

  if (dim(object)[2] == 1)
    return(object)

  ncols <- ncol(object)
  nrows <- nrow(object)
  res <- array(object, c(nrows * ncols, 1))

  attr(res, "blocks_names") <- colnames(object) %||% rep("", ncols)
  attr(res, "blocks_indices") <- c(0, cumsum(rep(nrows, ncols)))
  attr(res, "dimnames") <- NULL
  colnames(res) <- last(names(dimnames(object)))
  res
}


`_cbind_blocks` <- function(object) {
  if (!dim_length(object) == 2)
    stop("Can only cbind the blocks of a matrix or a column vector")

  if (dim(object)[2] > 1)
    stop("todo")

  dims <- dim(object)
  blocks <- attr(object, "blocks_names")
  indices <- attr(object, "blocks_indices")
  gap <- indices[2]

  res <- array(object, c(gap, length(blocks) * last(dims)))
  colnames(res) <- blocks
  res
}
