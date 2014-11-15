#' gsim.
#'
#' @name gsim
#' @docType package
#' @importFrom magrittr %>% %$%
# Note, maybe @rdname to document unexported functions?

#' @export
gsim <- function(mclist, ...) {
  if (!is.mclist(mclist))
    mclist <- as.mclist(mclist)

  context <- list()
  context$nsims <- dim(mclist[[1]])[1]
  context$call_stack <- list()
  context$reactive_stack <- list()
  context$reactive_lhs_list <- list()
  context$locked <- NULL

  context$list <- `_list`
  ## `_enclos`$rnorm <- gen_norm

  storage <- init_storage(mclist, ...)
  storage$`_ref_stack` <- 0
  storage$`_i` <- 1

  fun <- container
  environment(fun) <- environment()
  class(fun) <- c("gsim_fun", "function")
  invisible(fun)
}

init_storage <- function(mclist, ...) {
  dots <- list(...)
  data <- 
    if (length(dots) == 0)
      NULL
    else if (length(dots) == 1)
      dots[[1]]
    else
      stop("Multiple data inputs not implemented yet")

  mclist <- Map(posterior, mclist)
  c(data, mclist)
}
