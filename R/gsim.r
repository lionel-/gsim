#' gsim.
#'
#' @name gsim
#' @docType package
#' @importFrom magrittr %>% %$%
# Note, maybe @rdname to document unexported functions?

# Debugging flags
pass1 <- FALSE
pass2 <- FALSE


#' @export
gsim <- function(sims, ..., n_sims = 100) {
  if (!is.mclist(sims))
    sims <- as.mclist(sims, n_sims = n_sims)

  context <- list()
  context$nsims <- dim(sims[[1]])[1]
  context$call_stack <- list()
  context$reactive_stack <- list()
  context$reactive_lhs_list <- list()
  context$locked <- NULL

  context$list <- `_list`

  storage <- init_storage(sims, ...)
  storage$`_ref_stack` <- 0
  storage$`_i` <- 1

  fun <- container
  environment(fun) <- environment()
  class(fun) <- c("gsim_container", "function")
  invisible(fun)
}

init_storage <- function(sims, ...) {
  dots <- list(...)
  data <- 
    if (length(dots) == 0)
      NULL
    else if (length(dots) == 1)
      dots[[1]]
    else
      stop("Multiple data inputs not implemented yet")

  sims <- lapply(sims, posterior)
  c(data, sims)
}
