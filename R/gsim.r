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
  context <- c(context, overrides)

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

  if (length(dots) > 0) {
    is_numeric <- papply(dots, is.numeric)
    dots[is_numeric] <- lapply(dots[is_numeric], list)
    dots <- do.call(c, dots)

    is_unnamed <- (names(dots) == "") %||% TRUE
    if (any(is_unnamed))
      warning("Some input elements are not named. They will not be visible in the container",
        call. = FALSE)

    is_conflict <- names(dots) %in% names(sims)
    if (any(is_conflict))
      stop(paste("There is a conflict between the names of data and parameters:",
        names(dots)[is_conflict]), call. = FALSE)
  }

  sims <- lapply(sims, posterior)
  c(dots, sims)
}
