#' Gorgeous manipulations of SIMulations
#'
#' @name gsim-package
#' @docType package
#' @importFrom magrittr %>% %$%
NULL

# Debugging flags
if (!exists("pass1", envir = globalenv()))
  pass1 <- FALSE
if (!exists("pass2", envir = globalenv()))
  pass2 <- FALSE


#' gsim container
#'
#' Create a container holding simulations and data.
#'
#' These containers are the workhorses of gsim: they execute operations
#' on simulations and data and return the result.
#'
#' \enumerate{
#'   \item Variables assigned in a statement will be saved inside the
#'         container and can be reused in later calls.
#'   \item Operations involving parameters are transparently
#'         executed over all simulations.
#'   \item The last statement is invisibly returned and can be
#'         assigned outside the container. The returned object is
#'         coerced as a data.frame by default. This can be overridden
#'         locally by wrapping the last statement with \code{I()}, or
#'         globally by setting \code{tidy_output} to FALSE when
#'         creating the container.
#' }
#' @seealso \code{\link{reactive}} Use a container to create a
#' reactive function. \code{\link{as.mclist}} The compatibility of
#' gsim with many simulations data structures is powered by
#' \code{as.mclist} coertions schemes.
#' @param sims Simulations from Stan, Jags, CODA, or arm::sim.
#' Alternatively, fitted models to be simulated from using arm::sim
#' @param ... Lists, data.frames or numeric vectors or arrays
#' containing the data.
#' @return A gsim container. This is a function that can be used to execute
#' statements.
#' @examples
#' # Create a gsim container from a fitted glm (uses arm::sim
#' # internally, with \code{n_sims} = 500 simulations)
#' wells <- fetch_wells_data()
#' wells_fit <- glm(switched ~ c_dist100 * c_arsenic, binomial, wells)
#' sims <- gsim(wells_fit, wells, n_sims = 500)
#'
#'
#' # The purpose of gsim containers is to execute operations. There are
#' # many ways of supplying statements to a gsim container.
#' 
#' # Directly, with one statement:
#' null <- gsim(beta_100 <- 100 * beta)  # Assign beta_100 in container but returns nothing
#' beta_100_df <- gsim(beta_100)         # Returns beta_100 in a data.frame
#' beta_100 <- gsim(I(beta_100))         # Returns beta_100 as an array
#'
#' # Directly, with several statements:
#' fitted <- gsim({
#'   X <- cbind(intercept(), c_dist100, c_arsenic, c_dist100 * c_arsenic)
#'   fitted <- invlogit(X %*% col_vector(beta))
#'   fitted
#' })
#'
#' # Indirectly, with one saved statement:
#' saved_op <- quote(beta_100 <- beta / 100)
#' gsim(saved_op)
#'
#' # Indirectly, with several saved statements:
#' fitted_ops <- quote({
#'   X <- cbind(intercept(), c_dist100, c_arsenic, c_dist100 * c_arsenic)
#'   fitted <- invlogit(X %*% col_vector(beta))
#' })
#' null <- sims(fitted_ops)  # Assign X and fitted, returns nothing
#' fitted <- sims(fitted)    # Returns fitted in a data.frame
#'
#'
#' # When a name refers both to a saved statement and to a variables
#' # inside the gsim container, the latter has predecence
#' fitted <- quote(beta)
#' sims(fitted)      # Returns fitted, not beta
#'
#' # The following returns beta since fitted does not exist in sims2
#' sims2 <- gsim(wells_fit, wells)
#' fitted <- sims2(fitted)  
#'
#'
#' # All these ways of executing operations can be combined by adding
#' # arguments. The following assigns beta_100, X, fitted, and returns
#' # fitted as an array.
#' fitted <- sims2(beta100 <- beta - 100, saved_op, fitted_ops, I(fitted))
#'
#'
#' # To get arrays instead of data.frames by default, create a
#' # container with \code{tidy_output} = FALSE.
#' sims <- gsim(wells_fit, wells, tidy_output = FALSE)
#' beta <- sims(beta)  # returns beta as an array
#' @export
gsim <- function(sims, ..., n_sims = 100, tidy_output = TRUE) {
  if (!is.mclist(sims))
    sims <- as.mclist(sims, n_sims = n_sims)

  context <- list()
  context$n_sims <- dim(sims[[1]])[1]
  context$ref_stack <- 0
  context$call_stack <- list()
  context$reactive_stack <- list()
  context$reactive_lhs_list <- list()
  context$locked <- NULL
  context$tidy_output <- tidy_output
  context <- c(context, overrides)

  storage <- init_storage(sims, ...)
  storage$`_i` <- 1

  fun <- container
  environment(fun) <- environment()
  class(fun) <- c("gsim_container", "function")
  invisible(fun)
}

decant <- gsim

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
