#' gsim.
#'
#' @name gsim
#' @docType package
#' @import dplyr stringr

## tools <- list(
##   `+.gs`,
##   `-.gs`,
##   `*.gs`,
##   `/.gs`,  
##   `^.gs`,
##   `%*%`
## )

#' @export
gsim <- function(data, mclist, groups = NULL) {
  env <- new.env(parent = asNamespace("gsim"))

  env$sim <- gsim_process(data, mclist, groups) %>%
    flatten

  env$..n.. <- nrow(data)
  env$..nsims.. <- dim(mclist[[1]])["iteration"]

  env$`<-` <- function(a, b) assign(deparse(substitute(a)), b, envir = parent.frame(2)$enclos) 
  env$c <- vec
  env$cbind <- cvec


  function(x) {

    # todo, rewrite comment
    ## When passed a name, it is evaluated in globenv. Need to
    ## substitute it and evaluate it in env$sim.

    ## Then, should only accept quotes and "{" objects. All other
    ## things should be evalled in env$sim. Because user may have two
    ## objects identically named in the global env and env$sim. We
    ## want to operate only on the object located in env$sim.

    x <- substitute(x)
    is_defined <- exists(deparse(x))

    if (is_defined && (class(eval(x)) == "{" || is.language(eval(x)))) {
      x <- eval(x)

      if (class(x) == "{") {

        ## When user passes a quoted list as argument, evaluate them
        ## sequentially and return last result
        x <- x[seq(2, length(x))]
        res <- lapply(x, function(x) eval(x, env$sim, env))
        res <- res[length(res)]

      } else if (is.language(x)) {
        res <- eval(x, env$sim, env)

      } else stop("uh oh")

    ## If x yielded an error, evaluate it inside env$sim
    } else {
      res <- eval(substitute(x), env$sim, env)
    }

    invisible(res)
  }
}
