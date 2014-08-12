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
  Map(function(fun, name) assign(name, fun, envir = env), utils, names(utils))

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

    ## todo, try what happens when same object exists in globenv

    eval_curly <- function(x) {
      x <- x[seq(2, length(x))]
      res <- lapply(x, function(x) eval(x, env$sim, env))
      res[[length(x) - 1]]
    }


    x <- substitute(x)

    ## If user passes a quoted "{", evaluate the quote to get the "{" object
    if (try(class(eval(x)), silent = TRUE) == "{")
      x <- eval(x)


    ## When user passes a list of names, make sure the name represent
    ## quoted "{" objects evaluate them sequentially, and return last
    ## result
    if (class(x) == "call" && x[[1]] == "list") {
      args <- as.list(x[-1])
      all_curly <- vapply(args, function(arg) class(arg %>% as.character %>% get) == "{",
                         logical(1)) %>% all
      stopifnot(all_curly)

      res <- lapply(args, function(arg) {
        curly <- get(as.character(arg))
        eval_curly(curly)
      })
      res <- res[[length(res)]]

    ## When user passes a "{" list as argument, evaluate the
    ## components sequentially and return last result
    } else if (class(x) == "{") {
       res <- eval_curly(x)

    ## Could be a name or a function (such as assignment)
    } else if (is.language(x)) {
      res <- eval(x, env$sim, env)

    } else stop("uh oh")


    if (!inherits(res, "AsIs"))
      res <- as.data.frame(res)
    else
      class(res) <- setdiff(class(res), "AsIs")

    invisible(res)
  }
}
