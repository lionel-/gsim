#' gsim.
#'
#' @name gsim
#' @docType package
#' @import dplyr tidyr stringr ensurer 

#' @export
gsim <- function(data, mclist, groups = NULL) {
  enclos_env <- new.env(parent = asNamespace("gsim"))

  enclos_env$..n.. <- nrow(data)
  enclos_env$..nsims.. <- dim(mclist[[1]])["iteration"]

  enclos_env$eval_env <- new.env(parent = asNamespace("gsim"))
  Map(function(fun, name) assign(name, fun, envir = enclos_env$eval_env), utils, names(utils))

  enclos_env$eval_env$sim <- gsim_process(data, mclist, groups) %>%
    flatten

  enclos_env$eval_env$`<-` <- function(a, b) assign(deparse(substitute(a)), b, envir = enclos_env$eval_env)
  enclos_env$eval_env$list <- list_gs
  enclos_env$eval_env$rbind.numeric <- rbind.gsvar
  enclos_env$eval_env$cbind.numeric <- cbind.gsvar
  enclos_env$eval_env$rnorm <- gen_norm


  fnu <- function(x) {
    x <- substitute(x)

    # todo, rewrite comment
    # todo, try what happens when same object exists in globenv

    ## When passed a name, it is evaluated in globenv. Need to
    ## substitute it and evaluate it in env$sim.

    ## Then, should only accept quotes and "{" objects. All other
    ## things should be evalled in env$sim. Because user may have two
    ## objects identically named in the global env and env$sim. We
    ## want to operate only on the object located in env$sim.

    eval_curly <- function(x) {
      x <- x[seq(2, length(x))]
      res <- lapply(x, function(x) eval(x, eval_env$sim, eval_env))
      res[[length(res)]]
    }


    ## If user passes a quoted "{", evaluate the quote to get the "{" object
    if (try(class(eval(x)), silent = TRUE)[1] == "{")
      x <- eval(x)

    ## When user passes a list of names, make sure the names represent
    ## quoted "{" objects, evaluate them sequentially, and return last
    ## result
    if (class(x) == "call" && x[[1]] == "list") {
      args <- as.list(x[-1])
      is_curly <- vapply(args, function(arg) class(arg %>% as.character %>% get) == "{",
                         logical(1))
      stopifnot(all(is_curly))

      res <- lapply(args, function(arg) {
        curly <- get(as.character(arg))
        eval_curly(curly)
      })
      res <- res[[length(res)]]

    }

    ## When user passes a "{" list as argument, evaluate the
    ## components sequentially and return last result
    else if (class(x) == "{") {
       res <- eval_curly(x)
    }

    ## Could be a name or a function (such as assignment)
    else if (is.language(x)) {
      res <- eval(x, eval_env$sim, eval_env)
    }

    else stop("uh oh")


    if (inherits(res, "AsIs"))
      class(res) <- setdiff(class(res), "AsIs")

    else
      res <-
        if (is.seq_gs(res))
          as.function(res)
        else
          as.data.frame(res)

    invisible(res)
  }

  environment(fnu) <- enclos_env
  invisible(fnu)
}
