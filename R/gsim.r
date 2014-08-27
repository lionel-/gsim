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

  enclos_env$..eval_env.. <- new.env(parent = asNamespace("gsim"))
  ## Map(function(fun, name) assign(name, fun, envir = enclos_env$eval_env), utils, names(utils))

  enclos_env$..eval_env..$input <- gsim_process(data, mclist, groups) %>% flatten

  enclos_env$..eval_env..$`<-` <- function(a, b) assign(deparse(substitute(a)), b, envir = enclos_env$..eval_env..)
  enclos_env$..eval_env..$list <- list_gs
  ## enclos_e..nv$eval_env$rbind.numeric <- rbind.data
  ## enclos_e..nv$eval_env$cbind.numeric <- cbind.data
  enclos_env$..eval_env..$rnorm <- gen_norm
  enclos_env$..eval_env..$`$.data` <- subset_data
  enclos_env$..eval_env..$`$.posterior` <- subset_posterior


  fnu <- function(x) {
    x <- substitute(x)

    # todo, rewrite comment
    # todo, try what happens when same object exists in globenv

    ## When passed a name, it is evaluated in globenv. Need to
    ## substitute it and evaluate it in env$input.

    ## Then, should only accept quotes and "{" objects. All other
    ## things should be evalled in env$input. Because user may have two
    ## objects identically named in the global env and env$input. We
    ## want to operate only on the object located in env$input.

    eval_curly <- function(x) {
      x <- x[seq(2, length(x))]
      res <- lapply(x, function(x) eval(x, ..eval_env..$input, ..eval_env..))
      res[[length(res)]]
    }

    ## If user passes a quoted "{", evaluate the quote to get the "{" object
    if (is_curly <- try(class(eval(x)), silent = TRUE)[1] == "{")
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
    else if (class(x) == "{")
      res <- eval_curly(x)
    
    ## Could be a name or a function (such as assignment)
    else if (is.language(x)) {
      convert_out <- TRUE
      res <- eval(x, ..eval_env..$input, ..eval_env..)
    }
      

    else stop("uh oh")


    if (inherits(res, "AsIs"))
      class(res) <- setdiff(class(res), "AsIs")
    else {
      res <-
        if (is.seq_gs(res))
          as.function(res)
        else
          as.data.frame(res)
    }

    invisible(res)
  }

  environment(fnu) <- enclos_env
  invisible(fnu)
}
