#' gsim.
#'
#' @name gsim
#' @docType package
#' @import dplyr tidyr stringr ensurer lazyeval


#' @export
gsim <- function(mclist, data = NULL, groups = NULL, sequences_nsims = 100) {
  if (!is.mclist(mclist))
    mclist <- as.mclist(mclist)

  nsims <- dim(mclist[[1]])[1]
  if (sequences_nsims == "all" || sequences_nsims > nsims)
    sequences_nsims <- nsims

  enclos_env <- new.env(parent = asNamespace("gsim"))
  ## Map(function(fun, name) assign(name, fun, envir = enclos_env$eval_env), utils, names(utils))

  enclos_env$..n.. <- nrow(data)
  enclos_env$..nsims.. <- nsims
  enclos_env$..seq_index.. <- sample(seq_len(enclos_env$..nsims..), sequences_nsims)

  enclos_env$`<-` <- function(a, b) assign(deparse(substitute(a)), b, envir = enclos_env$input)
  enclos_env$ones <- ones_
  enclos_env$rbind <- rbind.gs
  enclos_env$cbind <- cbind.gs
  enclos_env$rnorm <- gen_norm
  enclos_env$`$.data` <- subset_col.data
  enclos_env$`$.posterior` <- subset_col.posterior
  enclos_env$`%%` <- subset_block_nonstd
  enclos_env$`[.gs` <- subset.gs

  enclos_env$input <- gsim_process(mclist, data, groups) %>% flatten %>% list2env(parent = enclos_env)

  
  fun <- function(x) {
    ..gsim_container.. <- TRUE
    x <- substitute(x)

    eval_statement <- function(x) {
      eval(x, enclos_env$input)
    }
    ## With this enclosing environment, ..n.., ..nsims.. and `input` can
    ## be looked up dynamically. todo: still needed?
    ## environment(eval_statement) <- enclos_env

    eval_curly <- function(x) {
      x <- x[seq(2, length(x))]
      res <- lapply(x, eval_statement)
      res[[length(res)]]
    }


    ## When passed a name, it is evaluated in globenv. Need to
    ## substitute it and evaluate it in env$input.

    ## Then, should only accept quotes and "{" objects. All other
    ## things should be evalled in env$input. Because user may have two
    ## objects identically named in the global env and env$input. We
    ## want to operate only on the object located in env$input.


    ## If user passes a quoted "{", evaluate the quote to get the "{" object
    if (try(class(eval(x)), silent = TRUE)[1] == "{") {
      beep(3)
      x <- eval(x) 
    }
     
    ## When user passes a list of names, make sure the names represent
    ## quoted "{" objects, evaluate them sequentially, and return last
    ## result
    if (class(x) == "call" && x[[1]] == "list") {
      beep(5)
      args <- as.list(x[-1])
      is_curly <- vapply(args, function(arg)
        class(arg %>% as.character %>% get) == "{", logical(1))
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
      ## beep(7)
      res <- eval_curly(x)
    }
    
    ## Could be a name or a function (such as assignment: `<-`)
    else if (is.language(x)) {
      ## beep(2)
      res <- eval_statement(x)
    }


    if (inherits(res, c("AsIs", "matrix", "array")))
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

  
  class(fun) <- c("gsim_fun", "function")
  invisible(fun)
}


#' @export
summary.gsim_fun <- function(x) {
  env <- environment(x)$enclos_env
  input <- as.list(env$input)

  lapply(input, head)
}

#' @export
print.gsim_fun <- function(x) {
  env <- environment(x)$enclos_env
  input <- as.list(env$input)

  is_posterior <- vapply(input, is.posterior, logical(1))
  n_posterior <- sum(is_posterior)
  n_data <- length(input) - n_posterior

  cat("gsim container with", n_data, "variables and", n_posterior, "parameters\n")
}
