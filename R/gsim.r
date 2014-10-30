#' gsim.
#'
#' @name gsim
#' @docType package
#' @import dplyr tidyr stringr ensurer lazyeval


#' @export
gsim <- function(mclist, data = NULL, groups = NULL) {
  if (!is.mclist(mclist))
    mclist <- as.mclist(mclist)
  nsims <- dim(mclist[[1]])[1]

  `_enclos_env` <- new.env(parent = asNamespace("gsim"))

  `_enclos_env`$`_n` <- nrow(data)
  `_enclos_env`$`_nsims` <- nsims
  `_enclos_env`$`_call_stack` <- list()
  `_enclos_env`$`_reactive_stack` <- list()
  `_enclos_env`$`_reactive_lhs` <- list()
  `_enclos_env`$`_locked` <- NULL

  `_enclos_env`$ones <- ones_
  `_enclos_env`$rnorm <- gen_norm

  `_enclos_env`$input <- gsim_process(mclist, data, groups) %>%
    list2env(parent = `_enclos_env`)

  `_enclos_env`$input$`_ref_stack` <- 0
  `_enclos_env`$input$`_i` <- 0


  fun <- function(x) {
    `_gsim_container` <- TRUE
    x <- substitute(x)


    # When passed a name, it is evaluated in globenv. Need to
    # substitute it and evaluate it in env$input.

    # Then, should only accept quotes and "{" objects. All other
    # things should be evalled in env$input. Because user may have two
    # objects identically named in the global env and env$input. We
    # want to operate only on the object located in env$input.


    # If user passes a quoted "{", evaluate the quote to get the "{" object
    if (try(class(eval(x)), silent = TRUE)[1] == "{") {
      ## beep(3)
      x <- eval(x) 
    }
     
    # When user passes a list of names, make sure the names represent
    # quoted "{" objects, evaluate them sequentially, and return last
    # result
    if (class(x) == "call" && x[[1]] == "list") {
      ## beep(5)
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

    # When user passes a "{" list as argument, evaluate the
    # components sequentially and return last result
    else if (class(x) == "{")
      res <- eval_curly(x)
    
    # Could be a name or a function (such as assignment: `<-`)
    else if (is.language(x)) {
      beep(2)
      res <- eval_statement(x, last_statement = TRUE)
    }


    if (inherits(res, "AsIs"))
      class(res) <- setdiff(class(res), "AsIs")

    else if (!is.null(res) && !is.reactive_fun(res))
      res <- as.data.frame(res)

    invisible(res)
  }

  
  class(fun) <- c("gsim_fun", "function")
  invisible(fun)
}


container_getter <- function(object) {
  function(element = NULL) {
    obj <- container_env()[[object]]

    if (is.null(element))
      obj
    else
      obj[[match(element, obj)]]
  }
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

