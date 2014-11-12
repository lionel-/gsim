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
  context$reactive_lhs <- list()
  context$locked <- NULL

  ## `_enclos`$rnorm <- gen_norm

  storage <- init_storage(mclist, ...)
  storage$`_ref_stack` <- 0
  storage$`_i` <- 1


  fun <- function(x) {
    `_anchor` <- TRUE
    x <- substitute(x)

    # If quoted "{", evaluate the quote to get the "{" object
    if (try(class(eval(x)), silent = TRUE)[1] == "{")
      x <- eval(x) 

     
    # If list of names, make sure the names represent quoted "{"
    # objects, evaluate them sequentially, and return last result
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
    else if (is.language(x))
      res <- eval_statement(x, last_statement = TRUE)

    else
      stop("Unrecognized input", call. = FALSE)


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
  storage <- environment(x)$storage
  storage$`_i` <- NULL
  storage$`_ref_stack` <- NULL

  lapply(storage, head)
}

#' @export
print.gsim_fun <- function(x) {
  storage <- environment(x)$storage
  storage$`_i` <- NULL
  storage$`_ref_stack` <- NULL

  is_posterior <- vapply(storage, is.posterior, logical(1))
  n_posterior <- sum(is_posterior)
  n_data <- length(storage) - n_posterior

  cat("gsim container with", n_data, "variables and", n_posterior, "parameters\n")
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
