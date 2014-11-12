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
  nsims <- dim(mclist[[1]])[1]

  # To test if we can go with lists.
  `_enclos_env` <- new.env()
  ## `_enclos_env` <- new.env(parent = asNamespace("gsim"))

  `_enclos_env`$`_nsims` <- nsims
  `_enclos_env`$`_call_stack` <- list()
  `_enclos_env`$`_reactive_stack` <- list()
  `_enclos_env`$`_reactive_lhs` <- list()
  `_enclos_env`$`_locked` <- NULL

  `_enclos_env`$rnorm <- gen_norm

  `_enclos_env`$`_input` <- input_process(mclist, ...) %>%
    list2env(parent = `_enclos_env`)

  `_enclos_env`$`_input`$`_ref_stack` <- 0
  `_enclos_env`$`_input`$`_i` <- 1


  fun <- function(x) {
    `_gsim_container` <- TRUE
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
  env <- environment(x)$`_enclos_env`
  input <- as.list(env$`_input`)
  input$`_i` <- NULL
  input$`_ref_stack` <- NULL

  lapply(input, head)
}

#' @export
print.gsim_fun <- function(x) {
  env <- environment(x)$`_enclos_env`
  input <- as.list(env$`_input`)
  input$`_i` <- NULL
  input$`_ref_stack` <- NULL

  is_posterior <- vapply(input, is.posterior, logical(1))
  n_posterior <- sum(is_posterior)
  n_data <- length(input) - n_posterior

  cat("gsim container with", n_data, "variables and", n_posterior, "parameters\n")
}


input_process <- function(mclist, ...) {
  dots <- list(...)
  data <- 
    if (length(dots) == 0)
      NULL
    else if (length(dots) == 1)
      dots[[1]]
    else
      stop("Multiple data inputs not implemented yet")

  mclist <- Map(gs, mclist, class = "posterior")
  if (!is.null(data))
    data <- Map(gs, data, class = "data")

  c(data, mclist)
}
