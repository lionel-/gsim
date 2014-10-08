

assign_in_input <- function(a, b) {
  container_env <- gsim_env()
  assign(a, b, envir = container_env$input)
}

call_args <- function(x) as.list(x)[-1]
lhs <- function(x) x[[2]]
rhs <- function(x) x[[3]]

`lhs<-` <- function(x, value) {
  x[[2]] <- value
  x
}

`rhs<-` <- function(x, value) {
  x[[3]] <- value
  x
}

is.assignment <- function(x) is.call(x) && class(x) == "<-"
is.saved <- function(x) inherits(x, "saved")
is_arg_call <- function(x) vapply(call_args(x), is.call, logical(1))
make_stack_call <- function(id) as.expression(call("[[", as.name("_stack"), id))  

is_arg_posterior <- function(x) {
  x <- call_args(x)
  env <- input_env()
  vapply(x, function(item) is.posterior(eval(item, envir = env)), logical(1))
}

add_to_stack <- function(x) {
  env <- input_env()
  stack <- env$`_stack`
  pos <- Position(is.null, stack, nomatch = length(stack) + 1)
  env$`_stack`[[pos]] <- x
  pos
}

clear_stack_refs <- function(x) {
  pos <- find_stack_refs(x) %>%
    unlist(use.names = FALSE)
  env <- input_env()
  if (!is.null(pos))
    env$`_stack`[pos] <- list(NULL)
}

find_stack_refs <- function(x) {
  if (is.atomic(x) || is.name(x))
    NULL

  else if (is.call(x)) {
    if (identical(x[[1]], quote(`[[`)) &&
        identical(x[[2]], as.name("_stack")))
      x[[3]]
    else
      lapply(x, find_stack_refs)
  }

  else stop("`find_stack_refs` is lost", call. = FALSE)
}

eval_assignment <- function(lhs, rhs) {
  if (is.call(rhs))
    rhs <- eval_statement(rhs)

  if (is.saved(rhs)) {
    # Save to association list, return name
  }

  else if (is.name(rhs)) {
    rhs <- get(rhs, envir = input_env())
    assign_in_input(lhs, rhs)
    rhs
  }

  else {
    assign_in_input(lhs, rhs)
    rhs
  }
}


eval_args <- function(x) {
  which_call <- which(c(FALSE, is_arg_call(x)))

  for (i in which_call) {
    res <- eval_statement(x[[i]])

    x[i] <- 
      if (is.language(res))
        as.expression(res)        ## as.saved?
      else {
        index <- add_to_stack(res)
        make_stack_call(index)
      }
  }

  x
}


eval_statement <- function(x) {
  ## browser(expr = getOption("debug_on"))
  if (is.name(x))
    get_in_input(deparse(x))

  else if (is.assignment(x)) {
    lhs <- deparse(lhs(x))
    eval_assignment(lhs, rhs(x))
  }

  else {
    if (any(is_arg_call(x)))
      x <- eval_args(x)

    if (any(is_arg_posterior(x)))
      as.expression(x)              ## as.saved?
    else {
      container_env <- gsim_env()
      res <- eval(x, container_env$input)
      clear_stack_refs(x)
      res
    }
  }
}


eval_curly <- function(x) {
  x <- x[seq(2, length(x))]
  res <- lapply(x, eval_statement)
  res[[length(res)]]
}
