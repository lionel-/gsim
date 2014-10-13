

assign_in_input <- function(a, b) {
  env <- container_env()
  assign(a, b, envir = env$input)
}


call_args <- function(x) as.list(x)[-1]
is_arg_call <- function(x) vapply(call_args(x), is.call, logical(1))

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
is.delayed <- function(x) inherits(x, "delayed")
is.empty <- function(x) inherits(x, "empty")

saved_statement <- function(x) {
  structure(as.expression(x), class = "saved")
}

delayed_pickup <- function(x) {
  structure(x, class = "delayed")
}

empty_posterior <- function(x) {
  structure(NULL, class = c("empty", "posterior"))
}


# todo: not used anymore?
is_arg_posterior <- function(x) {
  x <- call_args(x)
  env <- input_env()
  vapply(x, function(item) is.posterior(eval(item, envir = env)), logical(1))
}


call_stack <- function() {
  env <- container_env()
  env$`_call_stack`
}

call_ref <- function(pos) {
  structure(pos, class = "stack_ref")
}

add_to_call_stack <- function(value) {
  value <- 
    if (is.expression(value))
      list(first(value))
    else
      list(value)
  env <- container_env()
  env$`_call_stack` <- c(env$`_call_stack`, value)
  call_ref(length(env$`_call_stack`))
}

replace_call_stack <- function(value) {
  env <- container_env()
  env$`_call_stack` <- value
  invisible(NULL)
}

clear_call_stack <- function() {
  env <- container_env()
  env$`_call_stack` <- list()
  invisible(NULL)
}



make_ref_call <- function(id) as.expression(call("[[", as.name("_stack"), id))

add_to_ref_stack <- function(x) {
  env <- input_env()
  stack <- env$`_stack`
  pos <- Position(is.null, stack, nomatch = length(stack) + 1)
  env$`_stack`[[pos]] <- x
  pos
}

clear_ref_stack <- function(x) {
  pos <- find_refs(x) %>%
    unlist(use.names = FALSE)
  env <- input_env()
  if (!is.null(pos))
    env$`_stack`[pos] <- list(NULL)
}

find_refs <- function(x) {
  if (is.atomic(x) || is.name(x))
    NULL

  else if (is.call(x)) {
    if (identical(x[[1]], quote(`[[`)) &&
        identical(x[[2]], as.name("_stack")))
      x[[3]]
    else
      lapply(x, find_refs)
  }

  else stop("`find_refs` is lost", call. = FALSE)
}


args_finder <- function(predicate, fun_name) {
  function(x) {
    if (is.name(x))
      if (predicate(get_in_input(deparse(x))))
        deparse(x)
      else
        NULL

    else if (is.call(x))
      unlist(lapply(x, match.fun(fun_name)), use.names = FALSE)

    else if (is.atomic(x))
      NULL

    else stop(paste0("`", fun_name, "` is lost"), call. = FALSE)
  }
}

find_posterior_args <- args_finder(is.posterior, "find_posterior_args")
find_data_args <- args_finder(is.data, "find_data_args")

find_posterior_pos <- function(x) {
  recurser <- function(x, pos = 1) {
    if (is.name(x))
      if (is.posterior(get_in_input(deparse(x))))
        pos
      else
        NULL

    else if (is.call(x))
      Map(recurser, x, Map(c, list(pos), as.list(seq_along(x))))

    else if (is.atomic(x))
      NULL

    else stop(paste0("`find_posterior_pos` is lost"), call. = FALSE)
  }

  recurser(x) %>%
    compact %>%
    flatten
}


make_alist <- function(args) {
  res <- replicate(length(args), substitute())
  names(res) <- args
  res
}
## process_call <- function(index, object) {
##   temp <- vector("list", length(index) + 1)
##   temp[[1]] <- object

##   for (i in seq_along(index))
##     temp[[i+1]] <- `[[`(temp[[i]], index[i])
##   last(temp) <- call("pick_sim", last(temp), quote(i))

##   for (i in rev(seq_along(index)))
##     temp[[i]][[index[i]]] <- temp[[i+1]]
##   temp[[1]]
## }

eval_first <- function(expr) {
  assign("_i", 1, envir = input_env())
  eval_in_input(expr)
}


wrap_posterior <- function(x, dims) {
  call <- do.call(call, c(list("[", x, quote(`_i`)), replicate(length(dims) - 1, substitute())), quote = TRUE)
  # call$drop <- FALSE
  call
}

process_call <- function(x) {
  recurser <- function(x) {
    if (is.name(x)) {
      obj <- get_in_input(deparse(x))

      if (is.posterior(obj))
        wrap_posterior(x, dim(obj))
      else
        x
    }

    else if (is.call(x)) {
      # Get dimensions of rhs to construct the lhs subsetting
      if (class(x) == "<-") {
        lhs <- x[[2]]
        lhs_obj <- get_in_input(deparse(lhs))

        if (is.empty(lhs_obj)) {
          rhs <- first(Map(recurser, x[-c(1, 2)]))
          rhs_obj <- eval_first(rhs)

          assign_in_input(deparse(lhs), init_posterior(rhs_obj))
          return(do.call(call, c(list("<-", recurser(x[[2]])), rhs), quote = TRUE))
        }
      }

      as.call(Map(recurser, x))
    }

    else if (is.atomic(x))
      x

    else stop(paste0("`process_call` is lost"), call. = FALSE)
  }

  recurser(x)
}


eval_in_input <- function(x) {
  eval(x, input_env())
}

eval_curly <- function(x) {
  last <- x[[length(x)]]
  others <- x[seq(2, length(x) - 1)]
  lapply(others, eval_statement)
  eval_statement(last, eval_posterior = TRUE)
}

eval_assignment <- function(lhs, rhs) {
  if (is.call(rhs))
    rhs <- eval_substatement(rhs)

  # Save delayed eval to stack call. Then assign ref to the call
  res <- NULL
  rhs <- 
    if (is.saved(rhs)) {
      res <- rhs
      empty_posterior()
    }
    else if (is.name(rhs))
      get_in_input(rhs)
    else
      gs(rhs, "data")

  assign_in_input(lhs, rhs)
  res
}

eval_args <- function(x) {
  which_call <- which(c(FALSE, is_arg_call(x)))

  for (i in which_call) {
    res <- eval_substatement(x[[i]])

    x[i] <- 
      if (is.saved(res))
        res
      else {
        index <- add_to_ref_stack(res)
        make_ref_call(index)
      }
  }

  x
}

eval_substatement <- function(x) {
  if (any(is_arg_call(x)))
    x <- eval_args(x)

  if (is.name(x))
    delayed_pickup(x)
  else if (!is.null(find_posterior_args(x)))
    # Cannot save in call_stack here: may be part of larger expression
    saved_statement(x)
  else {
    res <- eval_in_input(x)
    clear_ref_stack(x)
    res
  }
}


eval_statement <- function(x, eval_posterior = FALSE) {
  # If not assignment and not last statement, ignore the statement. We
  # assume the user does not rely on any side-effects in its
  # statements.
  if (is.assignment(x)) {
    lhs <- deparse(lhs(x))
    x <- eval_assignment(lhs, rhs(x))

    if (is.saved(x))
      add_to_call_stack(call("<-", as.name(lhs), first(x)))
    x <- NULL
  }

  if (eval_posterior) {
    if (!is.null(x)) {
      x <- eval_substatement(x)
      if (is.saved(x))                  # `_eval`
        add_to_call_stack(x)
    }

    if (!length(call_stack()) == 0) {

      # Note: relatively expensive
      stack <- lapply(call_stack(), process_call)

      ## inner_body <- do.call(call, c(list("{"), quote(browser(expr = getOption("debug_on"))), stack), quote = TRUE)
      inner_body <- do.call(call, c(list("{"), stack), quote = TRUE)
      call_args <- list("for", quote(`_i`), bquote(seq(1, .(nsims()))), inner_body)
      loop <- do.call(call, call_args, quote = TRUE)
      eval(loop, envir = input_env())

      browser(expr = getOption("debug_on"))
      clear_call_stack()
      clear_ref_stack(loop)

      if (is.delayed(x))
        get_in_input(deparse(x))
      else
        todo("last eval")
    }

    else x
  }

  else NULL
}
