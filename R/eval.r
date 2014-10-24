

eval_in_input <- function(x) {
  eval(x, input_env())
}

assign_in_input <- function(a, b) {
  env <- container_env()
  assign(a, b, envir = env$input)
}

check_in_input <- function(x, predicate) {
  obj <- get_in_input(deparse(x))

  if (predicate(obj))
    x
  else
    NULL
}


call_args <- function(x) as.list(x)[-1]
is_arg_call <- function(x) vapply(call_args(x), is.call, logical(1))

is.empty <- function(x) inherits(x, "empty")
is.assignment <- function(x) is.call(x) && identical(first(x), as.name("<-"))
is.to_loop <- function(x) inherits(x, "to_loop")
is.delayed <- function(x) inherits(x, "delayed")
is.vectorized <- function(x) attr(x, "vectorized") %||% FALSE


empty_posterior <- function(x) {
  structure(NULL, class = c("empty", "posterior"))
}

to_loop <- function(x, subclass = NULL, vectorized = NULL, ...) {
  if (!is.null(vectorized) && !class(x) == "(")
    x <- call("(", x)
  structure(
    x,
    class = c("to_loop", subclass),
    vectorized = vectorized,
    ...
  )
}

delayed_pickup <- function(x) {
  structure(x, class = "delayed")
}

vectorized <- function(x) {
  structure(x, vectorized = TRUE)
}


find_input_names <- function(x) {
  if (is.name(x))
    check_in_input(x, is.input)
  else if (is.call(x))
    unlist(lapply(x, find_input_names), use.names = FALSE)
  else
    NULL
}

find_reactive_args <- function(x, recursive = FALSE) {
  if (is.reactive(x))
    attr(x, "input_names")
  else if (is.name(x))
    check_in_input(x, is.reactive)
  else if (is.call(x) && recursive)
    unlist(lapply(x, find_reactive_args, recursive = TRUE), use.names = FALSE)
  else
    NULL
}

find_posterior_args <- function(x, recursive = FALSE) {
  if (is.vectorized(x))
    x
  else if (is.name(x))
    check_in_input(x, is.posterior)
  else if (is.call(x) && recursive)
    unlist(lapply(x, find_posterior_args, recursive = TRUE), use.names = FALSE)
  else
    NULL
}

find_names <- function(x) {
  if (is.call(x))
    unlist(lapply(x[-1], find_names), use.names = FALSE)
  else if (is.name(x))
    deparse(x)
  else
    NULL
}


eval_curly <- function(x) {
  others <- x[seq(2, length(x) - 1)]
  last <- x[[length(x)]]
  lapply(others, eval_statement)
  eval_statement(last, last_statement = TRUE)
}

eval_substatement <- function(x) {
  ## browser(expr = getOption("debug_on"))
  if (any(is_arg_call(x)))
    x <- eval_args(x)

  if (is.input(x))
    stop()


  # A name only gets to this point if it's the last statement
  else if (is.name(x))
    delayed_pickup(x)

  else if (is.call(x)) {
    reactive_args <- compact(lapply(x[-1], find_reactive_args))
    post_args <- compact(lapply(x[-1], find_posterior_args))

    n_reactive <- length(reactive_args)
    n_posterior <- length(post_args)

    if (n_posterior > 0 || n_reactive > 0) {
      res <-
        if (n_posterior > 0) {
          fun <- deparse(first(x))

          if (fun %in% c("(", "{", "I"))
            to_loop(x, vectorized = TRUE)

          # No looping for binary elementwise ops with two posterior args
          else if (n_posterior == 2 && fun %in% c("+", "-", "*", "/"))
            to_loop(x, vectorized = TRUE)

          else
            to_loop(x)
        }
        else 
          x

      if (n_reactive > 0)
        reactive(res, input_names = unique(unlist(reactive_args, use.names = FALSE)))

      else
        res
    }

    else {
      res <- eval_in_input(x)
      clear_refs(x)
      res
    }
  }
}

# Eval arguments of a call. If result is a to_loop expression, return
# it. Otherwise, add the result to the ref stack and return a
# reference to the result.

# Results that do not rely on posterior objects do not need to be
# computed in the posterior loops, so we eval them before to recycle
# them.
eval_args <- function(x) {
  which_is_call <- which(c(FALSE, is_arg_call(x)))

  for (i in which_is_call) {
    obj <- eval_substatement(x[[i]])

    x[i] <- 
      if (is.to_loop(obj))
        as.expression(obj)

      else {
        index <- add_ref(obj)
        res <- make_ref_call(index)

        if (is.input(obj))
          res <- input(res)

        as.expression(res)
      }
  }

  x
}

# Returns to_loop rhs call if it relies on a posterior variable, NULL
# otherwise
eval_assignment <- function(lhs, rhs) {
  if (is_locked(lhs))
    stop(paste("Cannot reassign", sQuote(lhs), "because it is part of an interactive call"),
      call. = FALSE)

  if (is.call(rhs))
    rhs <- eval_substatement(rhs)

  rhs <- 
    if (is.to_loop(rhs)) {
      if (is.reactive(rhs)) {
        add_to_reactive_stack(lhs, rhs)
        lock(c(lhs, find_names(rhs)))
        reactive("empty", input_names = attr(rhs, "input_names"), args = find_names(rhs))
      }
      else {
        add_to_call_stack(lhs, rhs)
        empty_posterior()
      }
    }

    else if (is.name(rhs))
      get_in_input(deparse(rhs))

    else
      rhs

  # will have to assign in process_call?
  ## assign_in_input(lhs, rhs)
  NULL
}

eval_statement <- function(x, last_statement = FALSE) {
  if (is.assignment(x))
    eval_assignment(lhs = deparse(x[[2]]), rhs = x[[3]])

  if (last_statement) {
    # todo: reactive last statement
    res <- 
      if (is.assignment(x))
        NULL
      else
        eval_substatement(x)

    if (is.to_loop(res))
        add_to_call_stack("_last", rhs = res)
    if (is.reactive(res))
      add_to_reactive_stack("_last", rhs = res)

    # todo: what if reactive is not a posterior??
    ## assign_in_input("_last", empty_posterior())

    process_stack(call_stack())
    clear_call_stack()
    clear_refs(stack)

    if (is.vectorized(res))
      eval_in_input(res)
    else if (is.delayed(res))
      get_in_input(deparse(res))
    else if (is.reactive(res))
      make_reactive_function(res)
    else if (length(stack) > 0)
      get_in_input("_last")
    else
      stop()
  }

  # If not assignment and not last statement, ignore the statement
  else
    NULL
}


## problem: atm there are both posterior and non-posterior statements
## in reactive_stack ...

## simplest design: save all calls in stacks and figure out in
## `process_stack` which are posterior and which are not.

make_reactive_function <- function(last_call) {
  browser(expr = getOption("debug_on"))
  stack <- reactive_stack()
  inputs <- attr(last(stack), "input_names")

  # Keep only relevant calls
  stack <- Filter(function(x) attr(x, "input_names") %in% inputs, stack)

  fun <- function() {
    process_stack(stack)
    get_in_input("_last")
  }

  args <- do.call("pairlist", vector("list", length(inputs)))
  names(args) <- inputs
  formals(fun) <- args
}


process_stack <- function(stack) {
  browser(expr = getOption("debug_on"))
  if (length(stack) > 0) {
    # Perf note: relatively expensive
    stack <- lapply(stack, process_call)

    # Wrap each non-vectorized call in a 'for' loop over sims. Wrap
    # vectorized call in a "{" ending with NULL (to avoid
    # unnecessary return values)
    stack <- lapply(stack, function(x) {
      if (is.vectorized(x))
        call("{", x, quote(NULL))
      else
        call("{", x) %>% call("for", quote(`_i`), bquote(seq(1, .(nsims()))), .)
    })

    lapply(stack, eval_in_input)
  }

  NULL
}
