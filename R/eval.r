

call_args <- function(x) as.list(x)[-1]
is_arg_call <- function(x) vapply(call_args(x), is.call, logical(1))

is.empty <- function(x) inherits(x, "empty")
is.assignment <- function(x) is.call(x) && as.character(x[[1]]) == "<-"
is.to_loop <- function(x) inherits(x, "to_loop")
is.no_loop <- function(x) inherits(x, "no_loop")
is.posterior_call <- function(x) inherits(x, "posterior_call")
is.to_recycle <- function(x) inherits(x, "to_recycle")


empty_posterior <- function(x) {
  structure(NULL, class = c("empty", "posterior"))
}

to_loop <- function(x) {
  structure(x, class = c("to_loop", "posterior_call"))
}

no_loop <- function(x, subclass = NULL) {
  structure(x, class = c("no_loop", subclass))
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
    input_names(x)
  else if (any(is.reactive_lhs(x)))
    input_names(reactive_lhs(as.character(x)))
  ## else if (is.name(x))
  ##   check_in_input(x, is.reactive)
  else if (is.call(x) && recursive)
    unlist(lapply(x, find_reactive_args, recursive = TRUE), use.names = FALSE)
  else
    NULL
}

find_posterior_args <- function(x, recursive = FALSE) {
  if (is.posterior_call(x) && !recursive)
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


# If argument is a posterior object scheduled to be looped over,
# return it as is. If it is an expression depending on reactive
# inputs, mark the surrounding call as reactive.
eval_args <- function(x) {
  which_is_call <- which(c(FALSE, is_arg_call(x)))
  inputs <- NULL

  for (i in seq_along(x)[-1]) {
    obj <-
      if (i %in% which_is_call)
        eval_substatement(x[[i]])
      else
        x[[i]]


    if (!is.atomic(obj)) {
      #todo: simplify to_recycle (only used for inputs)
      if (is.input(obj))
        obj <- to_recycle(obj)
      else if (!(is.posterior_call(obj) || is.reactive(obj) || !(is.name(obj) &&  is.locked(obj)))) {
        res <- eval_in_input(obj)
        obj <- add_ref(res)
      }

      new_inputs <- 
        if (is.reactive(obj))
          input_names(obj)
        else if (is.name(obj) && is.locked(obj)) {
          reactive_lhs <- reactive_lhs()[[match(obj, reactive_lhs)]]
          input_names(reactive_lhs)
        }
        else
          NULL
      inputs <- unique(c(inputs, new_inputs))
    }

    x[[i]] <- obj
  }

  if (is.null(inputs))
    x
  else
    reactive(x, inputs)
}


# Looping over all simulations can be avoided in certain cases
mark_looping <- function(x, n_post) {
  fun <- as.character(first(x))
  n <- length(x) - 1

  if (fun %in% c("(", "{", "I"))
    no_loop(x, subclass = "posterior_call")

  # No looping for binary elementwise ops with _two posterior_ args
  else if (n_post == 2 && fun %in% c("+", "-", "*", "/"))
    no_loop(x, subclass = "posterior_call")

  # Optimized unary cbind (useful to make column vectors)
  else if (n == 1 && fun %in% c("cbind", "col_vector")) {
    x[[1]] <- as.name("unary_cbind")
    no_loop(x, subclass = "posterior_call")
  }

  else
    to_loop(x)
}

eval_substatement <- function(x) {
  if (any(is_arg_call(x)))
    x <- eval_args(x)

  # A name only gets to this point if it's the last statement
  if (is.name(x))
    ## delayed_pickup(x)
    stop("hey")

  else if (is.call(x)) {
    reactive_args <- unique(unlist(compact(lapply(x[-1], find_reactive_args))))
    n_reactive <- length(reactive_args)
    n_posterior <- length(compact(lapply(x[-1], find_posterior_args)))

    x <- 
      if (n_posterior > 0)
        mark_looping(x, n_posterior)
      else
        x

    # Happens when one of the arguments is a previous LHS reactive
    if (n_reactive > 0 && !is.reactive(x))
      reactive(x, reactive_args)
    else
      x
  }

  else
    stop("ho")
}


eval_assignment <- function(lhs, rhs) {
  if (is.locked(lhs))
    stop(paste("Cannot reassign", sQuote(lhs), "because it is part of an
      interactive call"), call. = FALSE)

  if (is.call(rhs))
    rhs <- eval_substatement(rhs)

  # Needed so that find_posterior_args will recognize posterior lhs
  # when they are included as arguments in ulterior calls
  if (is.posterior_call(rhs))
    assign_in_input(lhs, empty_posterior())

  if (is.reactive(rhs)) {
    lock(lhs, dependencies = find_names(rhs), inputs = input_names(rhs))
    add_to_reactive_stack(lhs, rhs)
  }
  else
    add_to_call_stack(lhs, rhs)

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
    browser(expr = getOption("debug_on"))

    if (is.reactive(res))
      add_to_reactive_stack("_last", rhs = res)
    else
      add_to_call_stack("_last", rhs = res)

    if (is.posterior_call(res))
      assign_in_input("_last", empty_posterior())

    stack <- process_stack(call_stack())
    clear_call_stack()
    clear_refs(stack)

    if (is.reactive(res))
      make_reactive_function(res)
    else if (is.no_loop(res))
      eval_in_input(res)
      stop("ha")
    else if (length(stack) > 0)
      get_in_input("_last")
    else
      stop("bah")
  }

  # If not assignment and not last statement, ignore the statement
  else
    NULL
}
