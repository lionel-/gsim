

call_args <- function(x) as.list(x)[-1]
is_arg_call <- function(x) vapply(call_args(x), is.call, logical(1))

is.empty <- function(x) inherits(x, "empty")
is.assignment <- function(x) is.call(x) && as.character(x[[1]]) == "<-"
is.to_loop <- function(x) inherits(x, "to_loop")
is.no_loop <- function(x) inherits(x, "no_loop")
is.posterior_call <- function(x) inherits(x, "posterior_call")
is.recyclable <- function(x) {
  duration <- attr(x, "non_recyclability")
  ((duration == 0) %||% TRUE) &&
    !(is.name(x) && is.posterior(get_in_storage(as.character(x))))
}


empty_posterior <- function(x, ...) {
  structure(NULL, class = c("empty", "posterior"), ...)
}

to_loop <- function(x) {
  structure(x, class = c("to_loop", "posterior_call"))
}

no_loop <- function(x, subclass = NULL) {
  structure(x, class = c("no_loop", subclass))
}


non_recyclability <- function(x, recursive = FALSE) {
  args_durations <- lapply(x[-1], function(x) {
    if (is.call(x) && !is.recyclable(x[[1]]))
      attr(x, "duration")
    else
      NULL
  })

  # Decrement non-recyclability duration of arguments
  args_durations <- unlist2(args_durations) - 1

  # Find if call is non_recyclable
  call_duration <-
    if (x[[1]] == as.name("intercept"))
      1
    else
      NULL

  durations <- c(args_durations, call_duration)

  if (length(durations) == 0)
    NULL
  else
    max(durations)
}

find_input_names <- function(x) {
  if (is.name(x))
    check_in_storage(x, is.input)
  else if (is.call(x))
    unlist2(lapply(x, find_input_names))
  else
    NULL
}

find_reactive_args <- function(x, recursive = FALSE) {
  if (is.reactive(x))
    input_names(x)
  else if (any(is.reactive_lhs(x)))
    input_names(reactive_lhs_list(as.character(x)))
  else if (is.call(x) && recursive)
    unlist2(lapply(x, find_reactive_args, recursive = TRUE))
  else
    NULL
}

find_posterior_args <- function(x, recursive = FALSE) {
  if (is.posterior_call(x) && !recursive)
    x
  else if (is.name(x))
    check_in_storage(x, is.posterior)
  else if (is.call(x) && recursive)
    unlist2(lapply(x, find_posterior_args, recursive = TRUE))
  else
    NULL
}

find_names <- function(x) {
  if (is.call(x))
    unlist2(lapply(x[-1], find_names))
  else if (is.name(x))
    deparse(x)
  else
    NULL
}

get_new_inputs <- function(x) {
  if (is.reactive(x))
    input_names(x)
  else if (is.name(x) && is.locked(x)) {
    reactive_lhs <- reactive_lhs_list()[[match(x, reactive_lhs)]]
    input_names(reactive_lhs)
  }
  else
    NULL
}

maybe_recycle <- function(x) {
  if (is.atomic(x))
    x
  else if (is.recyclable(x) && !(is.name(x) &&  is.locked(x))) {
    res <- eval_in_storage(x)
    is_input <- is.input(x)
    x <- add_ref(res, is_input)
    x
  }
  else
    x
}

# If argument is a posterior object scheduled to be looped over,
# return it as is. If it is an expression depending on reactive
# inputs, mark the surrounding call as reactive.
pass1_args <- function(x) {
  browser(expr = pass1)
  which_is_call <- which(c(FALSE, is_arg_call(x)))
  inputs <- NULL

  for (i in seq_along(x)[-1]) {
    obj <-
      if (i %in% which_is_call)
        pass1_substatement(x[[i]])
      else
        x[[i]]

    obj <- maybe_recycle(obj)
    x[[i]] <- obj
  }

  new_inputs <- get_new_inputs(obj)
  inputs <- unique(c(inputs, new_inputs))
  if (is.null(inputs))
    x
  else
    reactive(x, inputs)
}


# Looping over all simulations can be avoided in certain cases
mark_looping <- function(x, n_post, to_loop) {
  fun <- as.character(first(x))
  n <- length(x) - 1

  if (to_loop)
    to_loop(x)

  else if (fun %in% c("(", "{", "I", "P", "list"))
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

pass1_substatement <- function(x) {
  browser(expr = pass1)
  if (any(is_arg_call(x)))
    x <- pass1_args(x)

  # A name only gets to this point if it's the last statement
  if (is.name(x))
    ## delayed_pickup(x)
    stop("name in pass1_substatement")

  # Mark calls: should posterior call be looped, should non-posterior
  # call be recycled, is call reactive?
  else if (is.call(x)) {
    reactive_args <- unique(unlist(compact(lapply(x[-1], find_reactive_args))))
    posterior_args <- compact(lapply(x[-1], find_posterior_args))
    n_reactive <- length(reactive_args)
    n_posterior <- length(posterior_args)

    to_loop <- any(papply(posterior_args, is.to_loop))

    non_recyclability <- 
      if (n_reactive > 0 || n_posterior > 0)
        Inf
      else
        non_recyclability(x)

    x <- 
      if (n_posterior > 0)
        mark_looping(x, n_posterior, to_loop)
      else
        x

    # Happens when one of the arguments is a previous LHS reactive
    x <- 
      if (n_reactive > 0 && !is.reactive(x))
        reactive(x, reactive_args)
      else
        x

    attr(x, "non_recyclability") <- non_recyclability
    x
  }

  else
    stop("Something went wrong in pass1_substatement")
}


pass1_assignment <- function(lhs, rhs) {
  browser(expr = pass1)
  if (is.locked(lhs))
    stop(paste("Cannot reassign", sQuote(lhs), "because it is part of an
      interactive call"), call. = FALSE)

  if (is.call(rhs))
    rhs <- pass1_substatement(rhs)
  rhs <- maybe_recycle(rhs)

  # Needed so that find_posterior_args will recognize posterior lhs
  # when they are included as arguments in ulterior calls
  if (is.posterior_call(rhs))
    assign_in_storage(lhs, empty_posterior())

  if (is.name(rhs)) {
    rhs_obj <- storage(as.character(rhs))
    assign_in_storage(lhs, rhs_obj)
  }
  else if (is.atomic(rhs))
    assign_in_storage(lhs, rhs)
  else if (is.reactive(rhs)) {
    lock(lhs, dependencies = find_names(rhs), inputs = input_names(rhs))
    add_to_reactive_stack(lhs, rhs)
  }
  else
    add_to_call_stack(lhs, rhs)

  NULL
}


pass1_statement <- function(x, last_statement = FALSE) {
  if (is.assignment(x))
    pass1_assignment(lhs = deparse(x[[2]]), rhs = x[[3]])
  else if (last_statement)
    pass1_assignment(lhs = "_last", rhs = x)

  stack <- context("call_stack")
  if (last_statement) {
    browser(expr = pass2)
    stack <- pass2_stack(stack)
    clear_call_stack()
    clear_refs(stack)

    storage("_last")
  }
  else
    NULL
}

pass1_dots <- function(x) {
  force(x)
  if (length(x) == 1)
    pass1_statement(x[[1]], last_statement = TRUE)
  else {
    others <- x[seq(1, length(x) - 1)]
    for (i in seq_along(others))
      enclos <- pass1_statement(others[[i]])
    pass1_statement(last(x), last_statement = TRUE)
  }
}
