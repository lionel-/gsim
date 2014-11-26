

call_args <- function(x) as.list(x)[-1]
is_arg_call <- function(x) vapply(call_args(x), is.call, logical(1))
call_fun <- function(x) deparse(x[[1]]) 


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
  else if (is.reactive_lhs(x)) {
    lhs <- reactive_lhs_list(as.character(x))
    input_names(lhs)
  }
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
    reactive_lhs <- reactive_lhs_list(as.character(x))
    input_names(reactive_lhs)
  }
  else
    NULL
}

maybe_recycle <- function(x) {
  if (is.atomic(x) || is.name(x))
    x
  else if (is.recyclable(x) && !(is.name(x) &&  is.locked(x))) {
    res <- eval_in_storage(x) %||% null()

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
        pass1_call(x[[i]])
      else
        x[[i]]

    obj <- maybe_recycle(obj)
    x[[i]] <- obj

    new_inputs <- get_new_inputs(obj)
    inputs <- unique(c(inputs, new_inputs))
  }

  
  if (is.null(inputs)) {
    if (call_fun(x) %in% vectorised_funs) {
      to_loop_args <- papply(x, is.to_loop)

      if (any(to_loop_args))
        x[to_loop_args] <- lapply(x[to_loop_args], function(item) {
          ref <- add_ref(empty_posterior())
          add_to_call_stack(ref, item)
          no_loop(ref, subclass = "posterior_call")
        })
    }

    x
  }
  else
    reactive(x, inputs)

}

vectorised_funs <- c(
  "(", "{", "I", "P", "T", "list",
  "%%", "summarise_sims", "bernoulli_check"
)

# Looping over all simulations can be avoided in certain cases
mark_looping <- function(x, n_post, to_loop) {
  fun <- as.character(first(x))
  n <- length(x) - 1

  if (to_loop)
    to_loop(x)

  else if (fun %in% vectorised_funs)
    no_loop(x, subclass = "posterior_call")

  # No looping for binary elementwise ops with _two_ _posterior_ args
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

pass1_call <- function(call) {
  browser(expr = pass1)
  if (any(is_arg_call(call)))
    call <- pass1_args(call)

  # Mark calls: should posterior call be looped, should non-posterior
  # call be recycled, is call reactive?
  reactive_inputs <- unique(unlist(compact(lapply(call[-1], find_reactive_args))))
  posterior_args <- compact(lapply(call[-1], find_posterior_args))
  n_reactive <- length(reactive_inputs)
  n_posterior <- length(posterior_args)

  to_loop <- any(papply(posterior_args, is.to_loop))

  non_recyclability <- 
    if (n_reactive > 0 || n_posterior > 0)
      Inf
    else
      non_recyclability(call)

  call <- 
    if (n_posterior > 0)
      mark_looping(call, n_posterior, to_loop)
    else
      call

  # Happens when one of the arguments is a previous LHS reactive
  call <- 
    if (n_reactive > 0 && !is.reactive(call))
      reactive(call, reactive_inputs)
    else
      call

  attr(call, "non_recyclability") <- non_recyclability
  call
}


pass1_assignment <- function(lhs, rhs) {
  browser(expr = pass1)
  if (is.locked(lhs))
    stop(paste("Cannot reassign", sQuote(lhs), "because it is part of an interactive call"),
      call. = FALSE)

  if (is.call(rhs))
    rhs <- pass1_call(rhs)
  rhs <- maybe_recycle(rhs)

  # Needed so that find_posterior_args will recognize posterior lhs
  # when they are included as arguments in ulterior calls
  if (is.posterior_call(rhs))
    assign_in_storage(lhs, empty_posterior())

  if (is.reactive(rhs)) {
    lock(lhs, dependencies = find_names(rhs), inputs = input_names(rhs))
    add_to_reactive_stack(lhs, rhs)
  }
  else if (is.name(rhs)) {
    rhs_obj <- storage(as.character(rhs))
    assign_in_storage(lhs, rhs_obj %||% null())
  }
  else if (is.atomic(rhs))
    assign_in_storage(lhs, rhs)
  else
    add_to_call_stack(lhs, rhs)

  NULL
}
