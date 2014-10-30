

wrap_posterior <- function(x, dims) {
  empty_args <- replicate(length(dims) - 1, substitute())
  args <- c(list("[", x, quote(`_i`)), empty_args)
  do.call(call, args, quote = TRUE)
}

eval_first <- function(expr) {
  old <- get_in_input("_i")
  assign_in_input("_i", 1)
  eval_in_input(expr)
  assign_in_input("_i", old)
  NULL
}


process_name <- function(x) {
  obj <- get_in_input(as.character(x))

  if (is.posterior(obj))
    wrap_posterior(x, dim(obj))
  else
    x
}

process_call <- function(x, single_sim = FALSE) {
  # If the arguments of a call are all vectorized operations, this
  # call is itself vectorized. If the arguments are mixed, then we
  # need to loop over the vectorised arguments as well.
  ## browser(expr = getOption("debug_on"))

  predicate <-
    if (single_sim)
      function(x) {
        is.posterior_call(x) ||
          (is.name(x) && is.posterior(get_in_input(as.character(x))))
      }
    else
      is.to_loop

  # First element is the function name, so we do not need to treat it
  is_to_loop <- papply(x[-1], predicate)
  x[-1][is_to_loop] <- lapply(x[-1][is_to_loop], process_expression,
    single_sim = single_sim)
  
  # single_sim needs a different logic: recursing until finding a name
  if (is.to_loop(x) && !single_sim) {
    is_posterior_call <- papply(x[-1], is.posterior_call)
    x[-1][is_posterior_call] <- lapply(x[-1][is_posterior_call], function(item) {
      obj <- eval_in_input(item) %>% init_posterior
      wrap_posterior(item, dim(obj))
    })
    x
  }
  else
    x
}

process_expression <- function(x, single_sim = FALSE) {
  # Process expressions to modify calls depending on posterior arguments
  # with loop subsetting (need to initialise posterior if not done yet
  # to get dimensions of posterior objects)
  ## browser(expr = getOption("debug_on"))

  if (is.no_loop(x) && !single_sim)
    x

  else if (is.call(x))
    process_call(x, single_sim = single_sim)

  else if (is.name(x))
    process_name(x)

  else
    x
}

process_assignment <- function(x, single_sim = FALSE) {
  browser(expr = getOption("debug_on"))
  lhs <- x[[2]]
  rhs <- x[[3]]

  #todo: Why would is.to_loop() not be applicable?
  ## if (is.posterior_call(rhs) && !is.no_loop(rhs)) {
  if (is.to_loop(rhs) || (is.no_loop(rhs) && single_sim)) {
    rhs <- process_call(rhs, single_sim = single_sim)

    # Need to get dimensions of rhs to construct the lhs subsetting
    rhs_obj <- eval_in_input(rhs) %>% init_posterior

    # Is this still needed? Now we evaluate sequentially
    assign_in_input(as.character(lhs), rhs_obj)

    lhs <- wrap_posterior(lhs, dim(rhs_obj))
    call("<-", lhs, rhs)
  }
  else
    no_loop(x)
}

# two logics in reactive functions: either give posterior mean, or use
# a single sim.

# posterior mean: respect vectorised ops.
# single sim: wrap every posterior_call objects

process_stack <- function(stack, single_sim = FALSE) {
  # We process and eval each expression sequentially because some LHS
  # may be needed to figure out the dimensions of certain quantities
  browser(expr = getOption("debug_on"))

  for (i in seq_along(stack)) {
    expr <- process_assignment(stack[[i]], single_sim = single_sim)

    # Wrap each non-vectorized call in a 'for' loop over sims. Wrap
    # vectorized call in a "{" ending with NULL (to avoid
    # unnecessary return values)
    expr <- 
      if (is.no_loop(expr) || single_sim)
        call("{", expr, quote(NULL))
      else
        call("{", expr) %>% call("for", quote(`_i`), bquote(seq(1, .(nsims()))), .)

    eval_in_input(expr)
    stack[[i]] <- expr
  }

  stack
}

make_reactive_function <- function(last_call) {
  stack_temp <- reactive_stack()
  deps <- attr(last(stack_temp), "deps")

  # Discard calls with non-relevant inputs
  stack <- list(last(stack_temp))
  while (length(stack_temp) > 0) {
    if (as.character(stack_temp[[c(length(stack_temp), 2)]]) %in% deps) {
      deps <- unique(c(deps, attr(last(stack_temp), "deps")))
      stack <- append(stack, last(stack_temp), after = 0)
    }
    last(stack_temp) <- NULL
  }


  # Pointer to container
  `_enclos_env` <- container_env()

  # Process stacks only once, when first called. We keep two different
  # stacks in memory, one optimized for computing all simulations, and
  # the other for computing quantities on a by-simulation basis.
  processed_stack <- NULL
  `_last` <- NULL
  
  fun <- function() {
    # So that dyn_get can find `_enclos_env` in the parent env
    `_gsim_container` <- TRUE

    # Assign user inputs to variables that can be lookep up while
    # evaluating calls
    inputs <- names(formals())
    lapply(inputs, function(x) {
      value <- get(x, envir = parent.env(environment()))
      assign_in_input(paste0("_input_ref_", x), value)
    })

    browser(expr = getOption("debug_on"))

    if (is.function(out)) {
      if (is.null(processed_stack)) {
        process_stack(stack)
        `_last` <<- get_in_input("_last")
      }

      `_last` %% out
    }

    else {
      if (out == "random")
        out <- sample(seq_len(nsims()), 1)
      assign_in_input("_i", out)

      if (is.null(processed_stack))
        processed_stack <<- process_stack(stack, single_sim = TRUE)
      else 
        lapply(processed_stack, eval_in_input)


      res <- get_in_input("_last")
      res <- pick_sim(res, out)

      if (drop)
        drop(res)
      else
        res
    }
  }

  inputs <- attr(last(stack), "input_names")
  args <- do.call("pairlist", vector("list", length(inputs) + 2))

  names(args) <- c(inputs, "out", "drop")
  len <- length(args)
  args[c(len - 1, len)] <- c("random", TRUE)

  formals(fun) <- args
  class(fun) <- c("function", "reactive_fun")

  fun
}
