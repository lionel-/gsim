
make_reactive_function <- function() {
  stack_temp <- context("reactive_stack")
  deps <- attr(last(stack_temp), "deps")

  # Discard calls with non-relevant inputs
  stack <- list(last(stack_temp))
  while (length(stack_temp) > 0) {
    last <- as.character(stack_temp[[c(length(stack_temp), 2)]])
    if (last %in% deps) {
      deps <- unique(c(deps, attr(last(stack_temp), "deps")))
      stack <- append(stack, last(stack_temp), after = 0)
    }
    last(stack_temp) <- NULL
  }

  context <- context()
  storage <- storage()

  # Process stacks only once, when first called. We keep two different
  # stacks in memory, one optimized for computing all simulations, and
  # the other for computing quantities on a by-simulation basis.
  processed_stack <- NULL
  processed_stack_single <- NULL
  

  fun <- reactive_fun
  environment(fun) <- environment()

  inputs <- attr(last(stack), "input_names")
  args <- do.call("pairlist", vector("list", length(inputs) + 2))

  names(args) <- c(inputs, "out", "drop")
  len <- length(args)
  args[c(len - 1, len)] <- c("random", TRUE)

  formals(fun) <- args
  class(fun) <- c("function", "reactive_fun")

  fun
}

reactive_fun <- function() {
  # Anchor signalling dyn_get to find `context` and `storage` in the
  # parent environment
  `_anchor` <- TRUE

  # Assign user inputs to variables that can be lookep up while
  # evaluating calls
  input_names <- names(formals())
  n_inputs <- length(input_names) - 2
  input_names <- input_names[seq_len(n_inputs)]

  inputs <- lapply(input_names, function(input) get(input))
  names(inputs) <- input_names

  inputs_len <- vapply(inputs, length, numeric(1))
  n_vectors <- sum(inputs_len > 1)
  if (n_vectors > 1)
    stop(paste("Reactive functions accept at most one vector. Try replacing", n_vectors - 1, "vectors by scalars"),
      call. = FALSE)


  # Select simulation with which to evaluate stack
  if (!is.function(out)) {
    if (out == "random") {
      n_sims <- context("n_sims")
      out <- sample(seq_len(n_sims), 1)
    }
    assign_in_storage("_i", out)
  }

  compute <- compute
  environment(compute) <- environment()

  # Using Map with do.call to allow vector inputs
  res <- do.call(Map, c(list(f = compute), inputs))
  unlist2(res, recursive = FALSE)
}

compute <- function(...) {
  dots <- list(...)
  dot_names <- names(dots)

  Map(function(input, value) {
    assign_in_storage(paste0("_input_ref_", input), value)
  }, input = dot_names, value = dots)


  if (is.function(out)) {
    if (is.null(processed_stack))
      processed_stack <<- pass2_stack(stack)
    else
      lapply(processed_stack, eval_in_storage)

    get_in_storage("_last") %% out
  }

  else {
    if (is.null(processed_stack_single))
      processed_stack_single <<- pass2_stack(stack, single_sim = TRUE)
    else 
      lapply(processed_stack_single, eval_in_storage)

    res <- get_in_storage("_last")
    res <- pick_sim(res, out)

    if (drop)
      drop(res)
    else
      res
  }
}

is.reactive_fun <- function(x) inherits(x, "reactive_fun")
