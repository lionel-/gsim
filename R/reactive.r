

## curve (invlogit (cbind (1, x/100, .5) %*% coef(fit.3)), add=TRUE)

reactive <- function(call, input_names, args = NULL) {
  structure(
    call,
    class = "reactive",
    input_names = input_names,
    args = args
  )
}

input <- function(input, input_names = NULL, ...) {
  #todo: is it still necessary to check in input?
  obj <- get_in_input(as.character(input))
  if (is.null(input_names) && is.input(obj))
    input_names <- attr(obj, "input_names")

  structure(
    input,
    class = "input",
    input_names = input_names,
    ...
  )
}

is.input <- function(x) inherits(x, "input")
is.reactive <- function(x) inherits(x, c("reactive", "input"))

x <- function() input("x", input_names = "x")
y <- function() input("y", input_names = "y")
z <- function() input("z", input_names = "z")





as.function.seq_gs <- function(obj) {
  seq_fun <-
    if (is.data(first(obj$value)))
      as.function.data(obj)

    else if (is.posterior(first(obj$value)))
      as.function.posterior(obj)

    else stop()
  

  args <- do.call("pairlist", vector("list", length(input_names(obj))))
  names(args) <- input_names(obj)

  if (is.posterior(first(obj$value)))
    args[["out"]] <- "random_sim"
  
  formals(seq_fun) <- args
  class(seq_fun) <- c("seq_fun", "function")
  seq_fun
}

as.function.data <- function(obj) {
  function(...) {
    args <- mget(names(formals()), sys.frame(sys.nframe()))
    formals(process_seq_in) <- c(args, alist(obj = ))

    do.call("process_seq_in", c(alist(obj = obj), args)) %>%
      as.numeric %>%
      process_seq_out
  }
}

as.function.posterior <- function(obj) {
  nsims <- length(first(obj$value))
  sims_perm <- sample(seq_len(nsims), nsims)
  i <- 1

  function(..., out = "random_sim") {
    args <- mget(names(formals()), sys.frame(sys.nframe()))
    formals(process_seq_in) <- c(args, alist(obj =))

    # Check that the input lengths are the same, or 1 (in which case,
    # the input is recycled), or 0 (in which case, we use the mean)
    vars <- args[setdiff(names(args), "out")]
    vars_lengths <- vapply(vars, length, numeric(1))
    seq_length <- max(vars_lengths)
    stopifnot(all(vars_lengths %in% c(seq_length, 1, 0)))

    # Use mean if input is not supplied
    is_null <- vapply(args, is.null, logical(1))
    null_vars <- names(args)[is_null]
    args[null_vars] <- lapply(null_vars, function(x) {
      (min(obj[x]) + max(obj[x])) / 2
    })

    # Reshape arguments in a form suitable to do.call and Map
    args <- Map(function(x, n) {
      x <- as.list(x)
      names(x) <- rep(n, length(x))
      x
    }, args, names(args))
    args["obj"] <- list(obj = list(obj))


    apply_process <- function(...) {
      res <- process_seq_in(...)
      if (length(res) == 1 && is.na(res))
        rep(NA, length.out = nsims)
      else
        do.call(c, res)
    }

    res <- do.call(function(...) Map(apply_process, ...), args)
    res <- 
      if (out == "random_sim")
        vpluck(res, pick_sim(), numeric(1))
      else if (is.numeric(out))
        vpluck(res, out, numeric(1))
      else stop("out argument not recognized")
    
    strip_attributes(res)
  }
}

process_seq_in <- function(obj) {
  inputs <- input_names(obj)

  outside_range <- vapply(inputs, function(input) {
    seq <- unique(obj[[input]])
    x <- get(input)
    if (!is.null(x) && (x < min(seq) || x > max(seq))) TRUE
    else FALSE
  }, logical(1))
  if (any(outside_range))
    return(NA)

  need_interpolate <- vapply(inputs, function(input) {
    x <- get(input)
    !(is.null(x) || x %in% obj[[input]])
  }, logical(1))


  if (sum(need_interpolate) > 1)
    stop("Only one variable can be interpolated. Supply exact values.")


  input_vals <- vapply(inputs, get, envir = environment(), numeric(1))

  subset <- input_vals[!need_interpolate] %>% as.list %>% quickdf
  subset <- 
    if (length(subset) == 0)
      obj
    else
      inner_join(subset, obj, by = names(subset))


  if (sum(need_interpolate) == 1) {
    interp_input <- inputs[need_interpolate]
    pos <- match_nearest(x, subset[[interp_input]])

    if (is.posterior(first(obj$value)))
      do_by_sims(nth(subset$value, pos[1]), nth(subset$value, pos[2]),
                 fun = function(a, b) (a + b) / 2)
    else
      (nth(subset$value, pos[1]) + nth(subset$value, pos[2])) / 2

  } else {
    single(subset$value)
  }
}

process_seq_out <- function(res) {
  if (length(res) > 1)
    as.data.frame(res)
  else
    strip_attributes(res)
}

match_nearest <- function(x, seq) {
  pos <- which(abs(seq - x) == min(abs(seq - x)))
  if (length(pos) == 2) {
    pos2 <- pos[2]
    pos <- pos[1]
  }

  else {
    pos2 <-
      if (x - seq[pos] > 0)
        pos + 1
      else if (x - seq[pos] < 0)
        pos - 1
      else  if (x == seq[pos])
        stop()
  }

  c(pos, pos2)
}

## pick_sim <- function() {
##   env <- parent.env(parent.frame())
##   sim <- env$sims_perm[env$i]
##   env$i <- env$i + 1
##   if (env$i > env$nsims) {
##     env$i <- 1
##     env$sims_perm <- sample(seq_len(env$nsims), env$nsims)
##   }
##   sim
## }

strip_attributes <- function(x) {
  ## Keep dims otherwise matrices will be coerced to vectors etc
  dims <- dim(x)
  attributes(x) <- NULL
  dim(x) <- dims
  x
}


#' @export
print.interactive_fun <- function(x) {
  obj <- environment(x)$obj

  names <- setdiff(names(obj), "value")
  nvar <- length(names)
  var_n <- vapply(obj[names], n_distinct, numeric(1))
  values <- vapply(obj[names], function(x) {
    if (n_distinct(x) > 5)
      paste0("(from ", round(min(x), 2), " to ", round(max(x), 2), ")")
    else
      paste0("(", paste(unique(x), collapse = ", "), ")")
  }, character(1))

  cat("gsim sequence with", nvar, "variables\n")
  for (i in seq_len(nvar))
    cat("  ", paste0(names[i], ":"), var_n[i], "distinct values", values[i], "\n")
}
