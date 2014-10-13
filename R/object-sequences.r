

## curve (invlogit (cbind (1, x/100, .5) %*% coef(fit.3)), add=TRUE)


vseq <- function(gs = NULL, seq = NULL, input_name = NULL, n = 101) {

  if (is.null(gs) && is.null(seq))
    stop("Need either a gs or a sequence")
  stopifnot(is.null(gs) || is.data(gs))

  if (is.null(input_name))
    input_name <- name(gs)
  if (is.null(seq))
    seq <- seq(range(gs)[1], range(gs)[2], length.out = n)

  ## Ensure mean of sequence is included (because this is the default
  ## value when user omits to specify argument in sequence function).
  seq_mean <- mean(c(min(seq), max(seq)))
  if (!seq_mean %in% seq)
    seq <- c(seq, seq_mean) %>% sort

  gs_seq <- lapply(seq, gs, class = "data", colnames = input_name) %>%
    list %>%
    set_class("data.frame") %>%
    set_attr("row.names", .set_row_names(1))

  res <- data.frame(
    seq = seq,
    value = gs_seq
  )

  seq_id <- list()
  seq_id[input_name] <- runif(1) %>% as.character
  names(res)[1] <- input_name

  seq_gs(res, seq_id)
}

x <- function(...) vseq(input_name = "x", ...)
y <- function(...) vseq(input_name = "y", ...)
z <- function(...) vseq(input_name = "z", ...)


seq_gs <- function(res, inputs) {
  class(res) <- c("gs", "seq_gs", "data.frame")
  attr(res, "inputs") <- inputs
  res
}

is.seq_gs <- function(x) inherits(x, "seq_gs")


## Todo: Reduce is inefficient with variadic functions such as
## cbind: it will call `operate` nseq * nvar times

seq_operate <- function(..., fun) {
  args <- dots(...)
  
  ## Remove unwanted simulations from new posterior objects
  args <- lapply(args, function(x) {
    if (is.posterior(x) && length(x) == nsims()) {
      do_naked(x, x[seq_index()])
    } else
      x
  })

  compound_seqs <- function(a, b) {
    if (!any(is.seq_gs(a), is.seq_gs(b))) {
      return(fun(a, b))
    }

    else if (!is.seq_gs(a)) {
      res <- b %>%
        rowwise() %>%
        do(value = fun(a, .$value)) %>%
        cbind.data.frame(select(b, -value), .) %>%
        seq_gs(inputs = inputs(b))

      return(res)
    }

    else if (!is.seq_gs(b)) {
      res <- a %>%
        rowwise() %>%
        do(value = fun(.$value, b)) %>%
        cbind.data.frame(a %>% select(-value), .) %>%
        seq_gs(inputs = inputs(a))
      return(res)
    }


    ## Multiple sequences

    not_in_a_p <- !ids(b) %in% ids(a)
    inputs_a <- input_names(a)
    inputs_b <- input_names(b)
    inputs_not_in_a <- input_names(b)[not_in_a_p]
    inputs_not_in_b <- input_names(a)[!ids(a) %in% ids(b)]
    twins <- input_names(b)[ids(b)  %in% ids(a)]


    ## Todo: check that redundant names have the same id
    inputs <- c(inputs(a), inputs(b)[not_in_a_p])
    Reduce(function(x, y) {stopifnot(!x == y); y}, inputs, NULL)


    ## Arranging common sequences (columns twins) in the same order
    a <- a %>% arrange_s(c(twins, inputs_not_in_b))
    b <- b %>% arrange_s(c(twins, inputs_not_in_a))


    seq_length <- function(obj) {
      Reduce(function(x, y) length(unique(x)) * length(unique(y)), obj, 1)
    }

    b_factor <- seq_length(a[inputs_not_in_b])
    a_factor <- seq_length(b[inputs_not_in_a])
    
    ## Expanding grid. Using rep.int trick for performance, but need
    ## to expand vector of gs objects with usual subsetting.
    ##   - Repeat b by length of new variables in a
    ##   - Repeat a by length of new variables in b
    exp_gs_a <- a["value"][rep(seq_len(nrow(a)), a_factor), ]
    exp_gs_b <- b["value"][rep(seq_len(nrow(b)), each = b_factor), ]
    
    exp_a <-
      Reduce(function(a1, a2) cbind(safe_rep.int(a1, a_factor),
                             safe_rep.int(a2, a_factor)),
             a[-length(a)], NULL) %>%
      data.frame(value1 = I(exp_gs_a))

    exp_b <-
      Reduce(function(b1, b2) cbind(rep(b1, each = b_factor),
                             rep(b2, each = b_factor)),
             b[-length(b)], NULL) %>%
      data.frame(value2 = I(exp_gs_b)) %>%
      set_names(inputs_b, "value") %>%
      extract(, c(inputs_not_in_a, "value"))


    merged <- data.frame(exp_a, exp_b) %>%
      set_names(inputs_a, "value1", inputs_not_in_a, "value2")

    merged %>%
      rowwise() %>%
      do(value = fun(.$value1, .$value2)) %>%
      cbind(merged, .) %>%
      select(-c(value1, value2)) %>%
      seq_gs(inputs = inputs)
  }

  if (length(args) == 1) {
    a <- first(args)
    a %>%
      rowwise() %>%
      do(value = fun(.$value)) %>%
      cbind.data.frame(a %>% select(-value), .) %>%
      seq_gs(inputs = inputs(a))
  } else {
    Reduce(compound_seqs, args)
  }
}


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

inputs <- function(x) attr(x, "inputs")

input_names <- function(x) names(inputs(x))

ids <- function(x) vapply(inputs(x), identity, character(1))


#' @export
print.seq_fun <- function(x) {
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
