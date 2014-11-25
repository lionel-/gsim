
P <- function(x) {
  class(x) <- c(class(x), "protected")
  x
}

T <- function(x) {
  class(x) <- c(class(x), "tidy_me")
  x
}

is.protected <- function(x) {
  inherits(x, c("protected", "AsIs"))
}

is.to_tidy <- function(x) {
  inherits(x, "tidy_me")
}


get_output_names <- function(x) {
  if (is.name(x))
    as.character(x)

  else if (is.call(x) && x[[1]] == as.name("list")) {
    x <- x[-1]

    names <- names(x) %||% as.character(x)
    empty <- names == ""
    names[empty] <- as.character(x[empty])

    # Remove I(), P() and T()
    trimmed <- stringr::str_match(names, "^I\\(([^)]+)\\)$")[, 2]
    names[!is.na(trimmed)] <- trimmed[!is.na(trimmed)]

    names
  }

  else
    NULL
}


maybe_tidy <- function(x, names) {
  browser(expr = getOption("debug_on"))
  if (is.protected(x))
    clean_class(x)

  else if (is.list(x)) {
    is.to_clean <-
      if (context("tidy_output"))
        is.protected
      else
        is.posterior
    
    is_to_clean <- papply(x, is.to_clean)
    is_to_tidy <-
      if (context("tidy_output"))
        !is_to_clean
      else
        papply(x, is.to_tidy)

    # Need to wrap tidy in a function to avoid lapply/mapply dispatching bug
    x[is_to_clean] <- lapply(x[is_to_clean], clean_class)
    x[is_to_tidy] <- Map(function(item, name) tidy(item, name),
      item = x[is_to_tidy], name = names[is_to_tidy] %||% NULL)

    names(x) <- make.names(names)
    x
  }

  else if (context("tidy_output") || is.to_tidy(x))
    tidy(x, names)

  else if (is.posterior(x))
    clean_class(x)

  else
    x
}

clean_class <- function(x) {
  class_diff <- 
    if (inherits(x, "protected"))
      c("protected", "tidy_me")
    else
      c("AsIs", "posterior", "tidy_me")

  class(x) <- (setdiff(class(x), class_diff)) %||% "numeric"
  x
}

tidy <- function(x, name = NULL) {
  UseMethod("tidy")
}

tidy.default <- function(x, ...) {
  x 
}

tidy.numeric <- function(x, name = NULL) {
  # Don't coerce atomics and vectors.
  # We don't use prod here because prod(NULL) = 1
  if ((Reduce(`*`, dim(x)) == length(x)) %||% TRUE)
    drop(x)
  else {
    x <- as.data.frame(x)
    names(x) <- make_names(x, name)
    x
  }
}

tidy.posterior <- function(x, name = NULL) {
  if (is.null(dim(x)))
    dim(x) <- length(x)
  dims <- dim(x)
  while ((last(dims) == 1) %||% FALSE)
    dims <- dims[-length(dims)]

  class(x) <- "array"
  n_dims <- length(dims)
  n_sims <- dims[1]

  if (n_dims %in% c(1, 2)) {
    name <- name %||% "theta"
    names <-
      if (!is.null(colnames(x)))
        colnames(x)
      else if (n_dims == 1)
        name
      else
        make_names(name, seq_len(dims[2]))

    x <- data.frame(seq_len(n_sims), x)
    names(x) <- c("sim", names)

    if (n_dims == 2)
      x <- tidyr::gather_(x, name, "value", names(x)[-1])
  }


  else if (n_dims == 3) {
    x <- apply(x, 1, list) %>%
      unlist2(recursive = FALSE) %>%
      ## lapply(dplyr::as_data_frame) %>%
      lapply(as.data.frame) %>%
      rbind_all

    sim_index <- rep(seq_len(n_sims), each = dims[2])
    x <- cbind(sim_index, x)
    names(x) <- c("sim", make_names(name, seq_along(x[-1])))
  }

  else {
    x <- melt(x)
    names(x) <- c("sim", paste0("dim", seq_len(n_dims - 1)), "value")
  }
  
  x
}

make_names <- function(name, seq) {
  name <- name %||% "theta"

  ends_numeric <- stringr::str_sub(name, -1) %in% as.character(0:9)
  if (ends_numeric)
    name <- paste0(name, "_")

  if (length(seq) == 1)
    name
  else
    paste0(name, seq)
}
