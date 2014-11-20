
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


maybe_tidy <- function(x, name) {
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

    x[is_to_clean] <- lapply(x[is_to_clean], clean_class)
    x[is_to_tidy] <- lapply(x[is_to_tidy], tidy, name)
    x
  }

  else if (context("tidy_output") || is.to_tidy(x))
    tidy(x, name)

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

  class(x) <- setdiff(class(x), class_diff) %||% "numeric"
  x
}

tidy <- function(x, name = NULL) {
  UseMethod("tidy")
}

tidy.default <- function(x, ...) {
  x 
}

tidy.numeric <- function(x, name = NULL) {
  x <- as.data.frame(x)
  names(x) <- make_names(x, name)
  x
}

tidy.posterior <- function(x, name = NULL) {
  dims <- dim(x)
  if (is.null(dims) || length(dims) == 1)
    dim(x) <- dims <- c(length(x), 1)

  class(x) <- "array"
  n_dims <- length(dims)
  n_sims <- dim(x)[1]

  if (n_dims %in% c(1, 2)) {
    x <- data.frame(sim = seq_len(n_sims), x)
    names(x)[-1] <- make_names(x[-1], name)
  }
  else if (n_dims == 3) {
    x <- apply(x, 1, list) %>%
      unlist2(recursive = FALSE) %>%
      lapply(as.data.frame) %>%
      rbind_all

    sim_index <- rep(seq_len(n_sims), each = dims[2])
    x <- cbind(sim_index, x)
    names(x) <- c("sim", make_names(x[-1], name))
  }
  else {
    x <- melt(x)
    names(x) <- c("sim", paste0("dim", seq_len(n_dims - 1)), "value")
  }
  
  x
}

make_names <- function(x, name = NULL) {
  if (is.null(name))
    name <- "col"
  
  if (length(x) == 1)
    name
  else
    paste0(name, seq_along(x))
}
