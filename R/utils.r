
pluck <- function(x, i) {
  lapply(x, `[[`, i)
}

vpluck <- function(x, i, type = NULL) {
  if (is.null(type)) type <- x[[1]][[i]]
  vapply(x, `[[`, i, FUN.VALUE = type)
}

indices <- function(gs) attr(gs, "indices", exact = TRUE)

labels <- function(gs) attr(gs, "labels", exact = TRUE)


## Faster than apply because uses colMeans
colVars <- function(a) {
  class(a) <- "matrix"
  n <- nrow(a)
  c <- ncol(a)
  .colMeans(((a - matrix(.colMeans(a, n, c), nrow = n, ncol = c, byrow = TRUE))^2), n, c) * n / (n - 1)
}

rowVars <- function(a) {
  class(a) <- "matrix"                   # Circumvents a bug
  n <- nrow(a)
  c <- ncol(a)
  .rowMeans(((a - matrix(.rowMeans(a, n, c), nrow = n, ncol = c, byrow = TRUE))^2), n, c) * n / (n - 1)
}


utils <- list()
utils$class <- function(x) cat(class(x), "\n")
utils$str <- function(x) cat(str(x), "\n")


group <- function(gs) attr(gs, "group")

group_by <- function(gs, group) {
  ## browser(expr = getOption("debug_on"))
  ## todo: make sure index is always from 1 to # levels
  ## as.factor is kind of a quick hack?
  group <- deparse(substitute(group))
  group_var <- get(group, envir = parent.frame())
  index <- as.factor(group_var) %>% as.numeric
  index_seq <- seq_len(get_n())

  indices <- lapply(unique(index), function(group) {
    index_seq[index == group]
  })
  attr(gs, "indices") <- indices

  attr(gs, "group") <- group
  gs <- set_gsim_class(gs, gsim_class(gs), grouped = TRUE)

  ## todo: next line also a hack
  attr(gs, "labels") <- unique(group_var)

  gs
}

gsim_group <- function(data, groups, type) {
  names_by_group <- lapply(groups, "[[", type)

  get_group <- function(x) {
    res <- NULL
    for (group in names(names_by_group)) {
      temp <- if (x %in% names_by_group[[group]]) group else NULL
      res <- c(res, temp)
    }
    assert_that(is.null(res) || length(res) == 1)
    res
  }

  lapply(names(data), get_group)
}


gsim_dim <- function(gs) {
  if (is.data(gs))
    dim(gs)
  else if (is.posterior(gs))
    dim(first(gs))
}


set_class <- function(x, ..., append = FALSE) {
  class <-
    if (append) 
      union(class(x), c(...))
    else
      c(...)

  class(x) <- class
  x
}


make_default_names <- function(n) {
  suffix <-
    if (n > 1)
      c("", seq_len(n)[-1])
    else
      ""
  paste0("gs", suffix)
}

set_attr <- function(x, attribute, value) {
  attr(x, attribute) <- value
  x
}

set_dim <- function(x, dims) {
  dim(x) <- dims
  x
}

set_dimnames <- function(x, names) {
  ## dots <- dots(...) %>%
  ##   vapply(identity, character(1))
  ## names <- vector("list", length(dots)) %>%
  ##   set_names(dots)
  dimnames(x) <- names
  x
}

set_last_dimnames <- function(x, nm) {
  len <- dim_length(x)
  dimnames <- vector("list", len)

  if (is.list(nm)) {
    dimnames[len] <- nm
    names(dimnames)[len] <- names(nm)
  }
  else
    dimnames[[len]] <- nm

  dimnames(x) <- dimnames
  x
}

first <- function(x) x[[1]]
last <- function(x) x[[length(x)]]
penultimate <- function(x) x[[min(length(x)-1, 1)]]
