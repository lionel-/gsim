

indices <- function(gs) attr(gs, "indices", exact = TRUE)
labels <- function(gs) attr(gs, "labels", exact = TRUE)


sample_nsims <- function(data, nsims, n = 5) {
  extract(data, , sample(seq(nsims), n))
}


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


get_name <- function(gs) {
  name <- attr(gs, "name", exact = TRUE)
  if (is.null(name)) "gs"
  else name
}
## name <- get_name


#' @export
`name<-` <- function(gs, value) {
  attr(gs, "name") <- value
  gs
}

set_name <- function(x, name) {
  UseMethod("set_name")
}

set_name.gs <- `name<-`
set_name.numeric <- `name<-`

#' @export
set_name.data.frame <- function(df, name) {
  if (is.null(df["gs"]))
    stop("column \"gs\" not found")
  else names(df)[match("gs", names(df))] <- name
  df
}


utils <- list()

utils$class <- function(x) cat(class(x), "\n")
utils$str <- function(x) cat(str(x), "\n")



make_names_unique <- function(names) {
  # todo: make gs1 = gs2 when there are two gs
  make.unique(names, sep = "")
}



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
