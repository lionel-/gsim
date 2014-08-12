

#' @export
nobs.gs <- function(gs) {
  n <- attr(gs, "nobs")
  if (is.null(n))
    stop("invalid nobs attribute")
  else n
}

#' @export
nsims <- function(gs) {
  n <- attr(gs, "nsims")
  if (is.null(n))
    stop("invalid nobs attribute")
  else n
}


get_n <- function(n_up = 1) get("..n..", envir = parent.frame(n_up + 1))
get_nsims <- function(n_up = 1) get("..nsims..", envir = parent.frame(n_up + 1))


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


as.data.frame.gs <- function(gs, ...) {
  if (is.vec_gs(gs))
    as.data.frame.vec_gs(gs, ...)
  else if (is.gsresult(gs))
    as.data.frame.gsresult(gs, ...)
}

as.data.frame.gsresult <- function(gs, ...) {
  nobs <- nrow(gs)
  nsims <- ncol(gs)

  gs %>%
    as.data.frame.matrix %>%
    set_colnames(seq_len(nsims)) %>%
    cbind(obs = seq_len(nobs)) %>%
    gather_("sim", name(gs), seq_len(nsims))
}


name <- function(gs) {
  name <- attr(gs, "name", exact = TRUE)
  if (is.null(name)) "gs"
  else name
}

#' @export
set_name <- function(x, name) {
  UseMethod("set_name")
}

set_name.gs <- function(gs, name) {
  attr(gs, "name") <- name
  gs
}

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
