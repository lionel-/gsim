
## Need to adapt: operators, cbind and maybe c
## -> Create a "gs_sequenced" object.
##    Then apply sequenced logic

## Need recycling in cbind...

## curve (invlogit (cbind (1, x/100, .5) %*% coef(fit.3)), add=TRUE)


vseq <- function(gs = NULL, range = NULL, name = NULL, n = 101) {
  if (is.null(gs) && is.null(range))
    stop("Need either a gs or a range")
  stopifnot(is.gsvar(gs))

  if (is.null(name))
    name <- name(gs)
  if (is.null(range))
    range <- range(gs)
  

  res <- vector("list", 1)
  names(res) <- name

  attr(res, "seq") <- list()
  attr(res, "seq")[[name]] <- seq(range[1], range[2], length.out = n)

  class(res) <- "vseq"
  res
}


is.vseq <- function(x) inherits(x, "vseq")
is.gs_sequenced <- function(x) inherits(x, "gs_sequenced")
