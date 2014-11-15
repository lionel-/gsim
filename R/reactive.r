

## curve (invlogit (cbind (1, x/100, .5) %*% coef(fit.3)), add=TRUE)

reactive <- function(call, input_names, deps = NULL, ...) {
  old_class <- class(call)

  structure(
    call,
    class = unique(c("reactive", old_class)),
    input_names = input_names,
    deps = deps
  )
}

input <- function(name) {
  stopifnot(is.character(name))
  name
}

is.reactive <- function(x) inherits(x, "reactive")
is.input <- function(x) {
  is.call(x) && as.character(x[[1]]) %in% c("x", "y", "z", "input")
}

input_names <- function(x) attr(x, "input_names")

x <- function() input("x")
y <- function() input("y")
z <- function() input("z")


is.reactive_fun <- function(x) inherits(x, "reactive_fun")
