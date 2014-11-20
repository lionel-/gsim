
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

x <- function() input("x")
y <- function() input("y")
z <- function() input("z")

is.reactive <- function(x) inherits(x, "reactive")

is.reactive_lhs <- function(lhs) {
  length(lhs == 1) && {
    lhs <- as.character(lhs)
    lhs %in% reactive_lhs_list()
  }
}

is.input <- function(x) {
  is.call(x) && as.character(x[[1]]) %in% c("x", "y", "z", "input")
}

input_names <- function(x) attr(x, "input_names")

clear_reactive_data <- function() {
  assign_in_context("reactive_stack", list())
  assign_in_context("reactive_lhs_list", list())
  lapply(context("locked"), function(x) {
    assign_in_storage(x, NULL)
  })
  assign_in_context("locked", NULL)
  invisible(NULL)
}
