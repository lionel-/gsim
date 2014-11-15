
reactive_lhs_list <- context_getter("reactive_lhs_list")

lock <- function(lhs, dependencies, inputs) {
  new <- reactive(lhs, inputs)

  dependencies <- dependencies[dependencies %in% reactive_lhs_list()]

  context <- context()
  context$reactive_lhs <- unique(c(context$reactive_lhs, list(new)))
  context$locked <- unique(c(context$locked, lhs, dependencies))

  NULL
}

is.locked <- function(var) {
  var <- as.character(var)
  context <- context()
  var %in% context$locked
}

is.reactive_lhs <- function(lhs) {
  lhs <- as.character(lhs)
  lhs %in% reactive_lhs_list()
}
