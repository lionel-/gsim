
reactive_lhs <- container_getter("_reactive_lhs")

lock <- function(lhs, dependencies, inputs) {
  new <- reactive(lhs, inputs)

  dependencies <- dependencies[dependencies %in% reactive_lhs()]

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
  lhs %in% reactive_lhs()
}
