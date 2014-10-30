
reactive_lhs <- container_getter("_reactive_lhs")

lock <- function(lhs, dependencies, inputs) {
  new <- reactive(lhs, inputs)

  dependencies <- dependencies[dependencies %in% reactive_lhs()]

  env <- container_env()
  env$`_reactive_lhs` <- unique(c(env$`_reactive_lhs`, list(new)))
  env$`_locked` <- unique(c(env$`_locked`, lhs, dependencies))

  NULL
}

is.locked <- function(var) {
  var <- as.character(var)
  env <- input_env()
  var %in% env$`_locked`
}

is.reactive_lhs <- function(lhs) {
  lhs <- as.character(lhs)
  lhs %in% reactive_lhs()
}
