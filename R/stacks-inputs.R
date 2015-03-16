
reactive_lhs_list <- context_getter("reactive_lhs_list")

lock <- function(lhs, dependencies, inputs) {
  # FIXME: safe to remove?
  lhs <- reactive(lhs, inputs)

  dependencies <- dependencies[dependencies %in% reactive_lhs_list()]

  reactive_lhs_list <- unique(c(context("reactive_lhs_list"), list(lhs)))
  locked <- unique(c(context("locked"), lhs, dependencies))

  assign_in_context("reactive_lhs_list", reactive_lhs_list)
  assign_in_context("locked", locked)
  NULL
}

is.locked <- function(var) {
  var <- as.character(var)
  var %in% context("locked")
}
