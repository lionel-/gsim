
add_to_reactive_stack <- function(lhs, rhs) {
  if (is.character(lhs))
    lhs <- as.name(lhs)

  inputs <- attr(rhs, "input_names")
  rhs_names <- find_names(rhs)
  rhs_names <- rhs_names[rhs_names %in% reactive_lhs_list()]

  call <- reactive(
    call("<-", lhs, rhs),
    input_names = inputs,
    deps = rhs_names
  )

  new_stack <- c(context("reactive_stack"), call)
  assign_in_context("reactive_stack", new_stack)

  NULL
}

add_to_call_stack <- function(lhs, rhs) {
  if (is.character(lhs))
    lhs <- as.name(lhs)

  call <- call("<-", lhs, rhs)
  new_stack <- c(context("call_stack"), call)
  assign_in_context("call_stack", new_stack)
  NULL 
}

clear_call_stack <- function() {
  assign_in_context("call_stack", list())
  invisible(NULL)
}
