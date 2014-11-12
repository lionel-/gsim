

call_stack <- container_getter("_call_stack")
reactive_stack <- container_getter("_reactive_stack")
reactive_recycling_stack <- container_getter("_reactive_recycling_stack")

add_to_reactive_stack <- function(lhs, rhs) {
  inputs <- attr(rhs, "input_names")
  rhs_names <- find_names(rhs)
  rhs_names <- rhs_names[rhs_names %in% reactive_lhs()]

  
  call <- reactive(
    call("<-", as.name(lhs), rhs),
    input_names = inputs,
    deps = rhs_names
  )

  env <- container_env()
  env$`_reactive_stack` <- c(env$`_reactive_stack`, call)

  NULL
}

add_to_call_stack <- function(lhs, rhs) {
  browser(expr = getOption("debug_on"))
  call <- call("<-", as.name(lhs), rhs)

  env <- container_env()
  env$`_call_stack` <- c(env$`_call_stack`, call)

  NULL
}

clear_call_stack <- function() {
  env <- container_env()
  env$`_call_stack` <- list()
  invisible(NULL)
}
