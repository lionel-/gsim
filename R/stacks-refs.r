
add_ref <- function(x, is_input = FALSE) {
  stack <- storage("_ref_stack")

  id <- last(stack) + 1
  ref <-
    if (is_input)
      paste0("_input_ref_", x)
    else
      paste0("_ref", id)

  assign_in_storage(ref, x)
  assign_in_storage("_ref_stack", c(stack, id))

  if (is_input)
    structure(
      as.name(ref),
      class = "reactive",
      input_names = x
    )
  else
    as.name(ref)
}

clear_refs <- function(x) {
  ids <- find_refs(x) %>% unlist(use.names = FALSE)
  if (!is.null(ids)) {
    env <- input_env()
    for (i in seq_along(ids)) {
      ref <- paste0("_ref", ids[i])
      rm(list = ref, envir = env)
    }
  }
}

find_refs <- function(x) {
  if (is.name(x)) {
    name <- deparse(x)
    if (substring(name, 1, 4) == "_ref")
      substring(name, 5)
    else
      NULL
  }

  else if (is.call(x))
    lapply(x, find_refs)

  else
    NULL
}
