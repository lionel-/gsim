

to_recycle <- function(x) {
  env <- input_env()
  stack <- env$`_ref_stack`
  id <- last(stack) + 1

  if (is.input(x)) {
    inputs <- eval(x)
    ref <- paste0("_input_ref_", inputs)
  }
  else {
    inputs <- input_names(x)
    ref <- paste0("_ref", id)
  }

  class <-
    if (is.input(x))
      "reactive"
    else if (!is.null(inputs))
      c("to_recycle", "reactive")
    else
      "to_recycle"

  env[[ref]] <- x
  env$`_ref_stack` <- c(stack, id)

  structure(
    as.name(ref),
    class = class,
    input_names = inputs
  )
}

add_ref <- function(x) {
  env <- input_env()
  stack <- env$`_ref_stack`

  id <- last(stack) + 1
  ref <- paste0("_ref", id)

  env[[ref]] <- x
  env[["_ref_stack"]] <- c(stack, id)

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
