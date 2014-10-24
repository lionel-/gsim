

make_ref_call <- function(id) as.name(paste0("_ref", id))

add_ref <- function(x) {
  env <- input_env()
  stack <- env$`_ref_stack`
  id <- last(stack) + 1
  env[[paste0("_ref", id)]] <- x
  env$`_ref_stack` <- c(stack, id)
  id
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
