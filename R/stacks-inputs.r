
lock <- function(vars) {
  env <- input_env()
  env$`_locked` <- c(env$`_locked`, vars)
  NULL
}

is_locked <- function(lhs) {
  env <- input_env()
  lhs %in% env$`_locked`
}
