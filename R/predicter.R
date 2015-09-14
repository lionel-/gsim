# Create a predicter function
#
# @export
as_predicter <- function(.x, ...) {
  UseMethod("as_predicter")
}

# @rdname as_predicter
# @export
as_predicter.sims <- function(.x, .f, .data = list(), ...) {
  if (!is.function(.f)) {
    stop(".f must be a function", call. = FALSE)
  }

  f <- function() {}
  body(f) <- body(.f)
  f <- adjust_signature(f, .x)

  args <- formals(.f)
  out <- function() {
    data <- names(args) %>% mget(envir = parent.env(environment()))
    by_sim(.x, f, .context = c(data, .data))
  }
  formals(out) <- args
  environment(f) <- environment(out)

  out
}
