
overrides <- list()


overrides$class <- function(x) {
  cat(class(x), "\n")
}


overrides$str <- function(x) {
  cat(str(x), "\n")
}


## overrides$list <- function(...) {
##   dots <- list(...)
##   expr <- substitute(alist(...))
##   expr <- as.list(expr[-1])

##   names <- names(dots) %||% rep("", length(dots))
##   named <- papply(names, function(name) !name == "")
##   names[!named] <- lapply(expr[!named], function(item) {
##     name <- as.character(item)
##     name <- setdiff(name, c("I", "P"))
##     name <- do.call(paste, c(as.list(name), list(sep = "_")))
##     name
##   })

##   names(expr) <- names
##   do.call(list, expr, envir = list2env(storage()))
## }


overrides$check_bernoulli <- function(y, p, stat) {
  stop("Don't work inside gsim yet")
  ## expr <- bquote(bernoulli_check(y, p, .(storage()), stat))
  ## eval(expr, envir = environment(), enclos = eval_env())
}

overrides$check_normal <- function(y, mu, sigma, stat) {
  stop("Don't work inside gsim yet")
}

overrides$check_model <- function(y, mu, sigma, stat) {
  stop("Don't work inside gsim yet")
}
