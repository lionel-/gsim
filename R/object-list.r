
## #' @param ... objects to concatenate
#' @export
make_list <- function(...) {
  args <- extract_dots(..., simplify = TRUE)

  ## ## If args simplified, can't work with numeric vectors to check class
  ## ## If not, can't work with lists of gs...
  ## class <- gsim_class(args[[1]])
  ## same_type <- lapply(args, gsim_class) == class
  ## stopifnot(all(same_type))

  gs(args, "gsvar")
}


as.data.frame.list_gs <- function(gs, ...) {
  ## Todo: very fishy
  ## What did I mean by grouped list back then??

  ## ## Remove attributes to prevent a tidyr warning
  ## res <- lapply(gs, function(col) {
  ##   attr(col, "name") <- NULL
  ##   col
  ## })

  if (is.gsparam(gs)) {
    res <- as.data.frame.list(res) %>%
      set_names(names(gs))

    if (is.grouped_gs(gs)) {
      group <- group(gs)
      nsims <- get_nsims()

      res <- gather_(res, group, name(gs), names(gs)) %>%
        regroup(list(group)) %>%
        mutate(sim = seq_len(nsims))
      res[group] <- factor(res[[group]], sort(unique(as.character(res[[group]]))))
    }

  } else if (is.gsvar(gs)) {
    if (is.grouped_gs(gs)) {
      res <- as.data.frame.list(gs) %>%
        gather_(group(gs), name(gs), seq_len(length(gs)))

    } else {
      names <- vapply(gs, name, character(1)) %>%
        make_names_unique
      res <- lapply(gs, function(item) `class<-`(item, "numeric")) %>%
        as.data.frame.list %>%
        set_names(names)
    }
  }

  res <- cbind(res, obs = seq_len(nrow(res)))
  res
}


#' @export
is.list_gs <- function(x) inherits(x, "list_gs")


## #' @export
## `[.list_gs` <- function(x, ...) {
##   class <- class(x)
##   class(x) <- "list"
##   x <- x[...]
##   class(x) <- class
##   x
## }
