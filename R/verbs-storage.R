#' Assemble variables into a (design) matrix
#'
#' \code{design()} works like dplyr's function
#' \code{data_frame()}. New columns can be defined in terms of
#' previous columns.
#'
#' It converts factor variables to columns of indicators. It supports
#' the \code{:} operator to interact predictors. Only variables of
#' length 1 are recycled.
#' @param ... Named arguments.
#' @param .unjoin When a vector identifying groups, and if the
#'   variables in \code{...} are unique within groups, the design
#'   matrix is reduced to one value per group.
#' @export
design <- function(..., .unjoin = NULL) {
  dots <- lazyeval::lazy_dots(...)
  out <- store_(dots, list(`:` = interact)) %>% recycle()
  out <- map_if(out, is.integer, as.double)

  if (is.character(.unjoin) && length(.unjoin) == 1) {
    .unjoin <- get(.unjoin, envir = parent.frame())
  }

  if (!is.null(.unjoin)) {
    out <- dplyr::as_data_frame(out)
    out %>%
      cbind(`slice_id__` = .unjoin) %>%
      slice_rows("slice_id__") %>%
      by_slice(function(slice) {
        n_unique_cols <- map_dbl(slice, dplyr::n_distinct)
        if (!all(n_unique_cols == 1)) {
          stop("values are not unique within groups", call. = FALSE)
        }
      })
    out <- dplyr::distinct_(out)
  }

  map_call(out, as_matrix) %>%
    structure(class = c("design", "matrix"))
}

interact <- function(...) {
  dots <- list(...)
  stopifnot(every(dots, is.numeric))

  # Fall back to ordinary `:` operator when two digits are given as
  # arguments
  if (length(dots) == 2 && every(dots, function(dot) length(dot) == 1)) {
    return(map_call(dots, base::`:`))
  }

  interact_binary <- function(x, y) {
    x <- x - min(x, na.rm = TRUE) + 1
    y <- y - min(y, na.rm = TRUE) + 1

    (x * n_unique(y) + y) - n_unique(y)
  }

  Reduce(interact_binary, dots)
}

recycle <- function(x) {
  lengths <- lapply(x, NROW)
  n <- do.call("max", lengths)
  stopifnot(all(lengths %in% c(1, n)))
  x <- map_if(x, lengths == 1, rep, n)
  x
}

print.design <- function(x, ...) {
  stop("todo")
}

#' Assemble a list of vectors and matrices into a matrix
#'
#' \code{as_matrix()} binds columns into a matrix. These columns can
#' come in the form of vectors or of a list or a data frame of
#' vectors.
#' @param ... Vectors and/or lists of vectors.
#' @export
#' @examples
#' vectors <- mtcars[3:4]
#' as_matrix(vectors, mpg = mtcars$mpg, vectors, mtcars$am)
#'
#' # If applied on an empty list or without arguments, as_matrix()
#' # returns a 0x0 matrix:
#' matrix()
as_matrix <- function(...) {
  dots <- map_if(list(...), is.data.frame, c)
  dots <- splice(dots)
  as_matrix_impl(dots)
}

#' Create a list
#'
#' This function is analog to \code{\link[dplyr]{data_frame}()} from
#' the dplyr package but creates a list instead of a data frame. It
#' evaluates its elements lazily and sequentially.
#' @param List elements.
#' @export
store <- function(...) {
  store_(lazyeval::lazy_dots(...))
}

#' @rdname store
#' @export
store_ <- function(dots, data = list()) {
  n <- length(dots)
  dots <- lazyeval::auto_name(dots)

  out <- vector("list", n)
  names(out) <- character(n)
  for (i in seq_len(n)) {
    out[[i]] <- lazyeval::lazy_eval(dots[[i]], c(data, out))
    names(out)[i] <- names(dots)[i]
  }
  out
}

#' Define or redefine elements of a list
#'
#' This function takes a list of objects or a data frame and changes
#' their contents. It discards all elements that are not mentioned in
#' \code{...}, leaving a clean list of inputs for your models.
#' @param .data A list or data frame.
#' @param ... Variables to keep and new definitions.
#' @export
define <- function(.data, ...) {
  define_(.data, .dots = lazyeval::lazy_dots(...))
}

#' @rdname define
#' @export
define_ <- function (.data, ..., .dots) {
  UseMethod("define_")
}

#' @rdname define
#' @export
define_.list <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  for (i in seq_along(dots)) {
    # Use list-preserving subsetting so that NULL results are
    # correctly recorded in the output list
    .data[names(dots)[i]] <- list(lazyeval::lazy_eval(dots[[i]], .data))
  }
  .data[names(dots)]
}

#' @rdname define
#' @export
define_.data.frame <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  define_(c(.data), .dots = dots)
}
