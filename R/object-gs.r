

gs <- function(object, class, group = NULL, name = "gs") {
  assert_that(class %in% c("gsvar", "gsparam", "gsresult"))

  # gsresults are the only legit matrices in gsim?

  # Purpose of this code? Transform matrices of coefficients into
  # handy list_gs. Maybe transform them into matrix_gs instead and
  # require explicit coercion into lists?
  # Automatic simplification of one-col matrices

  if (is.matrix(object)) {
    ## todo: should not be the case anymore
    ## Remove rownames, sometimes occurring after a matrix operation
    attr(object, "dimnames")[[1]] <- NULL


    if (ncol(object) == 1) {
      ## Simplify single column matrices into vectors
      attr(object, "dim") <- NULL

    } else if (!class == "gsresult") {
      ## Simplify matrices to lists
      names <- dimnames(object)[[2]]
      if (is.null(names)) names <- list(NULL)

      object %<>% apply(1, list)           # How come?
      object <-
        Map(function(obj, ...) gs(obj[[1]], ...),
            object, class = class, name = names) %>%
        simplify_list
    }

  } else if (is.list(object) || is.list_gs(object)) {
    if (length(object) == 1) {
      object <- object[[1]]

    } else {
      names <- vapply(object, get_name, character(1)) %>%
        make_names_unique
      object <- Map(function(gs, name) set_name(gs, name), object, names)
    }
  }

  object <- structure(
             object,
             group = group,
             name = name
  )

  object <- set_gs_class(object, class, grouped = !is.null(group))
  object
}


#' @export
is.gs         <- function(x) inherits(x, "gs")

#' @export
is.grouped_gs <- function(x) inherits(x, "grouped_gs")

#' @export
is.gsvar      <- function(x) inherits(x, "gsvar")

#' @export
is.gsparam    <- function(x) inherits(x, "gsparam")

#' @export
is.gsresult   <- function(x) inherits(x, "gsresult")

#' @export
is_any.gsresult <- function(...) {
  any(vapply(list(...), is.gsresult, logical(1)))
}


## Todo?: replace set_gs_class with gsim_class<-

#' @export
gsim_class <- function(x) {
  if (is.gsparam(x)) return("gsparam")
  else if (is.gsvar(x)) return("gsvar")
  else if (is.gsresult(x)) return("gsresult")
  else if (is.numeric(x)) return("numeric")
  else stop("Not a gsim object")
}

set_gs_class <- function(gs, class, grouped = NULL) {
  class(gs) <- c("gs", class)
  if (grouped) {
    class(gs) <- c(class(gs), "grouped_gs")
  }
  if (is.list(gs)) {
    class(gs) <- c(class(gs), "list_gs")
  }
  gs
}


as.data.frame.gsvar <- function(gs, ...) {
  gs <- gs %>%
    as.data.frame.numeric %>%
    set_names(name(gs)) %>%
    cbind(obs = seq_len(length(gs)))

  gs
}

as.data.frame.gsresult <- function(gs, ...) {
  nobs <- nrow(gs)
  nsims <- ncol(gs)

  gs %>%
    as.data.frame.matrix %>%
    set_colnames(seq_len(nsims)) %>%
    cbind(obs = seq_len(nobs)) %>%
    gather_("sim", name(gs), seq_len(nsims))
}
