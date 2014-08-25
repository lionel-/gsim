

gs <- function(object, class, group = NULL, name = "gs") {
  assert_that(class %in% c("gsvar", "gsparam", "gsresult"))
  ## if (class == "gsparam") browser(expr = getOption("debug_on"))

  # gsresults are the only legit matrices in gsim?

  # Purpose of this code? Transform matrices of coefficients into
  # handy list_gs. Maybe transform them into matrix_gs instead and
  # require explicit coercion into lists?
  # Automatic simplification of one-col matrices

  if (length(dim(object)) > 3)
      stop()

  ## else if (length(dim(object)) > 1) {
  ## todo: find another place and a more general way to simplify
  ## if (is.gsvar(object) && ncol(object) == 1) {
  ##   ## Simplify single column matrices into vectors
  ##   attr(object, "dim") <- NULL
  ## }

  else if (length(dim(object)) > 1) {

    if (class %in% c("gsvar", "gsparam")) {
      ## browser(expr = getOption("debug_on"))
      ## Simplify matrices to lists
      dim_names <- dimnames(object)
      names <- last(dim_names) %||% list(NULL)

      ## objectbak <- object
      ## object <- objectbak

      ## if (class == "gsparam" && length(dim(object)) %in% c(2, 3))
      ##   object %<>% rbind_cols
      

      object %<>% apply(MARGIN = length(dim_names), list)

      ## fixme: first(obj) because apply list yields a list of list...
      object <-
        Map(function(obj, ...) gs(first(obj), ...),
            object, class = class, name = names) %>%
          simplify_list
    }

  } else if (is.list(object) || is.list_gs(object)) {
    if (length(object) == 1) {
      object <- first(object)

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
  else NULL
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
  gs %>%
    as.numeric %>%
    as.data.frame %>%
    set_names(name(gs)) %>%
    cbind(obs = seq_len(length(gs)), .) %>%
    gs_regroup(group(gs))
}

as.data.frame.gsresult <- function(gs, ...) {
  nobs <- nrow(gs)
  nsims <- ncol(gs)

  gs %>%
    as.data.frame.matrix %>%
    set_colnames(seq_len(nsims)) %>%
    cbind(obs = seq_len(nobs)) %>%
    gather_("sim", name(gs), seq_len(nsims)) %>%
    gs_regroup(group(gs))
}


gs_regroup <- function(res, group) {
  if (!is.null(group)) {
    ## group is as.vector'd to convert it to character or numeric
    res <- cbind(as.vector(dyn_get(group)), res)
    names(res)[1] <- group
    res %<>% regroup(list(group))
  }
  res
}
