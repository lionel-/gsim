

as.data.frame.gs <- function(gs, ...) {
  ## if (is.seq_gs(gs))
  ##   as.data.frame.data.frame(gs, ...)

  if (is.list_gs(gs))
    as.data.frame.list_gs(gs, ...)

  else NextMethod()
}
