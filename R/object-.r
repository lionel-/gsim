

as.data.frame.gs <- function(gs, ...) {
  if (is.seq_gs(gs))
    as.data.frame.seq_gs(gs, ...)

  else if (is.list_gs(gs))
    as.data.frame.list_gs(gs, ...)

  else NextMethod()
}
