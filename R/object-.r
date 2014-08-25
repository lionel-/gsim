
`$.gs` <- function(gs, name) {
  if (is.list_gs(gs))
    gs$name

  else if (!attr(gs, "blocks_names") %>% is.null) {
    class <- gsim_class(gs)

    index <- block_index(gs, name)
    if (index %>% is.null)
      return(NULL)

    if (is.gsvar(gs))
      gs <- gs[index]
    else
      gs <- gs[, index]

    gs(gs, class, name = name)
  }
}



as.data.frame.gs <- function(gs, ...) {
  ## if (is.seq_gs(gs))
  ##   as.data.frame.data.frame(gs, ...)

  if (is.list_gs(gs))
    as.data.frame.list_gs(gs, ...)

  else NextMethod()
}
