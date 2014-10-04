

gsim_process <- function(mclist, data = NULL, groups = NULL) {
  if (first(mclist) %>% is.list)
    mclist <- mcmc_collapse(mclist)

  nobs <- nrow(data)
  nsims <- first(dim(first(mclist)))

  sim_groups  <- gsim_group(mclist, groups, type = "params")
  mclist <- Map(gs, mclist, class = "posterior", group = sim_groups)

  if (!is.null(data)) {
    data_groups <- gsim_group(data, groups, type = "vars")

    ## Reduce grouped variables to a vector with one value per group
    reduce <- function(x, y) {
      if (!is.null(y)) {
        x <- unname(tapply(x, data[[y]], unique, simplify = TRUE))
      }
      as.vector(x)
    }
    data <- Map(reduce, data, data_groups)

    ## todo: get rid of colnames arg, currently necessary for data objects
    data   <- Map(gs, data,   class = "data",      group = data_groups, colnames = names(data))
  }

  list(data = data, mclist = mclist)
}


#' @export
gsim_assign <- function(data, mclist, groups = NULL) {
  sim <- gsim_process(data, mclist, groups)

  env <- parent.frame()
  for (i in seq_along(sim$mclist)) {
    assign(names(sim$mclist)[i], sim$mclist[[i]], envir = env)
  }
  for (i in seq_along(sim$data)) {
    assign(names(sim$data)[i], sim$data[[i]], envir = env)
  }
}


#' @export
gsim_attach <- function(data, mclist, groups = NULL) {
  # todo: check that ..gsim.. is not already in search path

  sim <- gsim_process(data, mclist, groups) %>%
    flatten

  nobs <- nrow(data)
  nsims <- dim(mclist[[1]])[2] %>% unname

  sim$`..n..` <- nobs
  sim$`..nsims..` <- nsims

  attach(sim, name = "..gsim..")
}

#' @export
gsim_detach <- function() {
  detach("..gsim..")
}


flatten <- function(x) {
  x %<>% unlist(recursive = FALSE)
  names(x) %<>% str_replace("^data\\.|^mclist\\.", "")
  x
}

