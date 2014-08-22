

gsim_process <- function(data, mclist, groups = NULL) {

  # todo: detect when mclist has been collapsed AND simplified
  if (length(dim(mclist[[1]])) == 3)
    mclist <- mcmc_collapse(mclist, simplify = FALSE)

  nobs <- nrow(data)
  nsims <- unname(dim(mclist[[1]])[2])

  data_groups <- get_gs_group(data, groups, type = "vars")
  mclist      <- lapply(mclist, subset_mcarray, drop = TRUE)
  sim_groups  <- get_gs_group(mclist, groups, type = "params")

  ## Reduce grouped variables to a vector with one value per group
  reduce <- function(x, y) {
    if (!is.null(y)) {
      x <- unname(tapply(x, data[[y]], unique, simplify = TRUE))
    }
    as.vector(x)
  }
  data <- Map(reduce, data, data_groups)

  data   <- Map(gs, data,   class = "gsvar",   group = data_groups, name = names(data))
  mclist <- Map(gs, mclist, class = "gsparam", group = sim_groups,  name = names(mclist))

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

