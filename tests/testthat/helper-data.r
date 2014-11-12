
get_wells_data <- function() {
  if (!requireNamespace("RCurl", quietly = TRUE)
        || !requireNamespace("dplyr", quietly = TRUE))
    stop("RCurl and dplyr need to be installed", call. = FALSE)
  
  url <- "https://raw.githubusercontent.com/stan-dev/example-models/master/ARM/Ch.7/wells.data.R"
  wells <- new.env()
  eval(expr = parse(text = RCurl::getURL(url)), wells)

  as.data.frame(as.list(wells)) %>%
    dplyr::select(-N) %>%
    dplyr::rename(switched = switc) %>%
    dplyr::mutate(
      c_dist100 = (dist - mean(dist, na.rm = TRUE)) / 100,
      c_arsenic = arsenic - mean(arsenic, na.rm = TRUE)
    )
}

wells <- get_wells_data()
n_sims <- 100

wells_fit <- glm(switched ~ c_dist100 * c_arsenic, binomial, wells)
arm_sims <- arm::sim(wells_fit, n.sims = n_sims)
clean_sims <- gsim(arm_sims, wells)

get_clean_sims <- function() {
  gsim(arm_sims, wells)
}

X <- wells %$% cbind(intercept(), c_dist100, c_arsenic, c_dist100 * c_arsenic)
