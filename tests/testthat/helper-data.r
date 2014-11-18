
if (!requireNamespace("RCurl", quietly = TRUE)
    || !requireNamespace("dplyr", quietly = TRUE))
  stop("RCurl and dplyr need to be installed", call. = FALSE)

get_wells_data <- function() {
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

get_radon_data <- function() {
  url <- "https://raw.githubusercontent.com/stan-dev/example-models/master/ARM/Ch.12/radon.data.R"
  radon <- new.env()
  eval(expr = parse(text = RCurl::getURL(url)), radon)

  as.data.frame(as.list(radon)) %>%
    dplyr::select(-N, -J)
}

n_sims <- 100

wells <- get_wells_data()
wells_fit <- wells %$% glm(switched ~ c_dist100 * c_arsenic, binomial)
X <- wells %$% cbind(intercept(), c_dist100, c_arsenic, c_dist100 * c_arsenic)

arm_sims <- arm::sim(wells_fit, n.sims = n_sims)
new_sims <- gsim(arm_sims, wells)

radon <- get_radon_data()

skip_heavy_computations <- TRUE
