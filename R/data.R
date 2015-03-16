#' Fetch ARM datasets
#'
#' Obtain and tidy ARM's wells and radon datasets from
#' http://github.com/stan-dev/example-models/
#'
#' @name fetch


#' @rdname fetch
#' @export
fetch_wells_data <- function() {
  check_packages("RCurl", "dplyr")

  url <- "https://raw.githubusercontent.com/stan-dev/example-models/master/ARM/Ch.7/wells.data.R"
  wells <- new.env()
  raw <- RCurl::getURL(url, .opts = list(ssl.verifypeer = FALSE))
  eval(expr = parse(text = raw), wells)

  as.data.frame(as.list(wells)) %>%
    dplyr::select(-N) %>%
    dplyr::rename(switched = switc) %>%
    dplyr::mutate(
      c_dist100 = (dist - mean(dist, na.rm = TRUE)) / 100,
      c_arsenic = arsenic - mean(arsenic, na.rm = TRUE)
    )
}

#' @rdname fetch
#' @export
fetch_radon_data <- function() {
  check_packages("RCurl", "dplyr")

  url <- "https://raw.githubusercontent.com/stan-dev/example-models/master/ARM/Ch.12/radon.data.R"
  radon <- new.env()
  raw <- RCurl::getURL(url, .opts = list(ssl.verifypeer = FALSE))
  eval(expr = parse(text = raw), radon)

  as.data.frame(as.list(radon)) %>%
    dplyr::select(-N, -J)
}
