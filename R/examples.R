#' Example datasets and models
#'
#' Obtain and tidy ARM's wells and radon datasets from
#' http://github.com/stan-dev/example-models/
#'
#' @name examples
NULL


#' @rdname examples
#' @export
wells_data <- function() {
  check_packages("httr", "dplyr")

  url <- "https://raw.githubusercontent.com/stan-dev/example-models/master/ARM/Ch.7/wells.data.R"
  wells <- new.env()
  raw <- httr::GET(url) %>% httr::content(as = "text")
  eval(expr = parse(text = raw), wells)

  as.data.frame(as.list(wells)) %>%
    dplyr::tbl_df() %>%
    dplyr::select(-N) %>%
    dplyr::rename(switched = switc) %>%
    dplyr::mutate(
      c_dist100 = (dist - mean(dist, na.rm = TRUE)) / 100,
      c_arsenic = arsenic - mean(arsenic, na.rm = TRUE)
    )
}

#' @rdname examples
#' @export
radon_data <- function() {
  check_packages("RCurl", "dplyr")

  url <- "https://raw.githubusercontent.com/stan-dev/example-models/master/ARM/Ch.12/radon.data.R"
  radon <- new.env()
  raw <- httr::GET(url) %>% httr::content(as = "text")
  eval(expr = parse(text = raw), radon)

  as.data.frame(as.list(radon)) %>%
    dplyr::tbl_df() %>%
    dplyr::select(-N, -J)
}

#' @rdname examples
#' @export
radon_model <- function(language = "stan", variant = "centered",
                        ref = FALSE) {
  file <- paste0("radon-", variant, ".", language)
  file <- system.file(paste0("extdata/models/", file), package = "gsim")
  if (file == "") {
    stop("Unsupported language/variant pair", call. = FALSE)
  }

  if (ref) {
    file
  } else {
    readChar(file, file.info(file)$size)
  }
}
