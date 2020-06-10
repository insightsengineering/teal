# These functions return dummy values to work with the examples
# todo: should these functions be exported

#' Get dummy filter states to apply initially
#'
#' This can be used for the argument `initial_filter_states` in
#' `\link{srv_teal}`.
#' @md
#' @return dummy filter states
get_dummy_filter_states <- function() {
  return(list(
    ADSL = list(SEX = list(choices = "M", keep_na = TRUE), AGE = "default"),
    ADLB = list(ASEQ = "default")
  ))
}

#' Get dummy CDISC data
#'
#' Get dummy CDISC data including `ADSL`, `ADAE` and `ADLB`.
#' Some NAs are also introduced to stress test.
#'
#' @md
#' @return `cdisc_data`
get_dummy_cdisc_data <- function() {
  library(random.cdisc.data) # needed for cached data
  ADSL <- random.cdisc.data::radsl(cached = TRUE)
  ADAE <- random.cdisc.data::radae(cached = TRUE)
  ADLB <- random.cdisc.data::radlb(cached = TRUE)

  ADSL$logical_test <- sample(c(TRUE, FALSE, NA), size = nrow(ADSL), replace = TRUE)
  ADSL$SEX[1:150] <- NA

  return(cdisc_data(
    cdisc_dataset(dataname = "ADSL", data = ADSL),
    cdisc_dataset(dataname = "ADAE", data = ADAE),
    cdisc_dataset(dataname = "ADLB", data = ADLB),
    code = "
ADSL <- radsl(cached = TRUE)
ADAE <- radae(cached = TRUE)
ADLB <- radlb(cached = TRUE)
"))
}

#' Get a dummy `datasets` object with `ADSL` data, useful in the examples
#'
#' @md
#' @return `FilteredData` with `ADSL` set
get_dummy_datasets <- function() {
  # todo: require package
  library(random.cdisc.data) # otherwise cached data not available, todo: why?
  ADSL <- random.cdisc.data::radsl(cached = TRUE)
  attr(ADSL, "keys") <- get_cdisc_keys("ADSL")
  datasets <- FilteredData$new()

  isolate({
    datasets$set_data("ADSL", ADSL)
  })
  return(datasets)
}

#' Get dummy modules
#'
#' Create an example hierarchy of `teal_modules` from which
#' a teal app can be created.
#'
#' @md
#' @return `teal_modules`
get_dummy_modules <- function() {
  create_mod <- function(module_name) module(
    module_name,
    server = function(input, output, session, datasets) {},
    ui = function(id, ...) { tags$p(paste0("id: ", id)) },
    filters = 'all'
  )
  mods <- modules(
    "d1",
    modules(
      "d2",
      modules(
        "d3",
        create_mod("aaa1"), create_mod("aaa2"), create_mod("aaa3")
      ),
      create_mod("bbb")
    ),
    create_mod("ccc")
  )
  return(mods)
}
