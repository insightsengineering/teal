# These functions return dummy values to provide good values for the examples of other non-exported functions.
# They should not be exported.

#' Get dummy filter states to apply initially
#'
#' This can be used for the argument `filter` in \code{\link{srv_teal}}.
#' @return dummy filter states
get_dummy_filter <- function() { # nousage # nolint
  res <- list(
    ADSL = list(SEX = list(choices = "M", keep_na = TRUE), AGE = default_filter()),
    ADLB = list(ASEQ = default_filter())
  )
  return(res)
}

#' Get dummy CDISC data
#'
#' Get dummy CDISC data including `ADSL`, `ADAE` and `ADLB`.
#' Some NAs are also introduced to stress test.
#'
#' @return `cdisc_data`
get_dummy_cdisc_data <- function() { # nousage # nolint
  teal_with_pkg("random.cdisc.data", code = {
    ADSL <- random.cdisc.data::radsl(cached = TRUE) # nolint
    ADAE <- random.cdisc.data::radae(cached = TRUE) # nolint
    ADLB <- random.cdisc.data::radlb(cached = TRUE) # nolint
  })

  ADSL$logical_test <- sample(c(TRUE, FALSE, NA), size = nrow(ADSL), replace = TRUE)
  ADSL$SEX[1:150] <- NA

  res <- cdisc_data(
    cdisc_dataset(dataname = "ADSL", x = ADSL),
    cdisc_dataset(dataname = "ADAE", x = ADAE),
    cdisc_dataset(dataname = "ADLB", x = ADLB),
    code = "
ADSL <- radsl(cached = TRUE)
ADAE <- radae(cached = TRUE)
ADLB <- radlb(cached = TRUE)
")
  return(res)
}

#' Get a dummy `datasets` object with `ADSL` data, useful in the examples
#'
#' Returns a new `R6` object on each invocation, not a singleton.
#' @return `FilteredData` with `ADSL` set
get_dummy_datasets <- function() { # nousage # nolint
  dummy_cdisc_data <- get_dummy_cdisc_data()
  datasets <- filtered_data_new(dummy_cdisc_data)
  isolate({
    filtered_data_set(dummy_cdisc_data, datasets)
  })
  return(datasets)
}

#' Get dummy modules
#'
#' Create an example hierarchy of `teal_modules` from which
#' a teal app can be created.
#'
#' @importFrom shiny tags
#' @return `teal_modules`
get_dummy_modules <- function() { # nousage # nolint
  create_mod <- function(module_name) module(
    module_name,
    server = function(input, output, session, datasets) {

    },
    ui = function(id, ...) {
      tags$p(paste0("id: ", id))
    },
    filters = "all"
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
