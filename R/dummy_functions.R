# These functions return dummy values to provide good values for the examples of other non-exported functions.
# They should not be exported.

#' Get dummy filter states to apply initially
#'
#' This can be used for the argument `filter` in [`srv_teal`].
#'
#' @param data (`TealData`)
#' @return dummy filter states
#' @keywords internal
example_filter <- function(data) { # nolint
  ADSL <- teal.data::get_raw_data(x = data, dataname = "ADSL") # nolint
  ADLB <- teal.data::get_raw_data(x = data, dataname = "ADLB") # nolint

  res <- list(
    ADSL = list(
      filter = list(
        list(
          SEX = teal.slice:::init_filter_state(
            x = ADSL$SEX,
            varname = "SEX",
            varlabel = "Sex"
          ),
          AGE = teal.slice:::init_filter_state(
            x = ADSL$AGE,
            varname = "AGE",
            varlabel = "Age"
          )
        )
      )
    ),
    ADLB = list(
      filter = list(
        list(
          ASEQ = teal.slice:::init_filter_state(
            x = ADLB$ASEQ,
            varname = "ASEQ",
            varlabel = "Sequence Number"
          )
        )
      )
    )
  )

  return(res)
}

#' Get dummy `CDISC` data
#'
#' Get dummy `CDISC` data including `ADSL`, `ADAE` and `ADLB`.
#' Some NAs are also introduced to stress test.
#'
#' @return `cdisc_data`
#' @keywords internal
example_cdisc_data <- function() { # nolint
  ADSL <- data.frame( # nolint
    STUDYID = "study",
    USUBJID = 1:10,
    SEX = sample(c("F", "M"), 10, replace = TRUE),
    AGE = stats::rpois(10, 40)
  )
  ADTTE <- rbind(ADSL, ADSL, ADSL) # nolint
  ADTTE$PARAMCD <- rep(c("OS", "EFS", "PFS"), each = 10) # nolint
  ADTTE$AVAL <- c( # nolint
    stats::rnorm(10, mean = 700, sd = 200), # dummy OS level
    stats::rnorm(10, mean = 400, sd = 100), # dummy EFS level
    stats::rnorm(10, mean = 450, sd = 200) # dummy PFS level
  )

  ADSL$logical_test <- sample(c(TRUE, FALSE, NA), size = nrow(ADSL), replace = TRUE) # nolint
  ADSL$SEX[c(2, 5)] <- NA # nolint

  cdisc_data_obj <- teal.data::cdisc_data(
    cdisc_dataset(dataname = "ADSL", x = ADSL),
    cdisc_dataset(dataname = "ADTTE", x = ADTTE)
  )

  res <- teal.data::cdisc_data(
    teal.data::cdisc_dataset(dataname = "ADSL", x = ADSL),
    teal.data::cdisc_dataset(dataname = "ADTTE", x = ADTTE),
    code = '
      ADSL <- data.frame(
        STUDYID = "study",
        USUBJID = 1:10,
        SEX = sample(c("F", "M"), 10, replace = TRUE),
        AGE = rpois(10, 40)
      )
      ADTTE <- rbind(ADSL, ADSL, ADSL)
      ADTTE$PARAMCD <- rep(c("OS", "EFS", "PFS"), each = 10)
      ADTTE$AVAL <- c(
        rnorm(10, mean = 700, sd = 200),
        rnorm(10, mean = 400, sd = 100),
        rnorm(10, mean = 450, sd = 200)
      )

      ADSL$logical_test <- sample(c(TRUE, FALSE, NA), size = nrow(ADSL), replace = TRUE)
      ADSL$SEX[c(2, 5)] <- NA
    '
  )
  return(res)
}

#' Get datasets to go with example modules.
#'
#' Creates a nested list, the structure of which matches the module hierarchy created by `example_modules`.
#' Each list leaf is a `FilteredData` objectst, a deep clone of one original one.
#'
#' @return named list of `FilteredData` objects, each with `ADSL` set.
#' @keywords internal
example_datasets <- function() { # nolint
  dummy_cdisc_data <- example_cdisc_data()
  dataset <- teal.slice::init_filtered_data(dummy_cdisc_data)
  list(
    "d2" = list(
      "d3" = list(
        "aaa1" = dataset$clone(deep = TRUE),
        "aaa2" = dataset$clone(deep = TRUE),
        "aaa3" = dataset$clone(deep = TRUE)
      ),
      "bbb" = dataset$clone(deep = TRUE)
    ),
    "ccc" = dataset$clone(deep = TRUE)
  )
}

#' Get example modules.
#'
#' Creates an example hierarchy of `teal_modules` from which a `teal` app can be created.
#'
#' @return `teal_modules`
#' @keywords internal
example_modules <- function() {
  mods <- modules(
    label = "d1",
    modules(
      label = "d2",
      modules(
        label = "d3",
        module(label = "aaa1"), module(label = "aaa2"), module(label = "aaa3")
      ),
      module(label = "bbb")
    ),
    module(label = "ccc")
  )
  return(mods)

  # modules(
  #   label = "root",
  #   module(label = "d1"),
  #   module(label = "d2")
  # )
}
