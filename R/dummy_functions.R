# These functions return dummy values to provide good values for the examples of other non-exported functions.
# They should not be exported.

#' Get dummy filter states to apply initially
#'
#' This can be used for the argument `filter` in [`srv_teal`].
#'
#' @param data (`TealData`)
#' @return dummy filter states
get_dummy_filter <- function(data) { # nousage # nolint
  ADSL <- get_raw_data(x = data, dataname = "ADSL") # nolint
  ADLB <- get_raw_data(x = data, dataname = "ADLB") # nolint

  res <- list(
    ADSL = list(
      filter = list(
        list(
          SEX = init_filter_state(
            x = ADSL$SEX,
            varname = "SEX",
            varlabel = "Sex"
          ),
          AGE = init_filter_state(
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
          ASEQ = init_filter_state(
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

#' Get dummy CDISC data
#'
#' Get dummy CDISC data including `ADSL`, `ADAE` and `ADLB`.
#' Some NAs are also introduced to stress test.
#'
#' @return `cdisc_data`
get_dummy_cdisc_data <- function() { # nousage # nolint
  teal_with_pkg("scda", code = {
    ADSL <- scda::synthetic_cdisc_data("latest")$adsl # nolint
    ADAE <- scda::synthetic_cdisc_data("latest")$adae # nolint
    ADLB <- scda::synthetic_cdisc_data("latest")$adlb # nolint
  })

  ADSL$logical_test <- sample(c(TRUE, FALSE, NA), size = nrow(ADSL), replace = TRUE) # nolint
  ADSL$SEX[1:150] <- NA # nolint

  res <- cdisc_data(
    cdisc_dataset(dataname = "ADSL", x = ADSL),
    cdisc_dataset(dataname = "ADAE", x = ADAE),
    cdisc_dataset(dataname = "ADLB", x = ADLB),
    code = "
      ADSL <- synthetic_cdisc_data(\"latest\")$adsl
      ADAE <- synthetic_cdisc_data(\"latest\")$adae
      ADLB <- synthetic_cdisc_data(\"latest\")$adlb
    "
  )
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
#' @return `teal_modules`
#'
get_dummy_modules <- function() { # nousage
  create_mod <- function(module_name) {
    module(
      module_name,
      server = function(input, output, session, datasets) {
      },
      ui = function(id, ...) {
        tags$p(paste0("id: ", id))
      },
      filters = "all"
    )
  }
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
