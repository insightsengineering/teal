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

  res <- teal.data::cdisc_data(
    ADSL = ADSL,
    ADTTE = ADTTE,
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
#' Each list leaf is the same `FilteredData` object.
#'
#' @return named list of `FilteredData` objects, each with `ADSL` set.
#' @keywords internal
example_datasets <- function() { # nolint
  dummy_cdisc_data <- example_cdisc_data()
  datasets <- teal_data_to_filtered_data(dummy_cdisc_data)
  list(
    "d2" = list(
      "d3" = list(
        "aaa1" = datasets,
        "aaa2" = datasets,
        "aaa3" = datasets
      ),
      "bbb" = datasets
    ),
    "ccc" = datasets
  )
}

#' An example `teal` module
#'
#' @description `r lifecycle::badge("experimental")`
#' @inheritParams module
#' @return A `teal` module which can be included in the `modules` argument to [teal::init()].
#' @examples
#' app <- init(
#'   data = teal_data(
#'     IRIS = iris,
#'     MTCARS = mtcars
#'   ),
#'   modules = example_module()
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#' @export
example_module <- function(label = "example teal module", datanames = "all") {
  checkmate::assert_string(label)
  module(
    label,
    server = function(id, data) {
      checkmate::assert_class(data(), "teal_data")
      moduleServer(id, function(input, output, session) {
        ns <- session$ns
        output$datanames <- renderUI({
          selectInput(ns("dataname"), "Choose a dataset", choices = teal.data::datanames(data()))
        })
        output$text <- renderPrint({
          req(input$dataname)
          data()[[input$dataname]]
        })
        teal.widgets::verbatim_popup_srv(
          id = "rcode",
          verbatim_content = reactive(teal.code::get_code(data())),
          title = "Association Plot"
        )
      })
    },
    ui = function(id) {
      ns <- NS(id)
      teal.widgets::standard_layout(
        output = verbatimTextOutput(ns("text")),
        encoding = div(
          uiOutput(ns("datanames")),
          teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
        )
      )
    },
    datanames = datanames
  )
}


#' Get example modules.
#'
#' Creates an example hierarchy of `teal_modules` from which a `teal` app can be created.
#' @param datanames (`character`)\cr
#'  names of the datasets to be used in the example modules. Possible choices are `ADSL`, `ADTTE`.
#' @return `teal_modules`
#' @keywords internal
example_modules <- function(datanames = c("ADSL", "ADTTE")) {
  checkmate::assert_subset(datanames, c("ADSL", "ADTTE"))
  mods <- modules(
    label = "d1",
    modules(
      label = "d2",
      modules(
        label = "d3",
        example_module(label = "aaa1", datanames = datanames),
        example_module(label = "aaa2", datanames = datanames),
        example_module(label = "aaa3", datanames = datanames)
      ),
      example_module(label = "bbb", datanames = datanames)
    ),
    example_module(label = "ccc", datanames = datanames)
  )
  return(mods)
}
