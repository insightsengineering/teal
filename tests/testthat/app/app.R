# nolint start # because app$setInput etc. are not well chosen names

#---------------------------------------------------------------------------------------
#------------------ Module Code --------------------------------------------------------
#---------------------------------------------------------------------------------------
source("../helper-functions.R")
eval(shinytest_load_pckg("teal"))

# Made Up Plot module
tm_plot_xy <- function(label = "Regression Analysis",
                       response,
                       regressor,
                       facetting = NULL,
                       pre_output = NULL,
                       post_output = NULL) {
  args <- as.list(environment())

  module(
    label = label,
    server = srv_plot_xy,
    ui = ui_plot_xy,
    ui_args = args,
    server_args = list(regressor = regressor, response = response, facetting = facetting),
    filters = "all"
  )
}

ui_plot_xy <- function(id, ...) {
  arguments <- list(...)

  ns <- NS(id)

  # layout
  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      tags$div(
        teal.widgets::plot_with_settings_ui(id = ns("outplot")),
        textOutput(ns("test_outtext")),
        textOutput(ns("test_outtext_response"))
      )
    ),
    encoding = div(
      teal.transform::data_extract_ui(
        id = ns("regressor"),
        label = "Regressor Variable",
        data_extract_spec = arguments$regressor
      ),
      teal.transform::data_extract_ui(
        id = ns("response"),
        label = "Response Variable",
        data_extract_spec = arguments$response
      ),
      teal.transform::data_extract_ui(
        id = ns("facetting"),
        label = "Facetting Variable",
        data_extract_spec = arguments$facetting
      )
    )
  )
}

srv_plot_xy <- function(id, response, regressor, facetting) {
  checkmate::assert_list(response)
  checkmate::assert_list(regressor)
  checkmate::assert_list(facetting, null.ok = TRUE)

  moduleServer(id, function(input, output, session) {
    # data_extract_srv, "response",
    # dataname AND filtering (yes/no) AND Names(Filtering-selected) AND Names(Columns-Selected)
    merged_data <- teal.transform::data_merge_module(
      datasets = datasets,
      data_extract = list(regressor = regressor, response = response, facetting = facetting)
    )

    plot_r <- reactive({
      # qplot is not a proper ggplot function
      x <- merged_data()

      ggplot2::ggplot(
        data = x$data(),
        ggplot2::aes_string(
          x = x$columns_source$regressor,
          y = x$columns_source$response
        )
      ) +
        ggplot2::geom_point()
    })

    teal.widgets::plot_with_settings_srv(id = "outplot", plot_r = plot_r)

    output$test_outtext <- renderText({
      merged_data()$columns_source$regressor
    })
    output$test_outtext_response <- renderText({
      merged_data()$columns_source$response
    })
  })
}

#---------------------------------------------------------------------------------------
#------------------ App Code -----------------------------------------------------------
#---------------------------------------------------------------------------------------
library(scda)
library(teal)
library(magrittr)

ADSL <- synthetic_cdisc_data("rcd_2021_05_05")$adsl
ADTTE <- synthetic_cdisc_data("rcd_2021_05_05")$adtte

adtte_filter_spec <- filter_spec(
  vars = "PARAMCD", # only key variables are allowed
  choices = value_choices(ADTTE, var_choices = "PARAMCD", var_label = "PARAM"),
  selected = "OS",
  multiple = TRUE, # if multiple, then a pivot_wider is needed
  label = "Choose endpoint",
  sep = " - "
)
adtte_filter_spec2 <- filter_spec(
  vars = "CNSR", # only key variables are allowed
  choices = value_choices(ADTTE, var_choices = "CNSR", var_label = "CNSR"),
  selected = "1",
  multiple = FALSE, # if multiple, then a pivot_wider is needed
  label = "Choose CNSR",
  sep = " - "
)


adtte_extracted1 <- data_extract_spec(
  dataname = "ADTTE",
  select = select_spec(
    choices = variable_choices(ADTTE, subset = c("AVAL", "AVALU", "BMRKR1", "SITEID")),
    selected = "AVAL",
    multiple = FALSE,
    fixed = FALSE, # Whether the user can select the item (optional)
    label = "Column" # Label the column select dropdown (optional)
  ),
  filter = list(adtte_filter_spec, adtte_filter_spec2)
)

adtte_extracted <- data_extract_spec(
  dataname = "ADTTE",
  select = select_spec(
    choices = variable_choices(ADTTE, subset = c("AVAL", "BMRKR1")),
    selected = "AVAL",
    multiple = FALSE,
    fixed = FALSE, # Whether the user can select the item
    label = "" # Label the column select dropdown (optional)
  ),
  filter = adtte_filter_spec
)

adsl_extracted <- data_extract_spec(
  dataname = "ADSL",
  select = select_spec(
    choices = variable_choices(ADSL, subset = c("SEX", "AGE")),
    selected = "AGE",
    multiple = FALSE,
    fixed = FALSE
  )
)

x <- init(
  data = cdisc_data(cdisc_dataset("ADSL", ADSL), cdisc_dataset("ADTTE", ADTTE), code = "", check = FALSE),
  modules = modules(
    tm_plot_xy(
      label = "Qplot",
      response = adtte_extracted,
      regressor = list(
        adtte_extracted1,
        adsl_extracted
      ),
      facetting = adsl_extracted
    )
  )
)

shinyApp(x$ui, x$server)

# nolint end
